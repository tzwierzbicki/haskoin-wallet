{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A wallet and its labels as the records of a BIP 329 file: what
-- exportwallet writes for the portfolios of a wallet, and what
-- importwallet makes of a file. A file creates whole the wallets and the
-- portfolios the database does not have, and leaves the others as they
-- are. It is refused when it would create a portfolio without all of its
-- accounts, a wallet without its portfolio0, or when its keys are not
-- those of the database. It may be narrowed to one wallet, by its
-- fingerprint, and to one portfolio. Then what importwallet makes of each
-- kind of record, and how show shows a file. Every test has a database of
-- its own.
module Haskoin.Wallet.LabelsSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Aeson as Json
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address (genExtAddress, updateAddressBalances)
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.BestBlock (updateBest)
import Haskoin.Wallet.Database.Contact (addContact, contactAt, tagContact)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag (TagSettings (..), accountAddressTags, lockingTags, setTagColor, setTagLocksCoins, tagAddress, tagSettings, untagAddress)
import Haskoin.Wallet.Database.TxInfo (repsertSummary)
import Haskoin.Wallet.Database.TxLabel (setTxLabel, txLabel)
import Haskoin.Wallet.Keys (AccountKind (..), PublicSeed (..), seedPortfolioKeys)
import Haskoin.Wallet.Labels
import Haskoin.Wallet.Tags (ShownTag (..), TagColor (..), colorName, readTagColor, tagText)
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = do
  exportSpec
  importKeySpec
  mergeSpec
  refusalSpec
  scopeSpec
  walletScopeSpec
  activeSpec
  foreignSpec
  roundTripSpec
  contactRecordSpec
  unusedTagSpec
  importAddressSpec
  importTxSpec
  importCoinSpec
  importOtherSpec
  wordLineSpec
  showFileSpec
  reportJsonSpec

{- Fixtures -}

-- | The id of the payment of the shared story, as the records write it:
-- fakeTxHash 1, whose bytes read backwards
paymentTxid :: Text
paymentTxid = "0000000000000000000000000000000000000000000000000000000000000001"

-- | The origins of the two accounts of portfolio 0 of the test wallet on
-- Bitcoin
mainOrigin, segwitOrigin :: Text
mainOrigin = "pkh([bc560f95/44'/0'/0'])"
segwitOrigin = "wpkh([bc560f95/84'/0'/0'])"

-- | The record of the key of an account as exportwallet writes it for a
-- portfolio without a description: the key, its origin, its network and
-- its purpose, on a line of its own
keyLine :: Text -> Text -> Text -> Text -> Text
keyLine key origin network purpose =
  "{\"type\":\"xpub\",\"ref\":\"" <> key <> "\",\"origin\":\"" <> origin <> "\",\"network\":\"" <> network <> "\",\"purpose\":" <> purpose <> "}\n"

-- | The key of the bip44 account of portfolio 0 of the test wallet on
-- Bitcoin, and that of its bip84 account there
mainKeyLine, segwitKeyLine :: Text
mainKeyLine = keyLine testAccountXPub mainOrigin "btc" "44"
segwitKeyLine = keyLine testSegwitAccountXPub segwitOrigin "btc" "84"

-- | The key of the bip44 account of portfolio 0 of the test wallet on
-- Bitcoin, as exportwallet writes it for a portfolio described "savings"
-- in a wallet described "steel backup"
describedKeyLine :: Text
describedKeyLine =
  "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"savings\",\"origin\":\"" <> mainOrigin <> "\",\"network\":\"btc\",\"purpose\":44,\"wallet\":\"steel backup\"}\n"

-- | The key of the bip44 account of portfolio 0 of the test wallet on
-- Bitcoin, as exportwallet writes it first in a file of a database with
-- three tags that nothing has: kyc is red, cold locks its coins, and rent
-- has neither a colour nor a lock
taggedKeyLine :: Text
taggedKeyLine =
  "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"" <> mainOrigin <> "\",\"network\":\"btc\",\"purpose\":44,\"tags\":[\"cold\",\"kyc\",\"rent\"],\"tagcolors\":{\"kyc\":\"red\"},\"taglocks\":[\"cold\"]}\n"

-- | The keys of the two accounts of portfolio 1 of the test wallet on
-- Bitcoin, m/44'/0'/1' and m/84'/0'/1'
secondKeyLine, secondSegwitKeyLine :: Text
secondKeyLine = keyLine testAccountXPub1 "pkh([bc560f95/44'/0'/1'])" "btc" "44"
secondSegwitKeyLine = keyLine testSegwitAccountXPub1 "wpkh([bc560f95/84'/0'/1'])" "btc" "84"

-- | The file exportwallet writes for portfolio 0 of the test wallet, with
-- no description and no label: the keys of its twelve accounts, from
-- five keys, since the test networks share theirs
testWalletFile :: Text
testWalletFile =
  Text.concat
    [ mainKeyLine,
      segwitKeyLine,
      keyLine testAccountXPubBch "pkh([bc560f95/44'/145'/0'])" "bch" "44",
      keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest" "44",
      keyLine testSegwitAccountXPubTest "wpkh([bc560f95/84'/1'/0'])" "btctest" "84",
      keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest4" "44",
      keyLine testSegwitAccountXPubTest "wpkh([bc560f95/84'/1'/0'])" "btctest4" "84",
      keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btcreg" "44",
      keyLine testSegwitAccountXPubTest "wpkh([bc560f95/84'/1'/0'])" "btcreg" "84",
      keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "bchtest" "44",
      keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "bchtest4" "44",
      keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "bchreg" "44"
    ]

-- | The file exportwallet writes for a portfolio of a seed, at its index,
-- with no description and no label, as 'testWalletFile' is for portfolio
-- 0 of the test wallet: the key of each of its twelve accounts, with its
-- origin, its network and its purpose. The keys come from the seed, as
-- the input of a test.
portfolioFileOf :: PublicSeed -> Natural -> IO Text
portfolioFileOf seed index = do
  keys <- maybe (fail "portfolioFileOf: the seed has no keys at this index") return =<< seedPortfolioKeys seed index
  return $ Text.concat [keyLine (xPubExport net key) (origin net kind) (cs net.name) (purpose kind) | (net, kind, key) <- keys]
  where
    purpose Bip44 = "44"
    purpose Bip84 = "84"
    scripts Bip44 = "pkh"
    scripts Bip84 = "wpkh"
    origin :: Network -> AccountKind -> Text
    origin net kind =
      scripts kind <> "([" <> fingerprintToText (seedFingerprint seed) <> "/" <> purpose kind <> "'/" <> cs (show net.bip44Coin) <> "'/" <> cs (show index) <> "'])"

-- | The file of portfolio 1 of the test wallet, whole
secondPortfolioFile :: IO Text
secondPortfolioFile = portfolioFileOf testSeed 1

-- | The file of portfolio 0 of the second test wallet, 4435aa1c, whole
otherWalletFile :: IO Text
otherWalletFile = portfolioFileOf testSeed2 0

-- | The key of the first account of the test wallet, as an account left
-- aside names it: its first twelve characters
mainKeyStart :: Text
mainKeyStart = "xpub6CHK45s4..."

-- | How an answer names an account: the index of its wallet, that of its
-- portfolio, its network and its type
accountAt :: Natural -> Natural -> Network -> AccountKind -> AccountRef
accountAt wallet portfolio net = AccountRef (Place (PortfolioRef wallet portfolio) net)

-- | The twelve accounts of a whole portfolio as the answers name them, in
-- the order of the keys of 'testWalletFile'
twelveAccounts :: Natural -> Natural -> [AccountRef]
twelveAccounts wallet portfolio =
  [ accountAt wallet portfolio btc Bip44,
    accountAt wallet portfolio btc Bip84,
    accountAt wallet portfolio bch Bip44,
    accountAt wallet portfolio btcTest Bip44,
    accountAt wallet portfolio btcTest Bip84,
    accountAt wallet portfolio btcTest4 Bip44,
    accountAt wallet portfolio btcTest4 Bip84,
    accountAt wallet portfolio btcRegTest Bip44,
    accountAt wallet portfolio btcRegTest Bip84,
    accountAt wallet portfolio bchTest Bip44,
    accountAt wallet portfolio bchTest4 Bip44,
    accountAt wallet portfolio bchRegTest Bip44
  ]

-- | The twelve accounts of a whole portfolio as 'accountsStored' reads
-- them
twelveStored :: Natural -> Natural -> [(Natural, Natural, Text, Int)]
twelveStored wallet portfolio =
  [ (wallet, portfolio, "btc", 44),
    (wallet, portfolio, "btc", 84),
    (wallet, portfolio, "bch", 44),
    (wallet, portfolio, "btctest", 44),
    (wallet, portfolio, "btctest", 84),
    (wallet, portfolio, "btctest4", 44),
    (wallet, portfolio, "btctest4", 84),
    (wallet, portfolio, "btcreg", 44),
    (wallet, portfolio, "btcreg", 84),
    (wallet, portfolio, "bchtest", 44),
    (wallet, portfolio, "bchtest4", 44),
    (wallet, portfolio, "bchreg", 44)
  ]

-- | The payment of 50000 stored in the pool, in block 800000, and the
-- best block of Bitcoin at the height given
paymentStored :: Pool -> BlockHeight -> DB IO ()
paymentStored pool best = do
  _ <- repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
  updateBest btc (fakeBlockHash 2) best 1700000000

-- | The outpoint of the coin of the payment
paymentCoin :: OutPoint
paymentCoin = OutPoint (fakeTxHash 1) 0

-- | The coin of the payment stored in the account, at the address given,
-- locked by the user or not
coinStoredAt :: Text -> DBAccountId -> Bool -> DB IO ()
coinStoredAt addr accId locked =
  P.insert_ $ DBCoin accId (outpointBytes paymentCoin) addr 50000 (S.encode (Store.BlockRef 800000 1)) seenTime locked fixtureTime

-- | The coin of the payment stored in the account, at the first receiving
-- address of the test wallet
coinStored :: DBAccountId -> Bool -> DB IO ()
coinStored = coinStoredAt (externalAddrsText !! 0)

-- | Whether the user locked the coin of the payment in the account
coinLocked :: DBAccountId -> DB IO (Maybe Bool)
coinLocked accId = fmap dBCoinLockedByUser <$> P.get (DBCoinKey accId (outpointBytes paymentCoin))

-- | The bip44 account of portfolio 0 of the test wallet on Bitcoin, with
-- its first receiving address. The portfolio is whole.
mainWithAddress :: ExceptT String (DB IO) (DBAccountId, DBAccount)
mainWithAddress = do
  held@(accId, _) <- testNewAcc
  _ <- genExtAddress def accId ""
  return held

-- | The wallets of the database: the index, the fingerprint and the
-- description of each
walletsStored :: DB IO [(Natural, Text, Text)]
walletsStored = map (\(_, wallet) -> (walletIndex wallet, dBWalletFingerprint wallet, dBWalletDescription wallet)) <$> getWallets

-- | The portfolios of the database: the index of the wallet of each, its
-- own index and its description
portfoliosStored :: DB IO [(Natural, Natural, Text)]
portfoliosStored =
  map (\portfolio -> (refWallet (portfolioRef portfolio), refPortfolio (portfolioRef portfolio), dBPortfolioDescription portfolio.portfolioRow)) <$> getAllPortfolios

-- | The accounts of the database: the index of the wallet of each, that
-- of its portfolio, its network and its purpose, in the order hw lists
-- them
accountsStored :: DB IO [(Natural, Natural, Text, Int)]
accountsStored =
  map (\(portfolio, _, acc) -> (refWallet (portfolioRef portfolio), refPortfolio (portfolioRef portfolio), dBAccountNetwork acc, dBAccountPurpose acc)) <$> getAccounts

-- | The keys of the accounts of the database on Bitcoin, in the order hw
-- lists them
bitcoinKeysStored :: DB IO [Text]
bitcoinKeysStored = map (\(_, _, acc) -> dBAccountXPubKey acc) . filter (\(_, _, acc) -> dBAccountNetwork acc == "btc") <$> getAccounts

-- | The labels of the receiving addresses of an account, by index
addressLabels :: DBAccountId -> DB IO [(Int, Text)]
addressLabels accId = do
  rows <- P.selectList [DBAddressAccount P.==. accId, DBAddressInternal P.==. False] [P.Asc DBAddressIndex]
  return [(dBAddressIndex a, dBAddressLabel a) | P.Entity _ a <- rows]

-- | A report of an import that changed nothing
nothingDone :: ImportReport
nothingDone = ImportReport [] [] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 []

-- | What a file of one key that is left aside reports: the start of the
-- key and the reason
leftAside :: Text -> ExceptT String (DB IO) [AccountImport]
leftAside content = reportAccounts <$> importFile wholeFile content

{- Exporting -}

exportSpec :: Spec
exportSpec =
  describe "The records of a wallet" $ do
    it "are the key of each of the twelve accounts of a portfolio, with its origin, its network and its purpose" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        records <- walletRecords [portfolio]
        liftIO $ recordsText records `shouldBe` testWalletFile

    it "give each key the description of its portfolio as its label, and that of its wallet under the key wallet" $
      inDatabaseE $ do
        created <- testPortfolio
        setPortfolioDescription created.portfolioId "savings"
        setWalletDescription created.portfolioWalletId "steel backup"
        portfolio <- getPortfolio created.portfolioId
        records <- walletRecords [portfolio]
        liftIO $ do
          map recordLine (take 1 records)
            `shouldBe` ["{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"savings\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44,\"wallet\":\"steel backup\"}"]
          map (\r -> (recordLabel r, recordWallet r)) records `shouldBe` replicate 12 (Just "savings", Just "steel backup")

    -- The payment has 6 confirmations at best block 800005. The bip44
    -- account on Bitcoin is the first of the portfolio.
    it "follow the key of an account with its labelled addresses, the labelled transactions of its portfolio on its network and its locked coins, each with the origin and the network of the account" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- testNewAcc
        _ <- genExtAddress def accId "Salary"
        lift $ setTxLabel (portfolioPool portfolio btc) (fakeTxHash 1) "First salary"
        lift $ paymentStored (portfolioPool portfolio btc) 800005
        lift $ coinStored accId True
        records <- walletRecords [portfolio]
        liftIO $
          map recordLine (take 4 records)
            `shouldBe` [ "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44}",
                         "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/0\"}",
                         "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"First salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"height\":800000,\"fee\":1000,\"value\":50000}",
                         "{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"spendable\":false,\"keypath\":\"/0/0\",\"height\":800000,\"value\":50000}"
                       ]

    -- A transaction is of the portfolio on a network, which has two
    -- accounts on Bitcoin
    it "write a labelled transaction once, with the first account of its network, though the portfolio has two accounts there" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        lift $ setTxLabel (portfolioPool portfolio btc) (fakeTxHash 1) "First salary"
        records <- walletRecords [portfolio]
        liftIO $
          [recordOrigin r | r <- records, recordType r == TxRecord] `shouldBe` [Just mainOrigin]

    -- The second receiving address of the bip84 account
    it "give a labelled address of the bip84 account the origin of that account" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- testNewSegwitAcc
        _ <- genExtAddress def accId ""
        _ <- genExtAddress def accId "Tips"
        records <- walletRecords [portfolio]
        liftIO $
          [recordLine r | r <- records, recordType r == AddrRecord]
            `shouldBe` ["{\"type\":\"addr\",\"ref\":\"bc1qqdensnp26rdechmq669tjk4s2wsyjr04nw3mc2\",\"label\":\"Tips\",\"origin\":\"wpkh([bc560f95/84'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/1\"}"]

    -- btctest and btctest4 have the same key, so the same origin and the
    -- same addresses: the network of the record tells them apart
    it "tell the labels of two test networks that share their keys apart by the network of each record" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- accountOf portfolio btcTest4 Bip44
        _ <- genExtAddress def accId "Faucet"
        records <- walletRecords [portfolio]
        liftIO $
          [recordLine r | r <- records, recordType r == AddrRecord]
            `shouldBe` ["{\"type\":\"addr\",\"ref\":\"mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih\",\"label\":\"Faucet\",\"origin\":\"pkh([bc560f95/44'/1'/0'])\",\"network\":\"btctest4\",\"keypath\":\"/0/0\"}"]

    -- 3 confirmations at best block 800002: the height may still change
    it "give a transaction and a coin no height before six blocks confirm them" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- mainWithAddress
        lift $ setTxLabel (portfolioPool portfolio btc) (fakeTxHash 1) "First salary"
        lift $ paymentStored (portfolioPool portfolio btc) 800002
        lift $ coinStored accId True
        records <- walletRecords [portfolio]
        liftIO $ map recordHeight (take 2 (drop 1 records)) `shouldBe` [Nothing, Nothing]

    it "give a labelled transaction the chain does not have yet neither height, fee nor value" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        lift $ setTxLabel (portfolioPool portfolio btc) (fakeTxHash 1) "First salary"
        records <- walletRecords [portfolio]
        liftIO $
          take 1 (drop 1 records)
            `shouldBe` [(record TxRecord paymentTxid) {recordLabel = Just "First salary", recordOrigin = Just mainOrigin, recordNetwork = Just "btc"}]

    it "leave out an address without a label and a coin that is free" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- mainWithAddress
        lift $ coinStored accId False
        records <- walletRecords [portfolio]
        liftIO $ map recordType records `shouldBe` replicate 12 XpubRecord

    -- What exportwallet --portfolio 1 writes
    it "are those of the one portfolio given, and of no other of its wallet" $
      inDatabaseE $ do
        _ <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        records <- walletRecords [second]
        liftIO $
          map recordOrigin records
            `shouldBe` [ Just "pkh([bc560f95/44'/0'/1'])",
                         Just "wpkh([bc560f95/84'/0'/1'])",
                         Just "pkh([bc560f95/44'/145'/1'])",
                         Just "pkh([bc560f95/44'/1'/1'])",
                         Just "wpkh([bc560f95/84'/1'/1'])",
                         Just "pkh([bc560f95/44'/1'/1'])",
                         Just "wpkh([bc560f95/84'/1'/1'])",
                         Just "pkh([bc560f95/44'/1'/1'])",
                         Just "wpkh([bc560f95/84'/1'/1'])",
                         Just "pkh([bc560f95/44'/1'/1'])",
                         Just "pkh([bc560f95/44'/1'/1'])",
                         Just "pkh([bc560f95/44'/1'/1'])"
                       ]

    it "are those of every portfolio given, in the order given" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        records <- walletRecords [first, second]
        liftIO $
          [recordRef r | r <- records, recordNetwork r == Just "btc"]
            `shouldBe` [testAccountXPub, testSegwitAccountXPub, testAccountXPub1, testSegwitAccountXPub1]

{- Importing keys -}

importKeySpec :: Spec
importKeySpec =
  describe "An xpub record" $ do
    -- The wallet has portfolio 0, whole
    it "is of the account of its network and type, in the portfolio of its index, of the wallet of its fingerprint" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile mainKeyLine
        liftIO $ do
          reportWallets report `shouldBe` [WalletImport 0 testFingerprint False [PortfolioImport 0 False]]
          reportAccounts report `shouldBe` [AccountHeld (accountAt 0 0 btc Bip44)]

    it "gives each account of a portfolio the import creates the key of the file" $
      inDatabaseE $ do
        _ <- importFile wholeFile testWalletFile
        lift bitcoinKeysStored `dbShouldBeE` [testAccountXPub, testSegwitAccountXPub]

    -- The key at m/44'/0'/1'. The wallet has portfolios 0 and 1.
    it "goes to the portfolio of the last step of its path" $
      inDatabaseE $ do
        _ <- testPortfolio
        _ <- nextPortfolioOf testMnemonic
        report <- importFile wholeFile secondKeyLine
        liftIO $ reportAccounts report `shouldBe` [AccountHeld (accountAt 0 1 btc Bip44)]

    it "is of the bip84 account when its origin is wpkh" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\",\"origin\":\"wpkh([bc560f95/84'/0'/0'])\",\"network\":\"btc\"}\n")
        liftIO $ reportAccounts report `shouldBe` [AccountHeld (accountAt 0 0 btc Bip84)]

    -- The zpub of the bip84 section of the Coldcard file of
    -- test/data/descriptors, which some wallets write for the key of a
    -- bip84 account, in place of the xpub in the file of portfolio 0
    it "gives a bip84 account the key of a zpub of SLIP-132, stored as its xpub" $
      inDatabaseE $ do
        report <- importFile wholeFile (Text.replace testSegwitAccountXPub testSegwitAccountZPub testWalletFile)
        liftIO $ take 2 (reportAccounts report) `shouldBe` [AccountAdded (accountAt 0 0 btc Bip44), AccountAdded (accountAt 0 0 btc Bip84)]
        lift bitcoinKeysStored `dbShouldBeE` [testAccountXPub, testSegwitAccountXPub]

    it "gives a new portfolio its label as its description, and a new wallet the description under the key wallet" $
      inDatabaseE $ do
        _ <- importFile wholeFile (Text.replace mainKeyLine describedKeyLine testWalletFile)
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", "steel backup")]
        lift portfoliosStored `dbShouldBeE` [(0, 0, "savings")]

    it "keeps the descriptions the wallet and the portfolio have against those of the file" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        setPortfolioDescription portfolio.portfolioId "rent"
        setWalletDescription portfolio.portfolioWalletId "paper backup"
        _ <- importFile wholeFile describedKeyLine
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", "paper backup")]
        lift portfoliosStored `dbShouldBeE` [(0, 0, "rent")]

    it "counts the labels a new wallet and a new portfolio take from the file among the labels set" $
      inDatabaseE $ do
        report <- importFile wholeFile (Text.replace mainKeyLine describedKeyLine testWalletFile)
        liftIO $ (reportWalletLabels report, reportPortfolioLabels report) `shouldBe` (1, 1)

    -- exportwallet writes the labels on each of the twelve keys
    it "counts the labels of a wallet and a portfolio once, though each of the twelve keys of the portfolio carries them" $
      inDatabaseE $ do
        let labelled = Text.replace ",\"origin\"" ",\"label\":\"savings\",\"origin\"" (Text.replace "}\n" ",\"wallet\":\"steel backup\"}\n" testWalletFile)
        report <- importFile wholeFile labelled
        liftIO $ (reportWalletLabels report, reportPortfolioLabels report) `shouldBe` (1, 1)

    -- As when the same file is imported again
    it "counts the labels of a wallet and a portfolio that had them already among the labels set" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        setPortfolioDescription portfolio.portfolioId "savings"
        setWalletDescription portfolio.portfolioWalletId "steel backup"
        report <- importFile wholeFile describedKeyLine
        liftIO $ (reportWalletLabels report, reportPortfolioLabels report) `shouldBe` (1, 1)

    it "counts the labels of the file that the wallet and the portfolio keep their own descriptions against as kept, and not as set" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        setPortfolioDescription portfolio.portfolioId "rent"
        setWalletDescription portfolio.portfolioWalletId "paper backup"
        report <- importFile wholeFile describedKeyLine
        liftIO $
          (reportWalletLabels report, reportPortfolioLabels report, reportWalletLabelsKept report, reportPortfolioLabelsKept report)
            `shouldBe` (0, 0, 1, 1)

    it "counts no label of a wallet or a portfolio when the file gives none" $
      inDatabaseE $ do
        report <- importFile wholeFile testWalletFile
        liftIO $
          (reportWalletLabels report, reportPortfolioLabels report, reportWalletLabelsKept report, reportPortfolioLabelsKept report)
            `shouldBe` (0, 0, 0, 0)

    it "is taken once when the file has it twice" $
      inDatabaseE $ do
        report <- importFile wholeFile (testWalletFile <> mainKeyLine)
        liftIO $ reportAccounts report `shouldBe` map AccountAdded (twelveAccounts 0 0)

    it "is left aside for a key that does not read on its network" $
      inDatabaseE $
        leftAside (keyLine testAccountXPubTest mainOrigin "btc" "44")
          `dbShouldBeE` [AccountLeftAside "tpubDCEeDpHv..." "The key does not read as a key of btc"]

    it "is left aside for an unknown network" $
      inDatabaseE $
        leftAside (keyLine testAccountXPub mainOrigin "nowhere" "44")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "Unknown network nowhere"]

    -- The master key of the vectors of the BIP, at depth 0
    it "is left aside for a key that is not three hardened steps below the master key" $
      inDatabaseE $
        leftAside "{\"type\":\"xpub\",\"ref\":\"xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8\",\"label\":\"Extended Public Key\",\"network\":\"btc\",\"purpose\":44}\n"
          `dbShouldBeE` [AccountLeftAside "xpub661MyMwA..." "The key is not that of an account, three hardened steps below the master key"]

    -- The key of the test account at /0', four hardened steps deep
    it "is left aside for a key a hardened step further than an account" $
      inDatabaseE $ do
        let deeper = xPubExport btc (deriveXPubKey (hardSubKey testAccountKey 0))
        found <- leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> deeper <> "\",\"network\":\"btc\",\"purpose\":44}\n")
        liftIO $ found `shouldBe` [AccountLeftAside (Text.take 12 deeper <> "...") "The key is not that of an account, three hardened steps below the master key"]

    -- The key of the master key of the shares of the tests at
    -- m/44'/0'/0, three steps deep, the last one not hardened
    it "is left aside for a key three steps deep whose last step is not hardened" $
      inDatabaseE $ do
        let master = fromMaybe (error "master") (xPrvImport btc testSharesMasterKey)
            soft = xPubExport btc (deriveXPubKey (derivePath (Deriv :| 44 :| 0 :/ 0 :: DerivPath) master))
        found <- leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> soft <> "\",\"network\":\"btc\",\"purpose\":44}\n")
        liftIO $ found `shouldBe` [AccountLeftAside (Text.take 12 soft <> "...") "The key is not that of an account, three hardened steps below the master key"]

    -- The path is that of the account; the script functions, those of a
    -- BIP49 wallet, are not
    it "is left aside when the script functions of its origin are not those of the account, at its very path" $
      inDatabaseE $
        leftAside (keyLine testAccountXPub "sh(wpkh([bc560f95/44'/0'/0']))" "btc" "44")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The key is not that of the bip44 account /44'/0'/0': its origin is sh(wpkh([bc560f95/44'/0'/0']))"]

    it "is left aside when its origin is not that of the account at its path, as the pkh origin of a key at a BIP84 path" $
      inDatabaseE $
        leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"pkh([bc560f95/84'/0'/0'])\",\"network\":\"btc\"}\n")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The key is not that of the bip44 account /44'/0'/0': its origin is pkh([bc560f95/84'/0'/0'])"]

    it "is left aside when its origin is of a kind of account hw does not hold, as sh(wpkh) of a BIP49 wallet" $
      inDatabaseE $
        leftAside (keyLine testAccountXPub "sh(wpkh([bc560f95/49'/0'/0']))" "btc" "44")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The key is not that of the bip44 account /44'/0'/0': its origin is sh(wpkh([bc560f95/49'/0'/0']))"]

    -- The coin type of Bitcoin Cash is 145: the key of an account of
    -- Bitcoin is of no account there
    it "is left aside when its origin is the path of an account of another network" $
      inDatabaseE $
        leftAside (keyLine testAccountXPub mainOrigin "bch" "44")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The key is not that of the bip44 account /44'/145'/0': its origin is pkh([bc560f95/44'/0'/0'])"]

    it "is left aside when its origin and its purpose name two types of account" $
      inDatabaseE $
        leftAside (keyLine testAccountXPub mainOrigin "btc" "84")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The record says its key is of accounts of two types, bip44 and bip84: its origin, its purpose and the prefix of its key do not agree"]

    it "is left aside when its purpose is of no type hw holds" $
      inDatabaseE $
        leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\",\"purpose\":49}\n")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The purpose 49 is of no type of account hw holds: bip44 or bip84"]

    it "is left aside as a bip84 account on Bitcoin Cash, which has no SegWit" $
      inDatabaseE $
        leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPubBch <> "\",\"network\":\"bch\",\"purpose\":84}\n")
          `dbShouldBeE` [AccountLeftAside "xpub6DXWb36C..." "A bip84 account needs SegWit, which bch does not have"]

    it "is left aside with a private key" $
      inDatabaseE $
        leftAside (keyLine testAccountXPrv mainOrigin "btc" "44")
          `dbShouldBeE` [AccountLeftAside "xprv9yHxeaLA..." "The key is a private key. hw takes the public key of an account, and no file of a wallet should hold a private one"]

    it "is left aside with a ypub of SLIP-132, which marks a BIP49 account" $
      inDatabaseE $
        leftAside "{\"type\":\"xpub\",\"ref\":\"ypub6YGZhSg8Nso498vduxTVMtR3EtZUu8H3bKfUmJDp5x1QXjy9DdACFQ8VwcgtS6NSgzCMGs5mUSCP2iG3BtXCRBP8Zh7V74rwL9qRRbN1yGV\",\"network\":\"btc\"}\n"
          `dbShouldBeE` [AccountLeftAside "ypub6YGZhSg8..." "The key has the prefix ypub of SLIP-132, which marks a BIP49 account, with P2SH-P2WPKH addresses: hw holds bip44 and bip84 accounts"]

    -- Nothing names the wallet: no fingerprint, and no account with the key
    it "is left aside without an origin when no account of the database holds its key" $
      inDatabaseE $
        leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> testSegwitAccountXPub <> "\",\"network\":\"btc\",\"purpose\":84}\n")
          `dbShouldBeE` [AccountLeftAside "xpub6CQF8An9..." "The record has no origin, and no account of the database holds its key: nothing says which wallet it is of"]

    it "does not stop the other keys of the file when it is left aside" $
      inDatabaseE $ do
        found <- leftAside (keyLine testAccountXPub mainOrigin "nowhere" "44" <> testWalletFile)
        liftIO $ found `shouldBe` AccountLeftAside mainKeyStart "Unknown network nowhere" : map AccountAdded (twelveAccounts 0 0)

    -- No file of hw writes a key without its origin. The wallet holds
    -- the key, and has the fingerprint bc560f95.
    it "is of the wallet of the account that holds its key, when it has no origin" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\",\"purpose\":44}\n")
        liftIO $ do
          reportAccounts report `shouldBe` [AccountHeld (accountAt 0 0 btc Bip44)]
          reportWallets report `shouldBe` [WalletImport 0 testFingerprint False [PortfolioImport 0 False]]

{- Importing a file into the database -}

mergeSpec :: Spec
mergeSpec =
  describe "A file of a wallet, imported" $ do
    it "creates the wallet and its portfolio with its twelve accounts, in a database that has neither" $
      inDatabaseE $ do
        report <- importFile wholeFile testWalletFile
        liftIO $ do
          reportWallets report `shouldBe` [WalletImport 0 testFingerprint True [PortfolioImport 0 True]]
          reportAccounts report `shouldBe` map AccountAdded (twelveAccounts 0 0)
        lift accountsStored `dbShouldBeE` twelveStored 0 0

    it "never makes a second portfolio with an index the wallet has, nor a second wallet of its fingerprint" $
      inDatabaseE $ do
        _ <- testPortfolio
        _ <- importFile wholeFile testWalletFile
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", "")]
        lift portfoliosStored `dbShouldBeE` [(0, 0, "")]

    it "leaves an account the portfolio had as it is, with its addresses and their labels" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- accountOf portfolio btc Bip44
        _ <- genExtAddress def accId "Salary"
        asItWas <- getAccountById accId
        _ <- importFile wholeFile testWalletFile
        getAccountById accId `dbShouldBeE` asItWas
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    it "adds nothing to a portfolio the wallet has, which holds every account of the file" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile testWalletFile
        liftIO $ do
          reportWallets report `shouldBe` [WalletImport 0 testFingerprint False [PortfolioImport 0 False]]
          reportAccounts report `shouldBe` map AccountHeld (twelveAccounts 0 0)
        lift accountsStored `dbShouldBeE` twelveStored 0 0

    -- The wallet has portfolio 0, and the file portfolio 1, whole
    it "creates a portfolio the wallet does not have, at its index, whole, and leaves the others alone" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO secondPortfolioFile
        report <- importFile wholeFile content
        liftIO $ reportWallets report `shouldBe` [WalletImport 0 testFingerprint False [PortfolioImport 1 True]]
        lift accountsStored `dbShouldBeE` (twelveStored 0 0 <> twelveStored 0 1)

    -- The file has the two accounts of portfolio 1 on Bitcoin, of twelve
    it "refuses a portfolio the wallet does not have when the file lacks some of its accounts" $
      inDatabase $ do
        _ <- runExceptT testPortfolio
        dbShouldFailWith
          "The file gives 2 of the 12 accounts of portfolio1 of wallet0, which the database does not have: \
          \importwallet creates a portfolio only whole, with every account. \
          \Write the file with exportwallet on the computer that has the whole portfolio"
          $ importFile wholeFile (secondKeyLine <> secondSegwitKeyLine)

    it "creates no account of a portfolio it refuses for the accounts it lacks" $
      inDatabaseE $ do
        _ <- testPortfolio
        lift $ dbShouldFail $ importFile wholeFile (secondKeyLine <> secondSegwitKeyLine)
        lift accountsStored `dbShouldBeE` twelveStored 0 0

    -- The database has no wallet. The file has one account of portfolio 0.
    it "refuses the portfolio0 of a new wallet when the file lacks some of its accounts, naming the wallet by its fingerprint" $
      inDatabase $
        dbShouldFailWith
          "The file gives 1 of the 12 accounts of portfolio0 of the wallet bc560f95, which the database does not have: \
          \importwallet creates a portfolio only whole, with every account. \
          \Write the file with exportwallet on the computer that has the whole portfolio"
          $ importFile wholeFile mainKeyLine

    it "creates no wallet when it refuses the portfolio0 of the wallet" $
      inDatabaseE $ do
        lift $ dbShouldFail $ importFile wholeFile mainKeyLine
        lift walletsStored `dbShouldBeE` []

    -- The database has no wallet, and the file portfolio 1 alone, whole
    it "refuses a wallet the database does not have when the file does not bring its portfolio0" $
      inDatabase $ do
        content <- liftIO secondPortfolioFile
        dbShouldFailWith
          "The file does not have portfolio0 of the wallet bc560f95, which the database does not have: \
          \a wallet is created with its portfolio0. \
          \Import a file of the whole wallet, as exportwallet writes it without --portfolio"
          $ importFile wholeFile content

    it "creates nothing when it refuses a wallet for the portfolio0 the file does not bring" $
      inDatabaseE $ do
        content <- liftIO secondPortfolioFile
        lift $ dbShouldFail $ importFile wholeFile content
        lift walletsStored `dbShouldBeE` []
        lift accountsStored `dbShouldBeE` []

    -- The wallet of the test mnemonic is wallet0. The file is of the
    -- second test mnemonic, whose fingerprint is 4435aa1c.
    it "finds its wallet by the fingerprint: a file of another mnemonic makes a wallet of its own" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO otherWalletFile
        report <- importFile wholeFile content
        liftIO $ reportWallets report `shouldBe` [WalletImport 1 testFingerprint2 True [PortfolioImport 0 True]]
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", ""), (1, "4435aa1c", "")]
        lift accountsStored `dbShouldBeE` (twelveStored 0 0 <> twelveStored 1 0)

    it "sorts the keys of two mnemonics into their two wallets" $
      inDatabaseE $ do
        other <- liftIO otherWalletFile
        report <- importFile wholeFile (testWalletFile <> other)
        liftIO $
          reportWallets report
            `shouldBe` [ WalletImport 0 testFingerprint True [PortfolioImport 0 True],
                         WalletImport 1 testFingerprint2 True [PortfolioImport 0 True]
                       ]
        lift accountsStored `dbShouldBeE` (twelveStored 0 0 <> twelveStored 1 0)

{- A file that cannot be imported -}

-- | The key of the second test wallet under the origin of the bip44
-- account of portfolio 0 of the test wallet on Bitcoin: another key than
-- that account has
wrongKeyLine :: Text
wrongKeyLine = keyLine testAccountXPub2 mainOrigin "btc" "44"

refusalSpec :: Spec
refusalSpec =
  describe "A file that cannot be imported into the database" $ do
    it "is refused when an account of the file has another key than the same account of the database, which it names" $
      inDatabase $ do
        _ <- runExceptT testPortfolio
        dbShouldFailWith "The bip44 account of portfolio0 of wallet0 on btc has another key than the file gives it: the file and the database are not of one mnemonic" $
          importFile wholeFile wrongKeyLine

    -- Portfolio 0 is created first, whole, then portfolio 1 is refused:
    -- the file has one account of it
    it "changes nothing when it is refused: no portfolio of the file is created" $
      inDatabaseE $ do
        lift $ dbShouldFail $ importFile wholeFile (testWalletFile <> secondKeyLine)
        lift walletsStored `dbShouldBeE` []
        lift accountsStored `dbShouldBeE` []

    -- The label of the file is for an address the wallet has
    it "sets no label of a file it refuses" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        lift $ dbShouldFail $ importFile wholeFile (wrongKeyLine <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n")
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

    -- The wallet of the test mnemonic, bc560f95, holds the keys. The file
    -- gives them to the seed 4435aa1c, which the database does not have.
    it "is refused when another wallet holds a key of the file" $
      inDatabase $ do
        _ <- runExceptT testPortfolio
        dbShouldFailWith "portfolio0 of wallet0 already has this key on btc" $
          importFile wholeFile (Text.replace "bc560f95" "4435aa1c" testWalletFile)

    -- The wallet holds the key in its bip44 account. The file says, by
    -- its purpose alone, that the key is of the bip84 account.
    it "is refused when it gives a key the wallet holds to an account of another type" $
      inDatabase $ do
        _ <- runExceptT testPortfolio
        dbShouldFailWith "The bip84 account of portfolio0 of wallet0 on btc has another key than the file gives it: the file and the database are not of one mnemonic" $
          importFile wholeFile ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\",\"purpose\":84}\n")

    -- The key of account 0 written with the child number 5' and the
    -- origin of portfolio 5, as a file can write it: 0x80000005 is 5'.
    -- Taken, it would give portfolio 5 the addresses of portfolio 0. The
    -- other accounts of portfolio 5 are its own.
    it "is refused when it gives the key of an account under another index" $
      inDatabaseE $ do
        _ <- testPortfolio
        fifth <- liftIO (portfolioFileOf testSeed 5)
        let key = testAccountPub
            renumbered = xPubExport btc (XPubKey key.depth key.parent 0x80000005 key.chain key.key)
            -- The first line of the file: the bip44 account on btc
            ownLine = Text.takeWhile (/= '\n') fifth <> "\n"
        lift $
          dbShouldFailWith "portfolio0 of wallet0 already has this key on btc" $
            importFile wholeFile (Text.replace ownLine (keyLine renumbered "pkh([bc560f95/44'/0'/5'])" "btc" "44") fifth)
        lift portfoliosStored `dbShouldBeE` [(0, 0, "")]

    -- The file of portfolio 0 gives the bip44 account on btctest4 the key
    -- of the second test mnemonic, at the path m/44'/1'/0' it shares with
    -- btctest
    it "is refused when it gives the accounts of two test networks at one path different keys, naming the path and the networks" $
      inDatabase $ do
        other <- liftIO $ maybe (fail "The second test seed has no key there") return =<< seedAccountKey testSeed2 Bip44 btcTest4 0
        let ownLine = keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest4" "44"
            otherLine = keyLine (xPubExport btcTest4 other) "pkh([bc560f95/44'/1'/0'])" "btctest4" "44"
        dbShouldFailWith "The bip44 account /44'/1'/0' of portfolio0 of wallet0 has one key on btctest and would get another on btctest4: the networks of one path share its key" $
          importFile wholeFile (Text.replace ownLine otherLine testWalletFile)
        walletsStored `dbShouldBe` []

    -- The file gives the bip44 account on btc two keys: its own, and that
    -- of the second test mnemonic
    it "is refused when it gives one account of a portfolio it would create two keys" $
      inDatabase $
        dbShouldFailWith "The file gives two keys for the bip44 account of portfolio0 of the wallet bc560f95 on btc" $
          importFile wholeFile (testWalletFile <> wrongKeyLine)

{- One portfolio of a file -}

-- | A file of the test wallet with two portfolios, 0 and 1, both whole.
-- It labels the first receiving address of portfolio 0, and a
-- transaction of portfolio 1.
twoPortfoliosFile :: IO Text
twoPortfoliosFile = do
  second <- secondPortfolioFile
  return $
    testWalletFile
      <> second
      <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/0\"}\n"
      <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Rent\",\"origin\":\"pkh([bc560f95/44'/0'/1'])\",\"network\":\"btc\"}\n"

-- | The scope of importwallet --portfolio 1
portfolioOne :: ImportScope
portfolioOne = ImportScope Nothing (Just 1)

-- | A file of two wallets, as exportwallet writes one for a database
-- that has them. The test wallet, bc560f95, comes first, with its
-- portfolios 0 and 1 and their two labels ('twoPortfoliosFile'). Then
-- comes portfolio 0 of the second test wallet, 4435aa1c, whole, with a
-- label for its first receiving address.
twoWalletsFile :: IO Text
twoWalletsFile = do
  first <- twoPortfoliosFile
  second <- portfolioFileOf testSeed2 0
  return $
    first
      <> second
      <> "{\"type\":\"addr\",\"ref\":\"1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2\",\"label\":\"Invoices\",\"origin\":\"pkh([4435aa1c/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/0\"}\n"

-- | The scope of importwallet --wallet with the fingerprint of the test
-- wallet, bc560f95, and with that of the second test wallet, 4435aa1c
testWalletAlone, secondWalletAlone :: ImportScope
testWalletAlone = ImportScope (Just "bc560f95") Nothing
secondWalletAlone = ImportScope (Just "4435aa1c") Nothing

-- | The active portfolio after an import: the first portfolio created in
-- a database that had none, as importwallet makes it. An import into a
-- database that has one leaves it.
activeSpec :: Spec
activeSpec =
  describe "The active portfolio after a file is imported" $ do
    it "is portfolio0 of the wallet of the file, in a database that had no portfolio" $
      inDatabaseE $ do
        _ <- importFile wholeFile testWalletFile
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)

    -- The file brings portfolios 0 and 1 of the test wallet, in the order
    -- of their indexes
    it "is the first portfolio the file creates, portfolio0, when it creates two" $
      inDatabaseE $ do
        content <- liftIO twoPortfoliosFile
        report <- importFile wholeFile content
        liftIO $ importCreated report `shouldBe` [PortfolioRef 0 0, PortfolioRef 0 1]
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)

    -- The database has portfolio0 of the second test wallet, wallet0; the
    -- file creates wallet1 with its portfolio0
    it "is the one the database had, when the file creates a portfolio of another wallet" $
      inDatabaseE $ do
        _ <- portfolioOf testMnemonic2
        _ <- importFile wholeFile testWalletFile
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)
        (map portfolioRef <$> lift getAllPortfolios) `dbShouldBeE` [PortfolioRef 0 0, PortfolioRef 1 0]

    -- The database has portfolio 0 of the test wallet; --portfolio 1
    -- creates portfolio 1
    it "is the one the database had, when the file creates another portfolio of its wallet" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        _ <- importFile portfolioOne content
        activePortfolioRef `dbShouldBeE` Just (PortfolioRef 0 0)

    -- show reads a file as importwallet would, and undoes what it did
    it "is still none after show reads a file in a database without a portfolio" $
      inDatabaseE $ do
        _ <- fileShown wholeFile (readWalletFile testWalletFile)
        activePortfolioRef `dbShouldBeE` Nothing

scopeSpec :: Spec
scopeSpec =
  describe "A file narrowed to one portfolio (--portfolio)" $ do
    -- The wallet has portfolio 0
    it "brings the accounts of that portfolio, and of no other" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        report <- importFile portfolioOne content
        liftIO $ reportAccounts report `shouldBe` map AccountAdded (twelveAccounts 0 1)
        lift accountsStored `dbShouldBeE` (twelveStored 0 0 <> twelveStored 0 1)

    -- A wallet is created with its portfolio 0, which --portfolio 1
    -- leaves out of the file
    it "refuses a portfolio other than 0 of a wallet the database does not have, and says to import the whole file" $
      inDatabase $ do
        content <- liftIO twoPortfoliosFile
        dbShouldFailWith
          "The database has no wallet of the fingerprint bc560f95, and importwallet --portfolio 1 does not create one: \
          \a wallet is created with its portfolio0. Import the whole file, without --portfolio"
          $ importFile portfolioOne content

    it "creates no wallet when it refuses a portfolio of a wallet the database does not have" $
      inDatabaseE $ do
        content <- liftIO twoPortfoliosFile
        lift $ dbShouldFail $ importFile portfolioOne content
        lift walletsStored `dbShouldBeE` []

    it "sets the labels of that portfolio" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        report <- importFile portfolioOne content
        liftIO $ reportTxLabels report `shouldBe` 1
        second <- (`portfolioNamed` 1) =<< walletNamed 0
        lift (txLabel (portfolioPool second btc) (fakeTxHash 1)) `dbShouldBeE` "Rent"

    -- The file has one label of portfolio 0, for its first receiving
    -- address. The twelve keys of portfolio 0 are not labels, and are
    -- not counted.
    it "counts the labels of the other portfolios of the file, which it does not read" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        report <- importFile portfolioOne content
        liftIO $ do
          reportOtherPortfolios report `shouldBe` 1
          (reportAddressLabels report, reportAddressesMissing report) `shouldBe` (0, 0)

    it "leaves an address of another portfolio without the label the file has for it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        content <- liftIO twoPortfoliosFile
        _ <- importFile portfolioOne content
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

    -- A sync brings nothing of a portfolio the import did not read
    it "does not ask for a sync on account of the labels it did not read" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        report <- importFile portfolioOne content
        liftIO $ reportNeedsSync report `shouldBe` False

    it "reads a record without an origin, which names no portfolio" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        second <- liftIO secondPortfolioFile
        report <- importFile portfolioOne (second <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n")
        liftIO $ (reportAddressLabels report, reportOtherPortfolios report) `shouldBe` (1, 0)
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- The file holds the keys of portfolios 0 and 1 of the test wallet
    it "refuses a portfolio the file has nothing of, naming the portfolios whose keys it holds" $
      inDatabaseE $ do
        content <- liftIO twoPortfoliosFile
        lift $
          dbShouldFailWith "The file has no portfolio2: it has portfolio0 and portfolio1" $
            importFile (ImportScope Nothing (Just 2)) content

    it "refuses a portfolio of a file of labels alone, which holds the key of no portfolio, when no label names it" $
      inDatabaseE $
        lift $
          dbShouldFailWith "The file has no portfolio1: it holds the key of no portfolio" $
            importFile portfolioOne "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n"

    -- A file of labels of another wallet: its record names portfolio 1 by
    -- its origin, and the wallet has portfolio 1
    it "reads a portfolio that a file of labels alone names by the origin of a record" $
      inDatabaseE $ do
        _ <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        report <- importFile portfolioOne ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Rent\",\"origin\":\"pkh([bc560f95/44'/0'/1'])\",\"network\":\"btc\"}\n")
        liftIO $ reportTxLabels report `shouldBe` 1
        lift (txLabel (portfolioPool second btc) (fakeTxHash 1)) `dbShouldBeE` "Rent"

    it "brings every portfolio of the file without --portfolio, and leaves no label out" $
      inDatabaseE $ do
        content <- liftIO twoPortfoliosFile
        report <- importFile wholeFile content
        liftIO $ do
          reportWallets report `shouldBe` [WalletImport 0 testFingerprint True [PortfolioImport 0 True, PortfolioImport 1 True]]
          (reportAddressLabels report, reportTxLabels report, reportOtherPortfolios report) `shouldBe` (1, 1, 0)

{- One wallet of a file -}

walletScopeSpec :: Spec
walletScopeSpec =
  describe "A file of several wallets narrowed to one of them (--wallet)" $ do
    it "brings the wallet of that fingerprint with every portfolio the file has of it, and no other wallet" $
      inDatabaseE $ do
        content <- liftIO twoWalletsFile
        report <- importFile testWalletAlone content
        liftIO $ reportWallets report `shouldBe` [WalletImport 0 testFingerprint True [PortfolioImport 0 True, PortfolioImport 1 True]]
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", "")]

    -- The second wallet of the file is the first one of the database
    it "brings a wallet that is not the first of the file, which takes the lowest free index" $
      inDatabaseE $ do
        content <- liftIO twoWalletsFile
        _ <- importFile secondWalletAlone content
        lift walletsStored `dbShouldBeE` [(0, "4435aa1c", "")]
        lift accountsStored `dbShouldBeE` twelveStored 0 0

    -- The database has portfolio 0 of the test wallet
    it "brings one portfolio of that wallet with --portfolio beside it, and nothing of the other wallets" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoWalletsFile
        report <- importFile (ImportScope (Just "bc560f95") (Just 1)) content
        liftIO $ reportWallets report `shouldBe` [WalletImport 0 testFingerprint False [PortfolioImport 1 True]]
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", "")]

    -- The index 0 names a portfolio of each of the two wallets
    it "is asked for when --portfolio comes alone, with the fingerprints of the wallets of the file" $
      inDatabase $ do
        content <- liftIO twoWalletsFile
        dbShouldFailWith
          "The file holds the wallets bc560f95 and 4435aa1c, and --portfolio alone does not say which of them portfolio0 is of. \
          \Name the wallet by its fingerprint too, as in --wallet bc560f95 --portfolio 0"
          $ importFile (ImportScope Nothing (Just 0)) content

    it "creates nothing when --portfolio comes alone and is refused" $
      inDatabaseE $ do
        content <- liftIO twoWalletsFile
        lift $ dbShouldFail $ importFile (ImportScope Nothing (Just 0)) content
        lift walletsStored `dbShouldBeE` []

    it "sets the labels of that wallet" $
      inDatabaseE $ do
        content <- liftIO twoWalletsFile
        report <- importFile secondWalletAlone content
        liftIO $ (reportAddressLabels report, reportTxLabels report) `shouldBe` (1, 0)

    -- The file has two labels of the test wallet: one for the first
    -- receiving address of its portfolio 0, and one for a transaction of
    -- its portfolio 1
    it "counts the labels of the other wallets of the file, which it does not read" $
      inDatabaseE $ do
        content <- liftIO twoWalletsFile
        report <- importFile secondWalletAlone content
        liftIO $ reportOtherPortfolios report `shouldBe` 2

    it "leaves an address of another wallet without the label the file has for it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        content <- liftIO twoWalletsFile
        _ <- importFile secondWalletAlone content
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

    it "reads a record without an origin, which names no wallet" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        second <- liftIO (portfolioFileOf testSeed2 0)
        report <- importFile secondWalletAlone (second <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n")
        liftIO $ (reportAddressLabels report, reportOtherPortfolios report) `shouldBe` (1, 0)
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    it "refuses a wallet the file has nothing of, naming the wallets it has" $
      inDatabase $ do
        content <- liftIO twoWalletsFile
        dbShouldFailWith "The file has no wallet 12345678: it has the wallets bc560f95 and 4435aa1c" $
          importFile (ImportScope (Just "12345678") Nothing) content

    it "refuses a wallet a file of one wallet does not have, naming the one it has" $
      inDatabase $
        dbShouldFailWith "The file has no wallet 4435aa1c: it has the wallet bc560f95" $
          importFile secondWalletAlone testWalletFile

    it "refuses a wallet of a file of labels alone whose records name no wallet" $
      inDatabase $
        dbShouldFailWith "The file has no wallet bc560f95: it names no wallet" $
          importFile testWalletAlone "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n"

    -- The file has portfolio 0 alone of the second test wallet
    it "refuses a portfolio the file has nothing of in the wallet named, naming the portfolios of that wallet" $
      inDatabase $ do
        content <- liftIO twoWalletsFile
        dbShouldFailWith "The file has no portfolio1 of the wallet 4435aa1c: it has portfolio0 of that wallet" $
          importFile (ImportScope (Just "4435aa1c") (Just 1)) content

    -- A wallet is created with its portfolio 0, which --portfolio 1
    -- leaves out. The file holds other wallets, so the advice is to
    -- import the wallet, not the file.
    it "refuses a portfolio other than 0 of a wallet the database does not have, and says to import the whole wallet" $
      inDatabase $ do
        content <- liftIO twoWalletsFile
        dbShouldFailWith
          "The database has no wallet of the fingerprint bc560f95, and importwallet --portfolio 1 does not create one: \
          \a wallet is created with its portfolio0. Import the whole wallet, without --portfolio"
          $ importFile (ImportScope (Just "bc560f95") (Just 1)) content

    it "brings every wallet of the file without --wallet" $
      inDatabaseE $ do
        content <- liftIO twoWalletsFile
        _ <- importFile wholeFile content
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", ""), (1, "4435aa1c", "")]

{- Records that are not hw's -}

foreignSpec :: Spec
foreignSpec =
  describe "An xpub record that is not one of hw's, as another wallet writes it" $ do
    -- A key labelled with the name of its signer, and nothing else
    it "is left aside with the reason when it names no network" $
      inDatabaseE $
        leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The record names no network. hw imports the wallets exportwallet writes, not a key alone: a key is one account, and does not say which portfolio it belongs to"]

    it "is left aside with the reason when nothing says the type of its account: no origin, no purpose, and a key that marks none" $
      inDatabaseE $
        leftAside ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\"}\n")
          `dbShouldBeE` [AccountLeftAside mainKeyStart "The record does not say the type of its account. hw imports the wallets exportwallet writes, not a key alone"]

    it "creates no wallet, no portfolio and no account" $
      inDatabaseE $ do
        report <- importFile wholeFile ("{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n")
        liftIO $ reportWallets report `shouldBe` []
        lift walletsStored `dbShouldBeE` []
        lift accountsStored `dbShouldBeE` []

    it "does not keep the labels of its file from being set" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- mainWithAddress
        report <-
          importFile wholeFile $
            "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"label\":\"BIP39\"}\n"
              <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n"
              <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"First salary\",\"origin\":\"" <> mainOrigin <> "\"}\n"
        liftIO $ (reportAddressLabels report, reportTxLabels report) `shouldBe` (1, 1)
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]
        lift (txLabel (portfolioPool portfolio btc) (fakeTxHash 1)) `dbShouldBeE` "First salary"

{- A whole wallet, from one database to another -}

-- | The file exportwallet writes for the wallet of a story. The wallet of
-- the test mnemonic is described as "steel backup". Its portfolio 0,
-- "savings", has three labelled addresses: the first of its bip44 account
-- on Bitcoin, "Salary", the second of its bip84 account there, "Tips",
-- and the first of its bip44 account on btctest4, "Faucet". It has the
-- payment of 50000 on Bitcoin, labelled "First salary", and its coin,
-- which the user locked. Its portfolio 1, "spending", has a transaction
-- on Bitcoin Cash labelled "Refund", which the chain does not have yet.
storyFile :: IO Text
storyFile = do
  exported <- inDatabase $ runExceptT $ do
    savings <- testPortfolio
    spending <- nextPortfolioOf testMnemonic
    setWalletDescription savings.portfolioWalletId "steel backup"
    setPortfolioDescription savings.portfolioId "savings"
    setPortfolioDescription spending.portfolioId "spending"
    (mainId, _) <- accountOf savings btc Bip44
    (segwitId, _) <- accountOf savings btc Bip84
    (testId, _) <- accountOf savings btcTest4 Bip44
    _ <- genExtAddress def mainId "Salary"
    _ <- genExtAddress def segwitId ""
    _ <- genExtAddress def segwitId "Tips"
    _ <- genExtAddress def testId "Faucet"
    lift $ setTxLabel (portfolioPool savings btc) (fakeTxHash 1) "First salary"
    lift $ paymentStored (portfolioPool savings btc) 800005
    lift $ coinStored mainId True
    lift $ setTxLabel (portfolioPool spending bch) (fakeTxHash 2) "Refund"
    portfolios <- lift getAllPortfolios
    recordsText <$> walletRecords portfolios
  expectRight exported

roundTripSpec :: Spec
roundTripSpec =
  describe "A whole wallet exported, then imported into another database" $ do
    it "has the description of the wallet and of each of its portfolios again" $ do
      content <- storyFile
      inDatabaseE $ do
        _ <- importFile wholeFile content
        lift walletsStored `dbShouldBeE` [(0, "bc560f95", "steel backup")]
        lift portfoliosStored `dbShouldBeE` [(0, 0, "savings"), (0, 1, "spending")]

    it "has the twelve accounts of each portfolio again, with their keys" $ do
      content <- storyFile
      inDatabaseE $ do
        _ <- importFile wholeFile content
        lift accountsStored `dbShouldBeE` (twelveStored 0 0 <> twelveStored 0 1)
        lift bitcoinKeysStored `dbShouldBeE` [testAccountXPub, testSegwitAccountXPub, testAccountXPub1, testSegwitAccountXPub1]

    -- The addresses are generated up to the labelled ones: one in the
    -- bip44 account, two in the bip84 account
    it "has the labels of the addresses again, in the account of each" $ do
      content <- storyFile
      inDatabaseE $ do
        report <- importFile wholeFile content
        liftIO $ reportAddressLabels report `shouldBe` 3
        portfolio <- testPortfolio
        (mainId, _) <- accountOf portfolio btc Bip44
        (segwitId, _) <- accountOf portfolio btc Bip84
        lift (addressLabels mainId) `dbShouldBeE` [(0, "Salary")]
        lift (addressLabels segwitId) `dbShouldBeE` [(0, ""), (1, "Tips")]

    -- btctest has the same key and the same address as btctest4, and no
    -- label of its own
    it "has the label of an address of a test network again on that network alone" $ do
      content <- storyFile
      inDatabaseE $ do
        _ <- importFile wholeFile content
        portfolio <- testPortfolio
        (test4Id, _) <- accountOf portfolio btcTest4 Bip44
        (testId, _) <- accountOf portfolio btcTest Bip44
        lift (addressLabels test4Id) `dbShouldBeE` [(0, "Faucet")]
        lift (addressLabels testId) `dbShouldBeE` []

    it "has the labels of the transactions again, each in its portfolio on its network, before any sync" $ do
      content <- storyFile
      inDatabaseE $ do
        report <- importFile wholeFile content
        liftIO $ reportTxLabels report `shouldBe` 2
        savings <- (`portfolioNamed` 0) =<< walletNamed 0
        spending <- (`portfolioNamed` 1) =<< walletNamed 0
        lift (txLabel (portfolioPool savings btc) (fakeTxHash 1)) `dbShouldBeE` "First salary"
        lift (txLabel (portfolioPool spending bch) (fakeTxHash 2)) `dbShouldBeE` "Refund"
        lift (txLabel (portfolioPool spending btc) (fakeTxHash 1)) `dbShouldBeE` ""

    -- A new database has no coin before its first sync: the lock waits
    -- for it, and the answer says that a sync is needed
    it "cannot lock the coin the user had locked before a sync brings the coin, and says so" $ do
      content <- storyFile
      inDatabaseE $ do
        report <- importFile wholeFile content
        liftIO $ (reportCoinsLocked report, reportCoinsMissing report, reportNeedsSync report) `shouldBe` (0, 1, True)

    it "locks the coin the user had locked once the wallet has it, on a second import" $ do
      content <- storyFile
      inDatabaseE $ do
        _ <- importFile wholeFile content
        (accId, _) <- testNewAcc
        lift $ coinStored accId False
        report <- importFile wholeFile content
        liftIO $ (reportCoinsLocked report, reportCoinsMissing report) `shouldBe` (1, 0)
        lift (coinLocked accId) `dbShouldBeE` Just True

{- Importing addresses -}

-- | The address book in a file: the landlord is at
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, an address of another wallet
contactRecordSpec :: Spec
contactRecordSpec = do
  let landlord = "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
      landlordLine = "{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"label\":\"Landlord\",\"network\":\"btc\",\"tags\":[\"rent\"],\"contact\":true}\n"
      labelOf = fmap contactLabel
      tagsOf = fmap (map (tagText . shownTag) . contactTags)

  describe "The records of the address book" $ do
    it "are an addr record for each contact, with its label, its network and its tags, marked as a contact" $
      inDatabaseE $ do
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc landlord [rentTag]
        records <- lift contactRecords
        liftIO $ recordsText records `shouldBe` landlordLine

    it "leave out the label of a contact that has none" $
      inDatabaseE $ do
        addContact btc (otherAddr 0) ""
        records <- lift contactRecords
        liftIO $ recordsText records `shouldBe` "{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"network\":\"btc\",\"contact\":true}\n"

    it "are none for an empty address book" $
      inDatabaseE $ do
        records <- lift contactRecords
        liftIO $ records `shouldBe` []

    it "are no part of the records of a portfolio" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        addContact btc (otherAddr 0) "Landlord"
        records <- walletRecords [portfolio]
        liftIO $ filter isContactRecord records `shouldBe` []

  describe "An addr record marked as a contact" $ do
    it "adds its address to the address book, with its label and its tags" $
      inDatabaseE $ do
        report <- importFile wholeFile landlordLine
        liftIO $ report `shouldBe` nothingDone {reportContacts = 1}
        lift $ (labelOf <$> contactAt btc landlord) `dbShouldBe` Just "Landlord"
        lift $ (tagsOf <$> contactAt btc landlord) `dbShouldBe` Just ["rent"]

    it "gives a contact of the address book the label of the record" $
      inDatabaseE $ do
        addContact btc (otherAddr 0) "Mr Smith"
        report <- importFile wholeFile landlordLine
        liftIO $ report `shouldBe` nothingDone {reportContacts = 1}
        lift $ (labelOf <$> contactAt btc landlord) `dbShouldBe` Just "Landlord"

    it "keeps the label of a contact when the record gives none" $
      inDatabaseE $ do
        addContact btc (otherAddr 0) "Mr Smith"
        _ <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"network\":\"btc\",\"contact\":true}\n"
        lift $ (labelOf <$> contactAt btc landlord) `dbShouldBe` Just "Mr Smith"

    it "adds its tags to those the contact has" $
      inDatabaseE $ do
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc landlord [kycTag]
        _ <- importFile wholeFile landlordLine
        lift $ (tagsOf <$> contactAt btc landlord) `dbShouldBe` Just ["kyc", "rent"]

    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is the first receiving address
    -- of the wallet
    it "is left aside when its address is one of the wallet, and counted apart" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Me\",\"network\":\"btc\",\"contact\":true}\n"
        liftIO $ report `shouldBe` nothingDone {reportContactsOwn = 1}
        lift $ (labelOf <$> contactAt btc "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj") `dbShouldBe` Nothing

    it "is counted among the records hw keeps nothing of when it names no network" $
      inDatabaseE $ do
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"label\":\"Landlord\",\"contact\":true}\n"
        liftIO $ report `shouldBe` nothingDone {reportNotKept = 1}

    -- The record is of no portfolio, so --portfolio does not leave it out
    it "is read whatever portfolio of the file the import is narrowed to" $
      inDatabaseE $ do
        report <- importFile (ImportScope Nothing (Just 0)) (testWalletFile <> landlordLine)
        liftIO $ reportContacts report `shouldBe` 1

    -- Without the key contact, the record labels an address the wallet
    -- may hold, and waits for it
    it "is the only addr record that goes to the address book: one without the mark waits for its address" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"label\":\"Landlord\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1}
        lift $ (labelOf <$> contactAt btc landlord) `dbShouldBe` Nothing

-- The file of the whole database names the tags that nothing has on its
-- first key record, with what the user set them to
unusedTagSpec :: Spec
unusedTagSpec = do
  describe "The records of the whole database" $ do
    -- cold is blue and locks its coins, and no address has it yet
    it "name a tag that nothing has on the first key record, with its colour and its lock" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        lift $ setTagColor coldTag (readTagColor "blue")
        lift $ setTagLocksCoins coldTag True
        records <- databaseRecords [portfolio]
        liftIO $ do
          map recordLine (take 1 records)
            `shouldBe` ["{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44,\"tags\":[\"cold\"],\"tagcolors\":{\"cold\":\"blue\"},\"taglocks\":[\"cold\"]}"]
          [recordRef r | r <- drop 1 records, recordTags r /= Nothing] `shouldBe` []

    -- The record of the address names the tag, with its lock
    it "name no tag there that a record of the file has" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- testNewAcc
        addr <- genExtAddress def accId ""
        lift $ tagAddress addr [coldTag]
        lift $ setTagLocksCoins coldTag True
        records <- databaseRecords [portfolio]
        liftIO $ [(recordType r, recordTagLocks r) | r <- records, recordTagLocks r /= Nothing] `shouldBe` [(AddrRecord, Just ["cold"])]

    -- kyc was on the address and is on nothing now, and the user gave it
    -- no colour and no lock: it is still a tag of the database
    it "name there a tag that nothing has and that has no colour and no lock, under tags alone" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (accId, _) <- testNewAcc
        addr <- genExtAddress def accId ""
        lift $ tagAddress addr [kycTag]
        _ <- lift $ untagAddress addr [kycTag]
        records <- databaseRecords [portfolio]
        liftIO $
          map recordLine (take 1 records)
            `shouldBe` ["{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"purpose\":44,\"tags\":[\"kyc\"]}"]

    -- A file of one wallet or of one portfolio is made of these records
    it "are alone in naming it: the records of a wallet do not" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        lift $ setTagLocksCoins coldTag True
        records <- walletRecords [portfolio]
        liftIO $ recordsText records `shouldBe` testWalletFile

  describe "A key record that names tags" $ do
    let withSettings = taggedKeyLine
        settingsOf tag = fmap (\s -> (colorName <$> tagSetColor s, tagSetLocksCoins s)) . Map.lookup tag <$> tagSettings

    it "makes each of them a tag of the database, with its colour or its lock, on nothing" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        _ <- importFile wholeFile withSettings
        lift (settingsOf kycTag) `dbShouldBeE` Just (Just "red", False)
        lift (settingsOf coldTag) `dbShouldBeE` Just (Nothing, True)
        lift (accountAddressTags accId) `dbShouldBeE` []

    it "makes a tag of the database of one that has no colour and no lock" $
      inDatabaseE $ do
        _ <- mainWithAddress
        _ <- importFile wholeFile withSettings
        lift (settingsOf rentTag) `dbShouldBeE` Just (Nothing, False)

    it "leaves a tag the colour it has here" $
      inDatabaseE $ do
        _ <- mainWithAddress
        lift $ setTagColor kycTag (readTagColor "blue")
        _ <- importFile wholeFile withSettings
        lift (settingsOf kycTag) `dbShouldBeE` Just (Just "blue", False)

    -- The tags are of no portfolio: the import is narrowed to portfolio
    -- 1, and the record is of portfolio 0
    it "is read whatever portfolio of the file the import is narrowed to" $
      inDatabaseE $ do
        _ <- mainWithAddress
        second <- liftIO secondPortfolioFile
        _ <- importFile (ImportScope Nothing (Just 1)) (withSettings <> second)
        lift (settingsOf coldTag) `dbShouldBeE` Just (Nothing, True)

importAddressSpec :: Spec
importAddressSpec = do
  describe "An addr record" $ do
    it "labels an address of the wallet, in whatever account has it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- The third receiving address, which the account has not generated
    it "derives an address the wallet does not have from its key path, generating the chain up to it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 2 <> "\",\"label\":\"From Bob\",\"origin\":\"" <> mainOrigin <> "\",\"keypath\":\"/0/2\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, ""), (2, "From Bob")]

    it "derives the address without an origin, in the account whose key gives it" $
      inDatabaseE $ do
        _ <- testNewAcc2
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 1 <> "\",\"label\":\"From Bob\",\"keypath\":\"/0/1\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, "From Bob")]

    -- The second receiving address of the bip84 account, which has
    -- generated none: its origin names the account, its key path the
    -- address
    it "derives an address of the bip84 account of the portfolio its wpkh origin names" $
      inDatabaseE $ do
        (accId, _) <- testNewSegwitAcc
        report <- importFile wholeFile ("{\"type\":\"addr\",\"ref\":\"" <> segwitExternalAddrsText !! 1 <> "\",\"label\":\"Tips\",\"origin\":\"" <> segwitOrigin <> "\",\"network\":\"btc\",\"keypath\":\"/0/1\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, ""), (1, "Tips")]

    -- Both accounts have the address, at /0/0 of the key they share
    it "labels an address two test networks share on the network its record names alone" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (testId, _) <- accountOf portfolio btcTest Bip44
        (test4Id, _) <- accountOf portfolio btcTest4 Bip44
        _ <- genExtAddress def testId ""
        _ <- genExtAddress def test4Id ""
        report <- importFile wholeFile ("{\"type\":\"addr\",\"ref\":\"" <> testnetExternalAddrText <> "\",\"label\":\"Faucet\",\"network\":\"btctest4\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels testId) `dbShouldBeE` [(0, "")]
        lift (addressLabels test4Id) `dbShouldBeE` [(0, "Faucet")]

    it "leaves the label of a change address aside, and generates none" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile ("{\"type\":\"addr\",\"ref\":\"" <> internalAddrsText !! 0 <> "\",\"label\":\"Change\",\"keypath\":\"/1/0\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportChangeAddresses = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountInternal acc `shouldBe` 0

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address,
    -- /1/0, which the account has not generated. A sync generates it once
    -- the chain has used it, and its coin must be under the tag then.
    it "keeps the tags of a change address the wallet has not generated, and generates none" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\",\"tags\":[\"kyc\"],\"keypath\":\"/1/0\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressTags = 1}
        lift (accountAddressTags accId) `dbShouldBeE` [((True, 0, "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"), [kycTag])]
        acc <- getAccountById accId
        liftIO $ dBAccountInternal acc `shouldBe` 0

    it "takes the lock of a tag from the record of a change address the wallet has not generated" $
      inDatabaseE $ do
        _ <- mainWithAddress
        _ <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\",\"tags\":[\"kyc\"],\"taglocks\":[\"kyc\"],\"keypath\":\"/1/0\"}\n"
        lift lockingTags `dbShouldBeE` [kycTag]

    it "keeps the tags of such a change address, and still leaves its label aside" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\",\"label\":\"Change\",\"tags\":[\"kyc\"],\"keypath\":\"/1/0\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressTags = 1, reportChangeAddresses = 1}
        lift (accountAddressTags accId) `dbShouldBeE` [((True, 0, "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"), [kycTag])]

    -- 1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6 is the change address /1/1, not
    -- /1/0: the key path of the record does not give its address
    it "keeps no tag for an address that the key path of its record does not derive" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6\",\"tags\":[\"kyc\"],\"keypath\":\"/1/0\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1}
        lift (accountAddressTags accId) `dbShouldBeE` []

    -- The address /0/1001, with /0/0 used: one past the thousand, so it
    -- is not generated. Its tag is kept all the same, and nothing of the
    -- record is left to place.
    it "keeps the tags of a receiving address over a thousand past the last used one, and generates none" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2\",\"tags\":[\"kyc\"],\"keypath\":\"/0/1001\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressTags = 1}
        lift (accountAddressTags accId) `dbShouldBeE` [((False, 1001, "1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2"), [kycTag])]
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1

    -- hw keeps a label on the row of its address, so the label waits
    it "keeps the tags of such a receiving address, and counts its label among those not in the wallet" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2\",\"label\":\"Far\",\"tags\":[\"kyc\"],\"keypath\":\"/0/1001\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressTags = 1, reportAddressesMissing = 1, reportAddressesTooFar = 1}

    -- The address /0/1000, with /0/0 used: the thousandth past it
    it "derives an address up to a thousand past the last used one of its chain" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1KANnCm9fYm1z6A8o7hPMjsBCqgFWt9v8N\",\"label\":\"Far\",\"keypath\":\"/0/1000\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1001

    -- The address /0/1001, with /0/0 used: one past the thousand
    it "does not derive an address over a thousand past the last used one, and counts it among those not in the wallet" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1BUtUkmTvZHpz7wPeEJNTX4VLTgLNxKNt2\",\"label\":\"Far\",\"keypath\":\"/0/1001\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1, reportAddressesTooFar = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1

    -- The address /0/1000 of a chain with no used address: the thousand
    -- count from its start
    it "counts the thousand from the start of a chain with no used address" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1KANnCm9fYm1z6A8o7hPMjsBCqgFWt9v8N\",\"label\":\"Far\",\"keypath\":\"/0/1000\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1, reportAddressesTooFar = 1}
        acc <- getAccountById accId
        liftIO $ dBAccountExternal acc `shouldBe` 1

    it "counts an address the wallet does not have and no key path places" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <- importFile wholeFile ("{\"type\":\"addr\",\"ref\":\"" <> externalAddrsText !! 5 <> "\",\"label\":\"Lost\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressesMissing = 1}

    it "does nothing without a label" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\"}\n"
        liftIO $ report `shouldBe` nothingDone
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

    it "removes a label with an empty one" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc
        _ <- genExtAddress def accId "Salary"
        _ <- importFile wholeFile "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"\"}\n"
        lift (addressLabels accId) `dbShouldBeE` [(0, "")]

  describe "A pubkey record" $ do
    it "labels the address of the key, when the wallet has it" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        -- The key of the first receiving address, /0/0 of the account key
        let key = derivePubPath (Deriv :/ 0 :/ 0) testAccountPub
            hex = encodeHex (exportPubKey True key.key)
        report <- importFile wholeFile ("{\"type\":\"pubkey\",\"ref\":\"" <> hex <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1}
        lift (addressLabels accId) `dbShouldBeE` [(0, "Salary")]

    -- An x-only key of 32 bytes, as Taproot writes them
    it "is left aside when hw cannot read the key" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <- importFile wholeFile "{\"type\":\"pubkey\",\"ref\":\"83409659355b6d1cc3c32decd5d561abaac86c37a353b52895a5e6c196d6f448\",\"label\":\"Taproot\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportNotKept = 1}

{- Importing transactions -}

importTxSpec :: Spec
importTxSpec =
  describe "A tx record" $ do
    it "labels the transaction in the portfolio its origin names, before the sync has it" $
      inDatabaseE $ do
        pool <- testPool
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel pool (fakeTxHash 1)) `dbShouldBeE` "Salary"

    -- A label is of the portfolio on a network, whichever of its accounts
    -- there the origin names
    it "labels the transaction of a portfolio whose bip84 account its origin names" $
      inDatabaseE $ do
        pool <- testPool
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> segwitOrigin <> "\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel pool (fakeTxHash 1)) `dbShouldBeE` "Salary"

    -- The origin m/44'/1'/0' is that of the bip44 account on every test
    -- network
    it "labels a transaction on the test network its record names alone, among those that share its origin" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Faucet\",\"origin\":\"pkh([bc560f95/44'/1'/0'])\",\"network\":\"btctest4\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (txLabel (portfolioPool portfolio btcTest4) (fakeTxHash 1)) `dbShouldBeE` "Faucet"
        lift (txLabel (portfolioPool portfolio btcTest) (fakeTxHash 1)) `dbShouldBeE` ""

    -- Portfolios 0 and 1 of the wallet both have the payment on Bitcoin,
    -- and portfolio 2 does not
    it "labels a transaction without origin in every portfolio that has it" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        third <- nextPortfolioOf testMnemonic
        lift $ paymentStored (portfolioPool first btc) 800002
        lift $ paymentStored (portfolioPool second btc) 800002
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 2}
        lift (mapM (\portfolio -> txLabel (portfolioPool portfolio btc) (fakeTxHash 1)) [first, second, third]) `dbShouldBeE` ["Salary", "Salary", ""]

    -- Portfolios 0 and 1 of the wallet both have accounts on Bitcoin, and
    -- portfolio 0 alone has the payment. A network names no portfolio.
    it "labels a transaction whose record names its network and no origin in the portfolios that have it there alone" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        lift $ paymentStored (portfolioPool first btc) 800002
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"network\":\"btc\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxLabels = 1}
        lift (mapM (\portfolio -> txLabel (portfolioPool portfolio btc) (fakeTxHash 1)) [first, second]) `dbShouldBeE` ["Salary", ""]

    it "counts a transaction whose record names its network and no origin when no portfolio has it, and labels it nowhere" $
      inDatabaseE $ do
        first <- testPortfolio
        second <- nextPortfolioOf testMnemonic
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"network\":\"btc\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxsMissing = 1}
        lift (mapM (\portfolio -> txLabel (portfolioPool portfolio btc) (fakeTxHash 1)) [first, second]) `dbShouldBeE` ["", ""]

    -- Portfolio 0 has the payment on Bitcoin Cash, and the record names
    -- Bitcoin
    it "does not label a transaction a portfolio has on another network than the one its record names" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        _ <- lift $ repsertSummary (portfolioPool portfolio bch) (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"network\":\"btc\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxsMissing = 1}
        lift (txLabel (portfolioPool portfolio bch) (fakeTxHash 1)) `dbShouldBeE` ""

    it "counts a transaction without origin that no portfolio has" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportTxsMissing = 1}

    -- The origin of account 0 of the second seed
    it "counts a transaction whose origin names an account the wallet does not have" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile ("{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"pkh([4435aa1c/44'/0'/0'])\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportOtherAccounts = 1}

    it "is counted among the unreadable lines when its reference is not a transaction id" $
      inDatabaseE $ do
        _ <- testPortfolio
        report <- importFile wholeFile "{\"type\":\"tx\",\"ref\":\"not-a-txid\",\"label\":\"Salary\"}\n"
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1]}

{- Importing coins -}

importCoinSpec :: Spec
importCoinSpec =
  describe "An output record" $ do
    it "locks the coin when it is not spendable, as lockcoin does" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        lift $ coinStored accId False
        report <- importFile wholeFile ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsLocked = 1}
        lift (coinLocked accId) `dbShouldBeE` Just True

    it "unlocks the coin when it is spendable" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        lift $ coinStored accId True
        report <- importFile wholeFile ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":true}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsUnlocked = 1}
        lift (coinLocked accId) `dbShouldBeE` Just False

    -- A store that serves two test networks the same chain reports the
    -- coin on both, and each account keeps its row
    it "locks a coin two test networks share on the network its record names alone" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (testId, _) <- accountOf portfolio btcTest Bip44
        (test4Id, _) <- accountOf portfolio btcTest4 Bip44
        lift $ coinStoredAt testnetExternalAddrText testId False
        lift $ coinStoredAt testnetExternalAddrText test4Id False
        report <- importFile wholeFile ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"network\":\"btctest4\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsLocked = 1}
        lift (coinLocked testId) `dbShouldBeE` Just False
        lift (coinLocked test4Id) `dbShouldBeE` Just True

    it "counts a coin the wallet does not have" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <- importFile wholeFile ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ report `shouldBe` nothingDone {reportCoinsMissing = 1}

    -- hw keeps no label on a coin: a coin shows the label of its transaction
    it "leaves the label of a coin aside" $
      inDatabaseE $ do
        (accId, _) <- mainWithAddress
        lift $ coinStored accId False
        report <- importFile wholeFile ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (received)\"}\n")
        liftIO $ report `shouldBe` nothingDone {reportNotKept = 1}

{- The rest of a file -}

importOtherSpec :: Spec
importOtherSpec =
  describe "The import of a file" $ do
    -- Line 3 is of a type the BIP does not define, line 4 is not JSON, and
    -- line 5 has no reference
    it "leaves aside inputs, silent payment keys and other types, and numbers the lines that are not records" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <-
          importFile wholeFile $
            Text.unlines
              [ "{\"type\":\"input\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (input)\"}",
                "{\"type\":\"spscan\",\"ref\":\"spscan1q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zsq9q6qjevn2kmdrnpuxt0v6h2kr2a2epkr0g6nk55ftf0xcxtddazgkrth3e\",\"label\":\"Scan\"}",
                "{\"type\":\"future\",\"ref\":{\"not\":\"validated\"},\"label\":\"Unknown\"}",
                "not a record",
                "{\"type\":\"addr\",\"label\":\"Salary\"}",
                "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Salary\"}"
              ]
        liftIO $ report `shouldBe` nothingDone {reportAddressLabels = 1, reportNotKept = 3, reportUnreadable = [4, 5]}

    it "takes the keys first, wherever they are in the file, so that their records find their accounts" $
      inDatabaseE $ do
        report <-
          importFile wholeFile $
            "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n" <> testWalletFile
        liftIO $ reportTxLabels report `shouldBe` 1

    it "does nothing with an empty file" $
      inDatabaseE $ do
        report <- importFile wholeFile ""
        liftIO $ report `shouldBe` nothingDone
        liftIO $ reportNeedsSync report `shouldBe` False

    it "needs a sync when it labels something the wallet does not have" $
      inDatabaseE $ do
        _ <- mainWithAddress
        report <- importFile wholeFile ("{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"spendable\":false}\n")
        liftIO $ reportNeedsSync report `shouldBe` True

{- A line that is not a record -}

wordLineSpec :: Spec
wordLineSpec =
  describe "A line of a file that is not a record" $ do
    -- Two words of the test mnemonic, one a line, as a backup on paper is
    -- copied
    it "is counted by its number, and nothing is made of it" $
      inDatabaseE $ do
        report <- importFile wholeFile "snow\nsenior\n"
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1, 2]}

    -- hw imports the wallets exportwallet writes, not a key alone
    it "is all an extended public key alone on its line is: no account is made of it" $
      inDatabaseE $ do
        report <- importFile wholeFile (testAccountXPub <> "\n")
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1]}
        lift accountsStored `dbShouldBeE` []

    it "is all a descriptor alone on its line is: no account is made of it" $
      inDatabaseE $ do
        report <- importFile wholeFile ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87\n")
        liftIO $ report `shouldBe` nothingDone {reportUnreadable = [1]}
        lift accountsStored `dbShouldBeE` []

{- Showing a file -}

-- | The file of a wallet of the text, as show shows it
shownOf :: Text -> ExceptT String (DB IO) FileShown
shownOf content = fileShown wholeFile (readWalletFile content)

-- | The records of each account of a file as show sorts them, by their
-- references, then the records of no account of the file
recordsSorted :: FileShown -> ([[Text]], [Text])
recordsSorted shown = ([recordRef <$> records | (_, records) <- shownAccounts shown], recordRef <$> shownOthers shown)

showFileSpec :: Spec
showFileSpec = do
  describe "A file of a wallet as show shows it" $ do
    it "gives a wallet and its portfolio the database does not have as importwallet would make them, and the database keeps none of it" $
      inDatabaseE $ do
        shown <- shownOf testWalletFile
        liftIO $ do
          shownWallets shown `shouldBe` [WalletImport 0 testFingerprint True [PortfolioImport 0 True]]
          map fst (shownAccounts shown) `shouldBe` map AccountAdded (twelveAccounts 0 0)
        lift walletsStored `dbShouldBeE` []
        lift accountsStored `dbShouldBeE` []

    -- The wallet has portfolio 0. importwallet would create portfolio 1.
    it "keeps what the wallet held before, and undoes only what it took in" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO secondPortfolioFile
        shown <- shownOf content
        liftIO $ map fst (shownAccounts shown) `shouldBe` map AccountAdded (twelveAccounts 0 1)
        lift accountsStored `dbShouldBeE` twelveStored 0 0

    it "refuses a file importwallet would refuse, with its refusal" $
      inDatabase $ do
        _ <- runExceptT testPortfolio
        dbShouldFailWith "The bip44 account of portfolio0 of wallet0 on btc has another key than the file gives it: the file and the database are not of one mnemonic" $
          shownOf wrongKeyLine

    -- The wallet has portfolios 0 and 1, which hold the keys of the file
    it "puts each record under the account of the file whose origin it names" $
      inDatabaseE $ do
        _ <- testPortfolio
        _ <- nextPortfolioOf testMnemonic
        shown <-
          shownOf $
            mainKeyLine
              <> secondKeyLine
              <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Savings\",\"origin\":\"pkh([bc560f95/44'/0'/1'])\"}\n"
              <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\",\"origin\":\"" <> mainOrigin <> "\"}\n"
        liftIO $ recordsSorted shown `shouldBe` ([["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"], [paymentTxid]], [])

    -- The keys of the bip44 accounts on btctest and on btctest4 are one
    -- key with one origin. The wallet has the portfolio.
    it "puts a record under the account on the network it names, among the accounts of test networks that share its origin" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <-
          shownOf $
            keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest" "44"
              <> keyLine testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest4" "44"
              <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Faucet\",\"origin\":\"pkh([bc560f95/44'/1'/0'])\",\"network\":\"btctest4\"}\n"
        liftIO $ recordsSorted shown `shouldBe` ([[], [paymentTxid]], [])

    -- Nothing in the file says which account the address is of
    it "puts a record without an origin apart" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf (mainKeyLine <> "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\"}\n")
        liftIO $ recordsSorted shown `shouldBe` ([[]], ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"])

    -- The origin of the test vectors of BIP 329, of another seed
    it "puts apart a record whose origin names no account of the file" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf (mainKeyLine <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Transaction\",\"origin\":\"wpkh([d34db33f/84'/0'/0'])\"}\n")
        liftIO $ recordsSorted shown `shouldBe` ([[]], [paymentTxid])

    -- The key record has no origin. The wallet holds the key, and has the
    -- fingerprint of its seed, bc560f95.
    it "finds the account of a key without an origin by the fingerprint of the wallet that holds the key" $
      inDatabaseE $ do
        _ <- mainWithAddress
        _ <- nextPortfolioOf testMnemonic
        shown <-
          shownOf $
            "{\"type\":\"xpub\",\"ref\":\"" <> testAccountXPub <> "\",\"network\":\"btc\",\"purpose\":44}\n"
              <> secondKeyLine
              <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\",\"label\":\"Salary\",\"origin\":\"" <> mainOrigin <> "\"}\n"
        liftIO $ recordsSorted shown `shouldBe` ([[paymentTxid], []], [])

    it "shows a coin with the lock the file gives it" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf (mainKeyLine <> "{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"origin\":\"" <> mainOrigin <> "\",\"spendable\":false}\n")
        liftIO $ map snd (shownAccounts shown) `shouldBe` [[(record OutputRecord (paymentTxid <> ":0")) {recordOrigin = Just mainOrigin, recordSpendable = Just False}]]

    it "leaves aside a coin with a label alone, which hw does not keep" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf (mainKeyLine <> "{\"type\":\"output\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (received)\"}\n")
        liftIO $ (recordsSorted shown, shownNotKept shown) `shouldBe` (([[]], []), 1)

    it "counts the label of a change address apart, by its key path, and does not show it" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf (mainKeyLine <> "{\"type\":\"addr\",\"ref\":\"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\",\"label\":\"Change\",\"keypath\":\"/1/0\"}\n")
        liftIO $ (recordsSorted shown, shownChangeLabels shown) `shouldBe` (([[]], []), 1)

    it "passes over a transaction without a label, which sets nothing" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf (mainKeyLine <> "{\"type\":\"tx\",\"ref\":\"" <> paymentTxid <> "\"}\n")
        liftIO $ (recordsSorted shown, shownNotKept shown, shownUnreadable shown) `shouldBe` (([[]], []), 0, [])

    -- Line 1 names no transaction, line 4 is of a type the BIP does not
    -- define, line 5 is not JSON, and line 6 has no reference
    it "leaves aside what hw keeps no label for, and numbers the lines that are not records in their order" $
      inDatabaseE $ do
        shown <-
          shownOf $
            Text.unlines
              [ "{\"type\":\"tx\",\"ref\":\"f91d0a8a\",\"label\":\"Salary\"}",
                "{\"type\":\"input\",\"ref\":\"" <> paymentTxid <> ":0\",\"label\":\"Salary (input)\"}",
                "{\"type\":\"spscan\",\"ref\":\"spscan1q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zs2pg9q5zsq9q6qjevn2kmdrnpuxt0v6h2kr2a2epkr0g6nk55ftf0xcxtddazgkrth3e\",\"label\":\"Scan\"}",
                "{\"type\":\"future\",\"ref\":{\"not\":\"validated\"},\"label\":\"Unknown\"}",
                "not a record",
                "{\"type\":\"addr\",\"label\":\"Salary\"}"
              ]
        liftIO $ (shownNotKept shown, shownUnreadable shown) `shouldBe` (3, [1, 5, 6])

    it "gives the tags a key record names, which nothing has, each with its colour and with whether it locks its coins" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf taggedKeyLine
        liftIO $
          shownTagsNotUsed shown
            `shouldBe` [FileTag (ShownTag coldTag Nothing) True, FileTag (ShownTag kycTag (Just TagRed)) False, FileTag (ShownTag rentTag Nothing) False]

    it "gives no such tag for a file whose keys name none" $
      inDatabaseE $ do
        _ <- testPortfolio
        shown <- shownOf testWalletFile
        liftIO $ shownTagsNotUsed shown `shouldBe` []

    -- As --json writes one of them
    it "writes such a tag with its name, its colour and whether it locks its coins" $
      Just (Json.toJSON (FileTag (ShownTag coldTag (Just TagBlue)) True))
        `shouldBe` Json.decode "{\"name\":\"cold\",\"color\":\"blue\",\"lockscoins\":true}"

  describe "A file narrowed to one portfolio, as show shows it" $ do
    -- importwallet reads these tags whatever it is narrowed to: the
    -- record is of portfolio 0, and the view is of portfolio 1
    it "still gives the tags that nothing has" $
      inDatabaseE $ do
        _ <- testPortfolio
        second <- liftIO secondPortfolioFile
        shown <- fileShown (ImportScope Nothing (Just 1)) (readWalletFile (taggedKeyLine <> second))
        liftIO $ (fileTag <$> shownTagsNotUsed shown) `shouldBe` [ShownTag coldTag Nothing, ShownTag kycTag (Just TagRed), ShownTag rentTag Nothing]

    -- What importwallet --portfolio 1 would make of 'twoPortfoliosFile',
    -- in a wallet that has portfolio 0
    it "shows the accounts of that portfolio alone" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        shown <- fileShown portfolioOne (readWalletFile content)
        liftIO $ do
          shownWallets shown `shouldBe` [WalletImport 0 testFingerprint False [PortfolioImport 1 True]]
          map fst (shownAccounts shown) `shouldBe` map AccountAdded (twelveAccounts 0 1)

    -- The label of the address of portfolio 0 is left out, and the label
    -- of the transaction of portfolio 1 is shown under its bip44 account
    -- on btc, the first of the twelve accounts of the portfolio
    it "counts the labels of the other portfolios, and shows those of that portfolio" $
      inDatabaseE $ do
        _ <- testPortfolio
        content <- liftIO twoPortfoliosFile
        shown <- fileShown portfolioOne (readWalletFile content)
        liftIO $ (recordsSorted shown, shownOtherPortfolios shown) `shouldBe` (([paymentTxid] : replicate 11 [], []), 1)

    it "is refused for a portfolio the file has nothing of, as importwallet refuses it" $
      inDatabaseE $ do
        content <- liftIO twoPortfoliosFile
        lift $
          dbShouldFailWith "The file has no portfolio2: it has portfolio0 and portfolio1" $
            fileShown (ImportScope Nothing (Just 2)) (readWalletFile content)

    it "counts no label of another portfolio in a file shown whole" $
      inDatabaseE $ do
        content <- liftIO twoPortfoliosFile
        shown <- shownOf content
        liftIO $ shownOtherPortfolios shown `shouldBe` 0

  describe "A file" $ do
    it "holds nothing of a wallet when no line is a record" $
      holdsAnything (readWalletFile "Buy milk\nsnow\n") `shouldBe` False

    -- What importacc took before portfolios, and importwallet refuses: a
    -- key is one account, which does not fit a portfolio
    it "holds nothing of a wallet when it is a key alone or a descriptor alone" $ do
      holdsAnything (readWalletFile (testAccountXPub <> "\n")) `shouldBe` False
      holdsAnything (readWalletFile ("pkh([bc560f95/44h/0h/0h]" <> testAccountXPub <> "/<0;1>/*)#vq597j87\n")) `shouldBe` False

    -- The document exportacc wrote: one JSON object over several lines,
    -- none of which is a record
    it "holds nothing of a wallet when it is the account document of an earlier hw" $
      holdsAnything (readWalletFile "{\n  \"xpubkey\": \"xpub6CHK45s4QJWpodaMQQBtCt5KUhCdBBb7Xj4pGtZG8x6HFeCp7W9ZtZdZaxA34YtFAhuebiKqLqHLYoB8HDadGutW8kEH4HeMdeS1KJz8Uah\",\n  \"network\": \"btc\",\n  \"name\": \"main\",\n  \"fingerprint\": \"bc560f95\"\n}\n")
        `shouldBe` False

    it "holds something of a wallet with a record of a type the BIP does not define" $
      holdsAnything (readWalletFile "{\"type\":\"future\",\"ref\":\"a\"}\n") `shouldBe` True

    it "holds the key of each account once, though the file has it twice" $
      map recordRef (fileKeys (readWalletFile (mainKeyLine <> segwitKeyLine <> mainKeyLine))) `shouldBe` [testAccountXPub, testSegwitAccountXPub]

{- The report in JSON -}

reportJsonSpec :: Spec
reportJsonSpec =
  describe "The report of importwallet in JSON" $ do
    -- The answer of importwallet --json for a file that made wallet0 with
    -- its portfolio 0, gave both their descriptions, left a key aside, set
    -- the label of an address and left one out
    it "writes what became of each wallet, each portfolio and each account, then the counts" $
      Json.object (reportPairs report)
        `shouldBe` fromMaybe
          (error "The expected JSON does not read")
          ( Json.decode
              "{\"wallets\":[{\"wallet\":0,\"fingerprint\":\"bc560f95\",\"created\":true,\"portfolios\":[{\"portfolio\":0,\"created\":true}]}],\
              \\"accounts\":[{\"status\":\"added\",\"account\":{\"wallet\":0,\"portfolio\":0,\"network\":\"btc\",\"type\":\"bip44\",\"derivation\":\"/44'/0'/0'\"}},\
              \{\"status\":\"leftaside\",\"name\":\"xpub6CQF8An9...\",\"reason\":\"Unknown network nowhere\"}],\
              \\"walletlabels\":1,\"portfoliolabels\":1,\
              \\"addresslabels\":1,\"txlabels\":0,\"addresstags\":0,\"txtags\":0,\"contacts\":0,\"contactsown\":0,\"coinslocked\":0,\"coinsunlocked\":0,\"addressesmissing\":0,\"addressestoofar\":0,\"txsmissing\":0,\"coinsmissing\":0,\
              \\"otheraccounts\":0,\"otherportfolios\":1,\"walletlabelskept\":0,\"portfoliolabelskept\":0,\"changeaddresses\":0,\"notkept\":0,\"unreadable\":[]}"
          )

    it "writes an account the wallet had already as held" $
      Json.object (reportPairs nothingDone {reportAccounts = [AccountHeld (accountAt 0 1 bch Bip44)]})
        `shouldBe` fromMaybe
          (error "The expected JSON does not read")
          ( Json.decode
              "{\"wallets\":[],\"accounts\":[{\"status\":\"held\",\"account\":{\"wallet\":0,\"portfolio\":1,\"network\":\"bch\",\"type\":\"bip44\",\"derivation\":\"/44'/145'/1'\"}}],\
              \\"walletlabels\":0,\"portfoliolabels\":0,\
              \\"addresslabels\":0,\"txlabels\":0,\"addresstags\":0,\"txtags\":0,\"contacts\":0,\"contactsown\":0,\"coinslocked\":0,\"coinsunlocked\":0,\"addressesmissing\":0,\"addressestoofar\":0,\"txsmissing\":0,\"coinsmissing\":0,\
              \\"otheraccounts\":0,\"otherportfolios\":0,\"walletlabelskept\":0,\"portfoliolabelskept\":0,\"changeaddresses\":0,\"notkept\":0,\"unreadable\":[]}"
          )
  where
    report =
      nothingDone
        { reportWallets = [WalletImport 0 testFingerprint True [PortfolioImport 0 True]],
          reportAccounts = [AccountAdded (accountAt 0 0 btc Bip44), AccountLeftAside "xpub6CQF8An9..." "Unknown network nowhere"],
          reportWalletLabels = 1,
          reportPortfolioLabels = 1,
          reportAddressLabels = 1,
          reportOtherPortfolios = 1
        }
