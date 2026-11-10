{-# LANGUAGE OverloadedStrings #-}

-- | The migration of hw around its steps: new databases, databases already at
-- the latest version, databases of a newer hw, and the steps run one after
-- the other, after one backup. Each step is tested on its own in
-- "Haskoin.Wallet.Migration.V2Spec", "Haskoin.Wallet.Migration.V3Spec" and
-- "Haskoin.Wallet.Migration.V4Spec", from the version before it to its
-- version; the copy of the database is tested in "Haskoin.Wallet.BackupSpec".
--
-- The whole way is tested here, from the databases of the two versions of
-- hw that were released before portfolios, hw 0.8 and hw 0.9: what they
-- become, and what the commands that hold a mnemonic make of them then.
-- These tests read the database with the live code, since they test what
-- this hw sees.
module Haskoin.Wallet.MigrationSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (sort)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Word (Word64)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin (Ctx, prepareContext)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (Adopted (..), adoptSeed, chooseAccount, fillWallet, getAccounts, nextPortfolioIndex)
import Haskoin.Wallet.Database.Portfolio (Portfolio (..), PortfolioChoice (..), choosePortfolio, getAllPortfolios, getWallets, walletAt)
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.Migration
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V1 as V1
import qualified Haskoin.Wallet.Schema.V2 as V2
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "The migration of hw" $ do
    it "sees a database without tables as having no version" $
      inMemory $
        runExceptT databaseVersion `dbShouldBe` Right Nothing

    it "creates the tables of a new database at the latest version, without a backup" $
      inMemory $ do
        migrateAndCollect ctx `dbShouldBe` (Right (), [], [])
        storedVersions `dbShouldBe` [(PersistInt64 (fromIntegral latestDatabaseVersion), "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just latestDatabaseVersion)
        expected <- liftIO currentSchema
        databaseSchema `dbShouldBe` expected

    it "leaves a database at the latest version alone" $
      inMemory $ do
        createTables migrateAll
        P.insert_ $ DBVersion latestDatabaseVersion
        P.insert_ $ DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105 Nothing
        commitFixture
        original <- (,) <$> databaseSchema <*> tablesRows ["d_b_version", "d_b_best"]
        migrateAndCollect ctx `dbShouldBe` (Right (), [], [])
        ((,) <$> databaseSchema <*> tablesRows ["d_b_version", "d_b_best"]) `dbShouldBe` original

    it "backs up the database once, then runs the steps one after the other" $
      inMemory $ do
        createTables V1.migrateAll
        commitFixture
        migrateToAndCollect 3 ctx
          `dbShouldBe` ( Right (),
                         [ "Migrating the database to version 2: the transaction summaries of hw 0.9",
                           "Migrating the database to version 3: the version stored as a number"
                         ],
                         [1]
                       )
        storedVersions `dbShouldBe` [(PersistInt64 3, "integer")]

    it "refuses a database of a newer hw" $
      inMemory $ do
        createTables migrateAll
        P.insert_ $ DBVersion newer
        commitFixture
        migrateAndCollect ctx
          `dbShouldBe` ( Left $
                           "The database is at version "
                             <> show newer
                             <> ", newer than the version "
                             <> show latestDatabaseVersion
                             <> " of this hw",
                         [],
                         []
                       )
        storedVersions `dbShouldBe` [(PersistInt64 (fromIntegral newer), "integer")]
  forM_ [hw08, hw09] (wholeWaySpec ctx)
  where
    newer = latestDatabaseVersion + 1

{- The whole way -}

-- | A database of a released hw, how to build it, and what the migration
-- does with it outside the database, in order
data Released = Released
  { releasedHw :: !String,
    releasedDatabase :: !(DB IO ()),
    releasedLog :: ![String]
  }

hw08, hw09 :: Released
hw08 =
  Released
    "hw 0.8"
    hw08Database
    [ "Backup at version 1",
      "Migrating the database to version 2: the transaction summaries of hw 0.9",
      "Migrating the database to version 3: the version stored as a number",
      "Migrating the database to version 4: the accounts filed under portfolios and wallets",
      partialPortfoliosNotice
    ]
hw09 =
  Released
    "hw 0.9"
    hw09Database
    [ "Backup at version 2",
      "Migrating the database to version 3: the version stored as a number",
      "Migrating the database to version 4: the accounts filed under portfolios and wallets",
      partialPortfoliosNotice
    ]

-- | The database of a released hw upgraded the whole way, then taken over
-- by the seed of the test mnemonic and filled in from it, as fillwallet
-- does. Both databases hold the same accounts and rows ('releasedAccounts'),
-- so they must end the same.
wholeWaySpec :: Ctx -> Released -> Spec
wholeWaySpec ctx released =
  describe ("A database of " <> releasedHw released <> " with accounts of two mnemonics on btc and bch") $ do
    it "is backed up once, then goes through every step to the latest version, the notice last" $
      inMemory $ do
        releasedDatabase released
        migrateToAndLog latestDatabaseVersion ctx `dbShouldBe` (Right (), releasedLog released)

    it "ends with the tables and the indexes of a new database of this hw" $
      inMemory $ do
        upgraded
        expected <- liftIO currentSchema
        databaseSchema `dbShouldBe` expected

    -- A wallet of hw 0.8 and hw 0.9 was a mnemonic on a coin type. The
    -- oldest account of each: cash, main, other, tips.
    it "gets a wallet for each mnemonic and coin type, numbered by its oldest account, without a fingerprint" $
      inMemory $ do
        upgraded
        walletsHeld
          `dbShouldBe` [ (0, Nothing, november1),
                         (1, Nothing, november2),
                         (2, Nothing, november3),
                         (3, Nothing, november5)
                       ]

    -- wallet0 and wallet3 have no account at index 0: their portfolio0 is
    -- empty, and bears the date of the wallet
    it "gets a portfolio for each index a wallet uses and a portfolio0, described by the names of their accounts" $
      inMemory $ do
        upgraded
        portfoliosHeld
          `dbShouldBe` [ (0, 0, "", november1),
                         (0, 2, "cash", november1),
                         (1, 0, "main", november2),
                         (1, 2, "spending", november4),
                         (2, 0, "other", november3),
                         (3, 0, "", november5),
                         (3, 1, "tips", november5)
                       ]

    it "keeps each account as a bip44 account of its portfolio, and gets no other" $
      inMemory $ do
        upgraded
        accountsHeld
          `dbShouldBe` [ (0, 2, "bch", 44, cashXPub),
                         (1, 0, "btc", 44, testAccountXPub),
                         (1, 2, "btc", 44, spendingXPub),
                         (2, 0, "btc", 44, testAccountXPub2),
                         (3, 1, "bch", 44, tipsXPub)
                       ]

    it "files every row under its new key" $
      inMemory $ do
        upgraded
        rowsHeld
          `dbShouldBe` [ [[PersistInt64 1, PersistInt64 0, PersistText "btc", PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", PersistText "salary"]],
                         [[PersistInt64 1, PersistInt64 0, PersistText "btc", PersistByteString creditOutpointBytes, PersistInt64 50000, PersistInt64 0]],
                         [[PersistInt64 1, PersistInt64 0, PersistText "btc", PersistByteString (hexBytes creditTxid), PersistInt64 50000]],
                         [[PersistInt64 1, PersistInt64 2, PersistText "btc", PersistByteString (hexBytes pendingNosigHash)]],
                         [[PersistByteString (hexBytes creditTxid), PersistByteString "the raw transaction"]],
                         [[PersistText "btc", PersistText bestBlock, PersistInt64 800105, PersistNull]]
                       ]

    -- wallet1 has portfolio0 and portfolio2
    it "leaves index 1 free for the next portfolio of a wallet whose accounts were at 0 and 2" $
      inMemory $ do
        upgraded
        Just (walletId, _) <- walletAt 1
        nextPortfolioIndex (testSeed ctx) walletId `dbShouldBe` 1

    it "refuses receive of an account a partial portfolio lacks, saying how to fill it in" $
      inMemory $ do
        upgraded
        dbShouldFailWith
          "portfolio0 of wallet1 has no bip84 account on btc yet. It is a partial portfolio, which lacks accounts. \
          \Fill it in with fillwallet on the computer that has the mnemonic, \
          \or with importwallet and a file of the whole wallet that exportwallet wrote there."
          $ do
            portfolio <- choosePortfolio (configWith []) (PortfolioChoice (Just 1) Nothing)
            chooseAccount (configWith []) portfolio Nothing (Just Bip84)

    -- bc560f95 is the fingerprint of the master key of the test mnemonic
    it "merges the two wallets of a mnemonic into the one of the lower index, with the fingerprint, once given its seed" $
      inMemory $ do
        upgraded
        adopted <- runExceptT (adoptSeed ctx (testSeed ctx))
        liftIO $ (fmap adoptedSummary <$> adopted) `shouldBe` Right (Just (0, Just "bc560f95", [1]))
        walletsHeld `dbShouldBe` [(0, Just "bc560f95", november1), (2, Nothing, november3), (3, Nothing, november5)]

    -- The empty portfolio0 of wallet0 gives no date: main does, of 2 Nov.
    -- portfolio2 takes the older of cash, of 1 Nov, and spending.
    it "merges the portfolios of the two wallets, their descriptions joined, dated by those that had accounts" $
      inMemory $ do
        upgraded
        _ <- adoptTestSeed ctx
        portfoliosHeld
          `dbShouldBe` [ (0, 0, "main", november2),
                         (0, 2, "cash, spending", november1),
                         (2, 0, "other", november3),
                         (3, 0, "", november5),
                         (3, 1, "tips", november5)
                       ]

    it "keeps the rows of the merged wallet, in the wallet that stays" $
      inMemory $ do
        upgraded
        _ <- adoptTestSeed ctx
        rowsHeld
          `dbShouldBe` [ [[PersistInt64 0, PersistInt64 0, PersistText "btc", PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", PersistText "salary"]],
                         [[PersistInt64 0, PersistInt64 0, PersistText "btc", PersistByteString creditOutpointBytes, PersistInt64 50000, PersistInt64 0]],
                         [[PersistInt64 0, PersistInt64 0, PersistText "btc", PersistByteString (hexBytes creditTxid), PersistInt64 50000]],
                         [[PersistInt64 0, PersistInt64 2, PersistText "btc", PersistByteString (hexBytes pendingNosigHash)]],
                         [[PersistByteString (hexBytes creditTxid), PersistByteString "the raw transaction"]],
                         [[PersistText "btc", PersistText bestBlock, PersistInt64 800105, PersistNull]]
                       ]

    -- portfolio0 had 1 account and lacked 11, portfolio2 had 2 and lacked
    -- 10: 11 + 10 = 21 accounts added
    it "fills in every portfolio of the merged wallet, to its twelve accounts" $
      inMemory $ do
        upgraded
        wallet <- adoptTestSeed ctx
        runExceptT (fillWallet ctx (testSeed ctx) wallet) `dbShouldBe` Right 21
        accounts <- accountsHeld
        liftIO $
          sort [(portfolio, net, purpose) | (0, portfolio, net, purpose, _) <- accounts]
            `shouldBe` [(portfolio, net, purpose) | portfolio <- [0, 2], (net, purpose) <- twelveAccounts]

    it "leaves the wallets of the other mnemonic partial when it fills in the merged one" $
      inMemory $ do
        upgraded
        wallet <- adoptTestSeed ctx
        _ <- runExceptT (fillWallet ctx (testSeed ctx) wallet)
        accounts <- accountsHeld
        liftIO $
          [(index, portfolio, net) | (index, portfolio, net, _, _) <- accounts, index /= 0]
            `shouldBe` [(2, 0, "btc"), (3, 1, "bch")]
  where
    upgraded = do
      releasedDatabase released
      migrateToAndLog latestDatabaseVersion ctx `dbShouldBe` (Right (), releasedLog released)

-- | Give the database the seed of the test mnemonic, as the commands that
-- hold a mnemonic do first, and return its wallet
adoptTestSeed :: Ctx -> DB IO (DBWalletId, DBWallet)
adoptTestSeed ctx = do
  adopted <- liftIO . expectRight =<< runExceptT (adoptSeed ctx (testSeed ctx))
  case adopted of
    Just found -> return (adoptedWallet found)
    Nothing -> liftIO (expectationFailure "adoptSeed found no wallet of the test seed") >> fail "adoptSeed"

-- | What a takeover by a seed tells: the index of the wallet of the seed,
-- its fingerprint, and the indexes of the wallets merged into it
adoptedSummary :: Adopted -> (Int, Maybe Text, [Natural])
adoptedSummary found =
  (dBWalletIndex (snd (adoptedWallet found)), dBWalletFingerprint (snd (adoptedWallet found)), adoptedMerged found)

{- What the database holds, as this hw reads it -}

-- | Each wallet: its index, its fingerprint and the date it was created
walletsHeld :: DB IO [(Int, Maybe Text, UTCTime)]
walletsHeld = map (\(_, w) -> (dBWalletIndex w, dBWalletFingerprint w, dBWalletCreated w)) <$> getWallets

-- | Each portfolio: the index of its wallet, its own, its description and
-- the date it was created
portfoliosHeld :: DB IO [(Int, Int, Text, UTCTime)]
portfoliosHeld = map held <$> getAllPortfolios
  where
    held p =
      ( dBWalletIndex (portfolioWallet p),
        dBPortfolioIndex (portfolioRow p),
        dBPortfolioDescription (portfolioRow p),
        dBPortfolioCreated (portfolioRow p)
      )

-- | Each account: the index of its wallet and of its portfolio, its
-- network, its purpose and its key, in that order
accountsHeld :: DB IO [(Int, Int, Text, Int, Text)]
accountsHeld = sort . map held <$> getAccounts
  where
    held (p, _, a) =
      ( dBWalletIndex (portfolioWallet p),
        dBPortfolioIndex (portfolioRow p),
        dBAccountNetwork a,
        dBAccountPurpose a,
        dBAccountXPubKey a
      )

-- | The rows other than wallets, portfolios and accounts, each table on
-- its own, a place written as the index of the wallet, that of the
-- portfolio and the network: the addresses, with their address and label;
-- the coins, with their outpoint, value and lock of the user; the
-- summaries, with their transaction id and amount; the pending
-- transactions, with their nosigHash; the raw transactions; the best
-- blocks
rowsHeld :: DB IO [[[PersistValue]]]
rowsHeld =
  mapM
    queryRows
    [ "select w.\"index\", p.\"index\", a.network, d.address, d.label from d_b_address d " <> inAccount "d.account",
      "select w.\"index\", p.\"index\", a.network, c.outpoint, c.value, c.locked_by_user from d_b_coin c " <> inAccount "c.account",
      "select w.\"index\", p.\"index\", t.network, t.txid, t.amount from d_b_tx_info t " <> inPortfolio "t.portfolio",
      "select w.\"index\", p.\"index\", t.network, t.nosig_hash from d_b_pending_tx t " <> inPortfolio "t.portfolio",
      "select hash, blob from d_b_raw_tx",
      "select network, best_block, best_height, best_time from d_b_best"
    ]
  where
    inPortfolio column = "join d_b_portfolio p on p.id = " <> column <> " join d_b_wallet w on w.id = p.wallet"
    inAccount column = "join d_b_account a on a.id = " <> column <> " " <> inPortfolio "a.portfolio"

-- | The accounts of a whole portfolio, by network and purpose, in the
-- order of the names of the networks: bip44 on every network, bip84 on
-- those of Bitcoin
twelveAccounts :: [(Text, Int)]
twelveAccounts =
  [ ("bch", 44),
    ("bchreg", 44),
    ("bchtest", 44),
    ("bchtest4", 44),
    ("btc", 44),
    ("btc", 84),
    ("btcreg", 44),
    ("btcreg", 84),
    ("btctest", 44),
    ("btctest", 84),
    ("btctest4", 44),
    ("btctest4", 84)
  ]

{- The databases of hw 0.8 and hw 0.9 -}

-- | An account as hw 0.8 and hw 0.9 stored it
data ReleasedAccount
  = ReleasedAccount
      -- | Its name
      !Text
      -- | The wallet it was filed under
      !Text
      -- | Its index
      !Int
      -- | Its network
      !Text
      -- | Its path
      !Text
      -- | Its key
      !Text
      -- | How many receiving addresses it gave out, what it received, and
      -- in how many coins
      !(Int, Word64, Word64)
      -- | When it was created
      !UTCTime

-- | The accounts of the two databases, in the order they were created.
-- The test mnemonic has main at index 0 and spending at index 2 on
-- Bitcoin, and cash at index 2 on Bitcoin Cash, created first. The second
-- mnemonic has other at index 0 on Bitcoin and tips at index 1 on Bitcoin
-- Cash. hw 0.8 and hw 0.9 filed each account under the fingerprint of the
-- first account of its mnemonic on its coin type, m/44'/coin'/0':
-- e5c9e2d7 and 892eb8e4 for the test mnemonic on Bitcoin Cash and on
-- Bitcoin, 807a5cfb and 38ceb035 for the second one. The keys and the
-- fingerprints were derived apart from hw, with the Python tool of
-- test/data/message-signing.
releasedAccounts :: [ReleasedAccount]
releasedAccounts =
  [ ReleasedAccount "cash" "e5c9e2d7" 2 "bch" "/44'/145'/2'" cashXPub (0, 0, 0) november1,
    ReleasedAccount "main" "892eb8e4" 0 "btc" "/44'/0'/0'" testAccountXPub (1, 50000, 1) november2,
    ReleasedAccount "other" "807a5cfb" 0 "btc" "/44'/0'/0'" testAccountXPub2 (0, 0, 0) november3,
    ReleasedAccount "spending" "892eb8e4" 2 "btc" "/44'/0'/2'" spendingXPub (0, 0, 0) november4,
    ReleasedAccount "tips" "38ceb035" 1 "bch" "/44'/145'/1'" tipsXPub (0, 0, 0) november5
  ]

-- | m/44'/145'/2' of the test mnemonic, m/44'/0'/2' of the test mnemonic,
-- and m/44'/145'/1' of the second mnemonic
cashXPub, spendingXPub, tipsXPub :: Text
cashXPub = "xpub6DXWb36CzdZvaF343t8PgVoboc2i49wtRH4Z7XsGPA9TjmAfogXu51Z89T8L6u4kRhLYdPWBV5MwqhzenDr9s6dq6gYZUahjCWHPHTwk96T"
spendingXPub = "xpub6CHK45s4QJWpvPoXu7KKatJo3k9XaEXauS9DMA3k4FAkzJDSFhQKMTQgqwXLCGAoeS8DPczpK68sPZRseu6ey1Qm2NnHZfBnvRXqo8xM2oV"
tipsXPub = "xpub6CTA26YudZuxrwibrfPeyRsNe1PhS8w5TC8AbAVakZKrJ169AgdsWX1PfisCR53YyJRFw3NpJgVFg3CmL1uMXGxctVgy8px8ydrqsb28bTZ"

-- | The database of hw 0.8 with the accounts and these rows of main: its
-- receiving address /0/0, labelled and paid 50000 by the credit, the
-- summary of the credit as hw 0.8 wrote it, and its coin; a pending
-- transaction of spending; the raw credit; and the best block of
-- Bitcoin. hw 0.8 had no version table.
hw08Database :: DB IO ()
hw08Database = do
  createTables V1.migrateAll
  forM_ releasedWallets $ P.insert_ . V1.DBWallet
  forM_ releasedAccounts $ \(ReleasedAccount name wallet index net path key (external, received, coins) created) ->
    P.insert_ $ V1.DBAccount name (V1.DBWalletKey wallet) index net path external 0 key received 0 coins created
  P.insert_ $ V1.DBAddress 0 mainWallet1 "/44'/0'/0'" "/0/0" "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "salary" 50000 0 1 1 50000 False False november2
  P.insert_ $ V1.DBTxInfo mainWallet1 "/44'/0'/0'" creditTxid creditBlock True creditBlobOfHw08 november2
  P.insert_ $ V1.DBCoin mainWallet1 "/44'/0'/0'" creditOutpointText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 creditBlock "the coin" True True november2
  P.insert_ $ V1.DBPendingTx (V1.DBWalletKey "892eb8e4") "/44'/0'/2'" pendingNosigHash "the signing data of a transaction" False november4
  P.insert_ $ V1.DBRawTx creditTxid "the raw transaction"
  P.insert_ $ V1.DBBest "btc" bestBlock 800105
  commitFixture
  where
    mainWallet1 = V1.DBWalletKey "892eb8e4"

-- | The database of hw 0.9 with the same accounts and rows, its summary
-- as hw 0.9 wrote it, and the version label of hw 0.9
hw09Database :: DB IO ()
hw09Database = do
  createTables V2.migrateAll
  P.insert_ $ V2.DBVersion "0.9"
  forM_ releasedWallets $ P.insert_ . V2.DBWallet
  forM_ releasedAccounts $ \(ReleasedAccount name wallet index net path key (external, received, coins) created) ->
    P.insert_ $ V2.DBAccount name (V2.DBWalletKey wallet) index net path external 0 key received 0 coins created
  P.insert_ $ V2.DBAddress 0 mainWallet2 "/44'/0'/0'" "/0/0" "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "salary" 50000 0 1 1 50000 False False november2
  P.insert_ $ V2.DBTxInfo mainWallet2 "/44'/0'/0'" creditTxid creditBlock True creditBlobOfHw09 november2
  P.insert_ $ V2.DBCoin mainWallet2 "/44'/0'/0'" creditOutpointText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 creditBlock "the coin" True True november2
  P.insert_ $ V2.DBPendingTx (V2.DBWalletKey "892eb8e4") "/44'/0'/2'" pendingNosigHash "the signing data of a transaction" False november4
  P.insert_ $ V2.DBRawTx creditTxid "the raw transaction"
  P.insert_ $ V2.DBBest "btc" bestBlock 800105
  commitFixture
  where
    mainWallet2 = V2.DBWalletKey "892eb8e4"

-- | The wallets of the accounts, each once
releasedWallets :: [Text]
releasedWallets = ["e5c9e2d7", "892eb8e4", "807a5cfb", "38ceb035"]

-- | The credit of main: its id, as hw wrote it, and its block, 800000 at
-- position 3
creditTxid :: Text
creditTxid = "0000000000000000000000000000000000000000000000000000000000000011"

creditBlock :: ByteString
creditBlock = S.encode (Store.BlockRef 800000 3)

-- | The coin of the credit, its output 0, as hw wrote it: the hexadecimal
-- text of its 36 bytes, the hash in its own order, so the byte 11 first,
-- then the index 0 in four bytes
creditOutpointText :: Text
creditOutpointText = "1100000000000000000000000000000000000000000000000000000000000000" <> "00000000"

creditOutpointBytes :: ByteString
creditOutpointBytes = hexBytes creditOutpointText

-- | The nosigHash of the pending transaction of spending
pendingNosigHash :: Text
pendingNosigHash = "0000000000000000000000000000000000000000000000000000000000000066"

bestBlock :: Text
bestBlock = "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054"

-- | The credit as hw 0.8 wrote it: 50000 received on /0/0 of main, out of
-- an input of 100000 of somebody else
creditBlobOfHw08 :: ByteString
creditBlobOfHw08 =
  "{\"amount\":50000,\"block\":{\"height\":800000,\"position\":3},\"confirmations\":12,\"fee\":1000,\"feebyte\":4,\
  \\"myinputs\":{},\"myoutputs\":{\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\":{\"path\":\"/0/0\",\"value\":50000}},\
  \\"nonstdinputs\":[],\"nonstdoutputs\":[],\"otherinputs\":{\"1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ\":{\"value\":100000}},\
  \\"otheroutputs\":{\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\":49000},\"size\":226,\
  \\"txid\":\"0000000000000000000000000000000000000000000000000000000000000011\",\"type\":\"credit\"}"

-- | The same credit as hw 0.9 wrote it, with a label for the output and
-- the pending state
creditBlobOfHw09 :: ByteString
creditBlobOfHw09 =
  "{\"amount\":50000,\"block\":{\"height\":800000,\"position\":3},\"confirmations\":12,\"fee\":1000,\"feebyte\":4,\
  \\"myinputs\":{},\"myoutputs\":{\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\":{\"label\":\"\",\"path\":\"/0/0\",\"value\":50000}},\
  \\"nonstdinputs\":[],\"nonstdoutputs\":[],\"otherinputs\":{\"1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ\":{\"value\":100000}},\
  \\"otheroutputs\":{\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\":49000},\"pending\":null,\"size\":226,\
  \\"txid\":\"0000000000000000000000000000000000000000000000000000000000000011\",\"type\":\"credit\"}"

november1, november2, november3, november4, november5 :: UTCTime
november1 = UTCTime (fromGregorian 2023 11 1) 0
november2 = UTCTime (fromGregorian 2023 11 2) 0
november3 = UTCTime (fromGregorian 2023 11 3) 0
november4 = UTCTime (fromGregorian 2023 11 4) 0
november5 = UTCTime (fromGregorian 2023 11 5) 0
