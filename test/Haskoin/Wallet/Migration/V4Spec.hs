{-# LANGUAGE OverloadedStrings #-}

-- | The migration to the format version 4: a database of version 3, built
-- from its tables ("Haskoin.Wallet.Schema.V3"), is migrated to version 4
-- and ends with its accounts filed under portfolios and wallets. A wallet
-- for each wallet of version 3, which is one mnemonic on one coin type,
-- numbered by its oldest account and without a fingerprint. A portfolio for
-- each account index a wallet uses, and a portfolio0, described by the
-- names of its accounts. The accounts as they were, bip44 ones, in
-- partial portfolios. Every other row under its new key, and the notice
-- that says how to fill the portfolios in.
--
-- The rows are read with SQL on the tables of version 4, and the expected
-- values are written out. A place in the new tables is written as the
-- index of the wallet, the index of the portfolio and the network.
module Haskoin.Wallet.Migration.V4Spec (spec) where

import Control.Exception (SomeException, try)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.List (isInfixOf)
import qualified Data.Serialize as S
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import qualified Database.Persist as P
import Database.Persist.Sql (PersistValue (..))
import Haskoin (Ctx, prepareContext)
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Schema.V3 as V3
import qualified Haskoin.Wallet.Schema.V4 as V4
import Haskoin.Wallet.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of version 3 to version 4" $ do
    versionSpec ctx
    walletsSpec ctx
    portfoliosSpec ctx
    accountsSpec ctx
    rowsSpec ctx
    noticeSpec ctx
    failureSpec ctx
    schemaSpec ctx

versionSpec :: Ctx -> Spec
versionSpec ctx =
  describe "The version" $ do
    it "of a database of version 3 reads as version 3" $
      inMemory $ do
        version3Database
        runExceptT databaseVersion `dbShouldBe` Right (Just 3)

    it "is stored as the number 4" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        storedVersions `dbShouldBe` [(PersistInt64 4, "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 4)

walletsSpec :: Ctx -> Spec
walletsSpec ctx =
  describe "The wallets" $ do
    it "hold the accounts of one mnemonic on one coin type together" $
      inMemory $ do
        version3With [mainAccount, savingsAccount]
        migrateTo4 ctx
        walletsOfKeys `dbShouldBe` [[PersistInt64 0, PersistText testAccountXPub], [PersistInt64 0, PersistText savingsXPub]]

    -- The test networks share the coin type 1: hw 0.9 filed the accounts of
    -- a mnemonic on all of them under one wallet
    it "hold the accounts of one mnemonic on two test networks together, since they share their coin type" $
      inMemory $ do
        version3With [testnetAccount, bchTestnetAccount]
        migrateTo4 ctx
        walletsOfKeys `dbShouldBe` [[PersistInt64 0, PersistText testAccountXPubTest], [PersistInt64 0, PersistText bchTestnetXPub]]

    -- hw 0.9 named a wallet by the first account of its mnemonic on a coin
    -- type, so it tells nothing of the accounts on another coin type
    it "keep the accounts of one mnemonic on Bitcoin and on Bitcoin Cash apart, as hw 0.9 filed them" $
      inMemory $ do
        version3With [mainAccount, cashAccount]
        migrateTo4 ctx
        walletsOfKeys `dbShouldBe` [[PersistInt64 0, PersistText testAccountXPub], [PersistInt64 1, PersistText testAccountXPubBch]]

    it "keep the accounts of different mnemonics apart, even at the same path" $
      inMemory $ do
        version3With [mainAccount, otherAccount]
        migrateTo4 ctx
        walletsOfKeys `dbShouldBe` [[PersistInt64 0, PersistText testAccountXPub], [PersistInt64 1, PersistText testAccountXPub2]]

    -- The ids of version 3 sort the other way: 807a5cfb, 892eb8e4, e5c9e2d7.
    -- And by their newest account, the Bitcoin Cash wallet (2 Nov) would
    -- come before the Bitcoin one (savings, 3 Nov).
    it "are numbered by their oldest account: main of 1 Nov, cash of 2 Nov, other of 4 Nov" $
      inMemory $ do
        version3With [otherAccount, savingsAccount, cashAccount, mainAccount]
        migrateTo4 ctx
        walletsOfKeys
          `dbShouldBe` [ [PersistInt64 0, PersistText testAccountXPub],
                         [PersistInt64 0, PersistText savingsXPub],
                         [PersistInt64 1, PersistText testAccountXPubBch],
                         [PersistInt64 2, PersistText testAccountXPub2]
                       ]

    -- All three are of 1 Nov. cash is on bch, which sorts before btc; other
    -- is at index 0 on btc, before savings at index 2.
    it "whose oldest accounts were created at the same moment are numbered by the network, then the index, of those accounts" $
      inMemory $ do
        version3With [atFixtureTime savingsAccount, atFixtureTime otherAccount, atFixtureTime cashAccount]
        migrateTo4 ctx
        walletsOfKeys
          `dbShouldBe` [ [PersistInt64 0, PersistText testAccountXPubBch],
                         [PersistInt64 1, PersistText testAccountXPub2],
                         [PersistInt64 2, PersistText savingsXPub]
                       ]

    it "are dated by their oldest account" $
      inMemory $ do
        version3With [otherAccount, savingsAccount, cashAccount, mainAccount]
        migrateTo4 ctx
        queryRows "select \"index\", created from d_b_wallet"
          `dbShouldBe` [ [PersistInt64 0, PersistText "2023-11-01T00:00:00Z"],
                         [PersistInt64 1, PersistText "2023-11-02T00:00:00Z"],
                         [PersistInt64 2, PersistText "2023-11-04T00:00:00Z"]
                       ]

    -- hw 0.9 kept the fingerprint of the first account of a mnemonic on a
    -- coin type, not that of its master key
    it "have no fingerprint" $
      inMemory $ do
        version3Database
        anotherSeed
        migrateTo4 ctx
        queryRows "select \"index\", fingerprint from d_b_wallet"
          `dbShouldBe` [[PersistInt64 0, PersistNull], [PersistInt64 1, PersistNull], [PersistInt64 2, PersistNull]]

portfoliosSpec :: Ctx -> Spec
portfoliosSpec ctx =
  describe "The portfolios" $ do
    it "are one for each account index a wallet uses, and no other: indexes 0 and 2 give portfolio0 and portfolio2" $
      inMemory $ do
        version3With [mainAccount, savingsAccount]
        migrateTo4 ctx
        queryRows "select w.\"index\", p.\"index\" from d_b_portfolio p join d_b_wallet w on w.id = p.wallet"
          `dbShouldBe` [[PersistInt64 0, PersistInt64 0], [PersistInt64 0, PersistInt64 2]]

    it "include an empty portfolio0 for a wallet without an account at index 0" $
      inMemory $ do
        version3With [savingsAccount]
        migrateTo4 ctx
        accountsPerPortfolio `dbShouldBe` [[PersistInt64 0, PersistInt64 0, PersistInt64 0], [PersistInt64 0, PersistInt64 2, PersistInt64 1]]

    -- The wallet's oldest account is savings, of 3 Nov; travel is of 5 Nov
    it "date the empty portfolio0 as its wallet, by the oldest account of the wallet" $
      inMemory $ do
        version3With [savingsAccount, travelAccount]
        migrateTo4 ctx
        portfolioDates
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "2023-11-03T00:00:00Z"],
                         [PersistInt64 0, PersistInt64 1, PersistText "2023-11-05T00:00:00Z"],
                         [PersistInt64 0, PersistInt64 2, PersistText "2023-11-03T00:00:00Z"]
                       ]

    it "are described by the name of their account" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        portfolioDescriptions
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "main"],
                         [PersistInt64 0, PersistInt64 2, PersistText "savings"],
                         [PersistInt64 1, PersistInt64 0, PersistText "cash"]
                       ]

    it "give the empty portfolio0 no description" $
      inMemory $ do
        version3With [savingsAccount]
        migrateTo4 ctx
        portfolioDescriptions `dbShouldBe` [[PersistInt64 0, PersistInt64 0, PersistText ""], [PersistInt64 0, PersistInt64 2, PersistText "savings"]]

    -- By their names or their networks, cash would come first
    it "are described by the names of all their accounts, joined with a comma, in the order they were created" $
      inMemory $ do
        version3With [cashUnderBtcWallet, mainAccount]
        migrateTo4 ctx
        portfolioDescriptions `dbShouldBe` [[PersistInt64 0, PersistInt64 0, PersistText "main, cash"]]

    it "are dated by their account" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        portfolioDates
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "2023-11-01T00:00:00Z"],
                         [PersistInt64 0, PersistInt64 2, PersistText "2023-11-03T00:00:00Z"],
                         [PersistInt64 1, PersistInt64 0, PersistText "2023-11-02T00:00:00Z"]
                       ]

    -- main is of 1 Nov, cash of 2 Nov, and cash is stored first
    it "are dated by the oldest of their accounts" $
      inMemory $ do
        version3With [cashUnderBtcWallet, mainAccount]
        migrateTo4 ctx
        portfolioDates `dbShouldBe` [[PersistInt64 0, PersistInt64 0, PersistText "2023-11-01T00:00:00Z"]]

accountsSpec :: Ctx -> Spec
accountsSpec ctx =
  describe "The accounts" $ do
    it "are each in the portfolio of its index, in the wallet of its mnemonic and coin type" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows ("select w.\"index\", p.\"index\", a.network, a.x_pub_key from d_b_account a " <> inPortfolio "a.portfolio")
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "btc", PersistText testAccountXPub],
                         [PersistInt64 0, PersistInt64 2, PersistText "btc", PersistText savingsXPub],
                         [PersistInt64 1, PersistInt64 0, PersistText "bch", PersistText testAccountXPubBch]
                       ]

    it "get the purpose 44: every account of version 3 is a bip44 one" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select x_pub_key, purpose from d_b_account"
          `dbShouldBe` [ [PersistText testAccountXPub, PersistInt64 44],
                         [PersistText savingsXPub, PersistInt64 44],
                         [PersistText testAccountXPubBch, PersistInt64 44]
                       ]

    -- A whole portfolio has twelve accounts. The others are behind hardened
    -- steps of the path, which a public account key cannot derive.
    it "are only those the database had: each portfolio is partial, with its one account" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        accountsPerPortfolio
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistInt64 1],
                         [PersistInt64 0, PersistInt64 2, PersistInt64 1],
                         [PersistInt64 1, PersistInt64 0, PersistInt64 1]
                       ]

    it "keep their other columns" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select x_pub_key, external, internal, balance_confirmed, balance_unconfirmed, balance_coins, created from d_b_account"
          `dbShouldBe` [ [PersistText testAccountXPub, PersistInt64 1, PersistInt64 1, PersistInt64 50000, PersistInt64 0, PersistInt64 1, PersistText "2023-11-01T00:00:00Z"],
                         [PersistText savingsXPub, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistText "2023-11-03T00:00:00Z"],
                         [PersistText testAccountXPubBch, PersistInt64 1, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistInt64 0, PersistText "2023-11-02T00:00:00Z"]
                       ]

rowsSpec :: Ctx -> Spec
rowsSpec ctx =
  describe "The other rows" $ do
    it "file the addresses under their account" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows ("select w.\"index\", p.\"index\", a.network, d.internal, d.\"index\", d.address from d_b_address d " <> inAccount "d.account")
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "btc", PersistInt64 0, PersistInt64 0, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                         [PersistInt64 0, PersistInt64 0, PersistText "btc", PersistInt64 1, PersistInt64 0, PersistText "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"],
                         [PersistInt64 1, PersistInt64 0, PersistText "bch", PersistInt64 0, PersistInt64 0, PersistText bchExternalAddrText]
                       ]

    -- The outpoint of version 3 is the hexadecimal text of its 36 bytes:
    -- the hash of the transaction, then its index, 0
    it "file the coins under their account, keyed by the bytes of their outpoint" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows ("select w.\"index\", p.\"index\", a.network, c.outpoint, c.address from d_b_coin c " <> inAccount "c.account")
          `dbShouldBe` [[PersistInt64 0, PersistInt64 0, PersistText "btc", PersistByteString outpoint1Bytes, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]]

    it "file the summaries under the portfolio and the network of their account, keyed by the bytes of the transaction id" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows ("select w.\"index\", p.\"index\", t.network, t.txid from d_b_tx_info t " <> inPortfolio "t.portfolio")
          `dbShouldBe` [[PersistInt64 0, PersistInt64 0, PersistText "btc", PersistByteString (hexBytes txid1)]]

    it "file the pending transactions under the portfolio and the network of their account, keyed by the bytes of their nosigHash" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows ("select w.\"index\", p.\"index\", t.network, t.nosig_hash from d_b_pending_tx t " <> inPortfolio "t.portfolio")
          `dbShouldBe` [[PersistInt64 0, PersistInt64 2, PersistText "btc", PersistByteString (hexBytes nosigHash5)]]

    it "key the raw transactions by the bytes of their hash, keeping them" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select hash, blob from d_b_raw_tx" `dbShouldBe` [[PersistByteString (hexBytes txid1), PersistByteString "raw"]]

    -- main and other are both m/44'/0'/0', of two mnemonics
    it "of two seeds whose accounts have the same path stay apart" $
      inMemory $ do
        version3Database
        anotherSeed
        migrateTo4 ctx
        queryRows ("select w.\"index\", p.\"index\", d.address from d_b_address d " <> inAccount "d.account" <> " where d.internal = 0 and a.network = 'btc'")
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                         [PersistInt64 2, PersistInt64 0, PersistText externalAddr2Text]
                       ]
        queryRows ("select w.\"index\", p.\"index\", t.txid from d_b_tx_info t " <> inPortfolio "t.portfolio")
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistByteString (hexBytes txid1)],
                         [PersistInt64 2, PersistInt64 0, PersistByteString (hexBytes txid2)]
                       ]
        queryRows ("select w.\"index\", p.\"index\", c.address from d_b_coin c " <> inAccount "c.account")
          `dbShouldBe` [ [PersistInt64 0, PersistInt64 0, PersistText "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
                         [PersistInt64 2, PersistInt64 0, PersistText externalAddr2Text]
                       ]

    it "keep their other columns, the free change address and the locked coin included" $
      inMemory $ do
        version3Database
        addressesBefore <- queryRows "select \"index\", address, label, balance_confirmed, balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"
        summariesBefore <- queryRows "select block_ref, blob, created from d_b_tx_info"
        coinsBefore <- queryRows "select address, value, block_ref, created from d_b_coin"
        pendingBefore <- queryRows "select blob, online, created from d_b_pending_tx"
        migrateTo4 ctx
        queryRows "select \"index\", address, label, balance_confirmed, balance_unconfirmed, balance_coins, balance_txs, balance_received, internal, created from d_b_address"
          `dbShouldBe` addressesBefore
        queryRows "select block_ref, blob, created from d_b_tx_info" `dbShouldBe` summariesBefore
        queryRows "select address, value, block_ref, created from d_b_coin" `dbShouldBe` coinsBefore
        queryRows "select blob, online, created from d_b_pending_tx" `dbShouldBe` pendingBefore

    -- Version 3 kept no time: the next sync fetches the transactions again
    it "give each summary the amount of its blob, what it spends below zero" $
      inMemory $ do
        version3Database
        P.insert_ $ V3.DBTxInfo (V3.DBWalletKey btcWallet) "/44'/0'/0'" txid3 (S.encode (Store.BlockRef 800001 1)) True spendingBlob fixtureTime
        commitFixture
        migrateTo4 ctx
        queryRows "select amount from d_b_tx_info" `dbShouldBe` [[PersistInt64 (-41000)], [PersistInt64 50000]]

    it "mark no summary dead: version 3 deleted the summaries of dead transactions" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select dead from d_b_tx_info" `dbShouldBe` [[PersistInt64 0]]

    it "give the summaries no time" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select time from d_b_tx_info" `dbShouldBe` [[PersistNull]]

    it "take no lock of version 3 for a lock of the user: those were the pending transactions' locks" $
      inMemory $ do
        version3Database
        queryRows "select locked from d_b_coin" `dbShouldBe` [[PersistInt64 1]]
        migrateTo4 ctx
        queryRows "select locked_by_user from d_b_coin" `dbShouldBe` [[PersistInt64 0]]

    it "start every pending transaction out of conflict, for the next sync to ask the store" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select conflicted from d_b_pending_tx" `dbShouldBe` [[PersistInt64 0]]

    -- Version 4 stores the time of the best block, which version 3 did
    -- not know: the next sync fills it
    it "leave the best blocks as they were, with no time until the next sync" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        queryRows "select network, best_block, best_height, best_time from d_b_best"
          `dbShouldBe` [[PersistText "btc", PersistText "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054", PersistInt64 800105, PersistNull]]

    it "include a table of labels of the transactions, empty: version 3 had none" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        columnNames "d_b_tx_label" `dbShouldBe` ["label", "network", "portfolio", "txid"]
        tableRows "d_b_tx_label" `dbShouldBe` []

    it "include no block a sync reached, for the first sync of each portfolio to look at every address: version 3 kept none" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        tablesRows ["d_b_synced_block"] `dbShouldBe` [[]]

noticeSpec :: Ctx -> Spec
noticeSpec ctx =
  describe "What the user is told" $ do
    -- The hooks of the migration only announce and back up: nothing in
    -- them could ask for a mnemonic
    it "is, without a question for the mnemonic, the step and then how to fill the portfolios in" $
      inMemory $ do
        version3Database
        migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced, partialPortfoliosNotice], [3])

    it "is the step alone for a database without accounts" $
      inMemory $ do
        version3With []
        migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced], [3])

failureSpec :: Ctx -> Spec
failureSpec ctx = do
  describe "The backup" $
    it "is taken at version 3, before the step" $
      inMemory $ do
        version3Database
        migrateToAndLog 4 ctx `dbShouldBe` (Right (), ["Backup at version 3", stepAnnounced, partialPortfoliosNotice])

  describe "A failure halfway" $
    it "changes nothing: the database stays at version 3, as it was" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "accounts.sqlite"
        inDatabaseFile file $ do
          version3Database
          -- A raw transaction under a key that is not hexadecimal, which no
          -- hw wrote: the step fails on it, the last of its copies, after it
          -- has set the old tables aside, made the wallets, the portfolios
          -- and the accounts, and filled the new tables
          P.insert_ $ V3.DBRawTx "not hexadecimal" "raw"
          commitFixture
        original <- inDatabaseFile file $ (,) <$> databaseSchema <*> tablesRows version3Tables
        result <- try (inDatabaseFile file (migrateToAndCollect 4 ctx)) :: IO (Either SomeException (Either String (), [String], [Int]))
        either show (const "no failure") result `shouldSatisfy` ("NOT NULL constraint failed: d_b_raw_tx.hash" `isInfixOf`)
        inDatabaseFile file ((,) <$> databaseSchema <*> tablesRows version3Tables) `shouldReturn` original
        inDatabaseFile file (runExceptT databaseVersion) `shouldReturn` Right (Just 3)

schemaSpec :: Ctx -> Spec
schemaSpec ctx =
  describe "The tables" $ do
    it "are those of version 4, empty, for a database without any account, as a new wallet of version 3 is" $
      inMemory $ do
        version3With []
        migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced], [3])
        tablesRows ["d_b_wallet", "d_b_portfolio", "d_b_account", "d_b_address", "d_b_tx_info", "d_b_tx_label", "d_b_coin", "d_b_raw_tx", "d_b_pending_tx", "d_b_synced_block"]
          `dbShouldBe` replicate 10 []
        expected <- liftIO $ schemaCreatedBy (createTables V4.migrateAll >> createIndexesOf V4.tableIndexes)
        databaseSchema `dbShouldBe` expected

    it "are those of version 4, with its indexes" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        expected <- liftIO $ schemaCreatedBy (createTables V4.migrateAll >> createIndexesOf V4.tableIndexes)
        databaseSchema `dbShouldBe` expected

    it "lose the columns whose values are read elsewhere now: names, indexes and paths of accounts, confirmed and free flags, locks, and the copy of each coin" $
      inMemory $ do
        version3Database
        migrateTo4 ctx
        columnNames "d_b_wallet" `dbShouldBe` ["created", "description", "fingerprint", "id", "index"]
        columnNames "d_b_portfolio" `dbShouldBe` ["created", "description", "id", "index", "wallet"]
        columnNames "d_b_account"
          `dbShouldBe` [ "balance_coins", "balance_confirmed", "balance_unconfirmed", "created", "external",
                         "id", "internal", "network", "portfolio", "purpose", "x_pub_key" ]
        columnNames "d_b_address"
          `dbShouldBe` [ "account", "address", "balance_coins", "balance_confirmed", "balance_received", "balance_txs",
                         "balance_unconfirmed", "created", "index", "internal", "label" ]
        columnNames "d_b_tx_info" `dbShouldBe` ["amount", "blob", "block_ref", "created", "dead", "network", "portfolio", "time", "txid"]
        columnNames "d_b_coin" `dbShouldBe` ["account", "address", "block_ref", "created", "locked_by_user", "outpoint", "value"]
        columnNames "d_b_pending_tx" `dbShouldBe` ["blob", "conflicted", "created", "network", "nosig_hash", "online", "portfolio"]
        columnNames "d_b_best" `dbShouldBe` ["best_block", "best_height", "best_time", "network"]

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 4: the accounts filed under portfolios and wallets"

-- | The migration to version 4 of a database with accounts, which must
-- succeed: a backup at version 3, the step, and the notice
migrateTo4 :: Ctx -> DB IO ()
migrateTo4 ctx = migrateToAndCollect 4 ctx `dbShouldBe` (Right (), [stepAnnounced, partialPortfoliosNotice], [3])

{- Reading the tables of version 4 -}

-- | The columns of a table, by name
columnNames :: Text -> DB IO [Text]
columnNames table = do
  rows <- queryRows $ "select name from pragma_table_info('" <> table <> "') order by name"
  return [name | [PersistText name] <- rows]

-- | The joins that give the portfolio of a row, as p, and its wallet, as w,
-- from the column that holds the portfolio
inPortfolio :: Text -> Text
inPortfolio column = "join d_b_portfolio p on p.id = " <> column <> " join d_b_wallet w on w.id = p.wallet"

-- | The joins that give the account of a row, as a, its portfolio, as p,
-- and its wallet, as w, from the column that holds the account
inAccount :: Text -> Text
inAccount column = "join d_b_account a on a.id = " <> column <> " " <> inPortfolio "a.portfolio"

-- | The wallet of each account, by its index, and the key of the account
walletsOfKeys :: DB IO [[PersistValue]]
walletsOfKeys = queryRows ("select w.\"index\", a.x_pub_key from d_b_account a " <> inPortfolio "a.portfolio")

-- | Each portfolio, by the index of its wallet and its own, with how many
-- accounts it has
accountsPerPortfolio :: DB IO [[PersistValue]]
accountsPerPortfolio =
  queryRows
    "select w.\"index\", p.\"index\", count(a.id) from d_b_portfolio p \
    \join d_b_wallet w on w.id = p.wallet left join d_b_account a on a.portfolio = p.id group by p.id"

-- | Each portfolio, by the index of its wallet and its own, with its
-- description
portfolioDescriptions :: DB IO [[PersistValue]]
portfolioDescriptions = queryRows "select w.\"index\", p.\"index\", p.description from d_b_portfolio p join d_b_wallet w on w.id = p.wallet"

-- | Each portfolio, by the index of its wallet and its own, with the date
-- it was created
portfolioDates :: DB IO [[PersistValue]]
portfolioDates = queryRows "select w.\"index\", p.\"index\", p.created from d_b_portfolio p join d_b_wallet w on w.id = p.wallet"

{- The database of version 3 -}

-- | The tables of version 3, its version, and these accounts with their
-- wallets, stored in the order given: hw 0.9 stored the wallet of an
-- account when it stored the account
version3With :: [V3.DBAccount] -> DB IO ()
version3With accounts = do
  createTables V3.migrateAll
  P.insert_ $ V3.DBVersion 3
  storeAccounts accounts
  commitFixture

-- | Store accounts of version 3, each after its wallet. A wallet of
-- version 3 is its fingerprint alone, which is also the key of its row.
storeAccounts :: [V3.DBAccount] -> DB IO ()
storeAccounts = mapM_ $ \acc -> do
  let walletKey = V3.dBAccountWallet acc
  P.repsert walletKey (V3.DBWallet (walletText walletKey))
  P.insert_ acc

-- | The fingerprint that names a wallet of version 3, which is its key
walletText :: V3.DBWalletId -> Text
walletText key = case P.keyToValues key of
  [PersistText fingerprint] -> fingerprint
  _ -> error "walletText: a wallet of version 3 is keyed by its fingerprint"

-- | A database of version 3 with the two wallets of the test mnemonic, as
-- hw filed them: the Bitcoin wallet 892eb8e4 with the account main at
-- index 0, created first, and savings at index 2, created last, and the
-- Bitcoin Cash wallet e5c9e2d7 with the account cash at index 0, created
-- in between. main has a receiving address that was paid and a change
-- address still free, the summary of its payment and its coin, locked by
-- a pending transaction of the time; cash has a receiving address;
-- savings holds a pending transaction.
version3Database :: DB IO ()
version3Database = do
  version3With [mainAccount, cashAccount, savingsAccount]
  P.insert_ $ address btcWallet "/44'/0'/0'" 0 "/0/0" "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" "salary" 50000 False False
  P.insert_ $ address btcWallet "/44'/0'/0'" 0 "/1/0" "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn" "" 0 True True
  P.insert_ $ address bchWallet "/44'/145'/0'" 0 "/0/0" bchExternalAddrText "" 0 False False
  P.insert_ $
    V3.DBTxInfo (V3.DBWalletKey btcWallet) "/44'/0'/0'" txid1 (S.encode (Store.BlockRef 800000 3)) True "{\"type\": \"credit\", \"amount\": 50000}" fixtureTime
  P.insert_ $
    V3.DBCoin (V3.DBWalletKey btcWallet) "/44'/0'/0'" outpoint1Text "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj" 50000 (S.encode (Store.BlockRef 800000 3)) "coin" True True fixtureTime
  P.insert_ $ V3.DBRawTx txid1 "raw"
  P.insert_ $ V3.DBPendingTx (V3.DBWalletKey btcWallet) "/44'/0'/2'" nosigHash5 "{\"tx\": \"\"}" False november3
  P.insert_ $ V3.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105
  commitFixture
  where
    address wallet accDeriv index path addr label received internal free =
      V3.DBAddress index (V3.DBWalletKey wallet) accDeriv path addr label received 0 (if received > 0 then 1 else 0) (if received > 0 then 1 else 0) received internal free fixtureTime

-- | The second test mnemonic in its own wallet, 807a5cfb, with the account
-- other at m/44'/0'/0', the same path as main, created after the others:
-- its receiving address, paid 10000 by transaction 2, and that coin. Only
-- the wallet tells its rows from those of main.
anotherSeed :: DB IO ()
anotherSeed = do
  storeAccounts [otherAccount]
  P.insert_ $ V3.DBAddress 0 (V3.DBWalletKey otherWallet) "/44'/0'/0'" "/0/0" externalAddr2Text "" 10000 0 1 1 10000 False False november4
  P.insert_ $
    V3.DBTxInfo (V3.DBWalletKey otherWallet) "/44'/0'/0'" txid2 (S.encode (Store.BlockRef 800001 1)) True "{\"type\": \"credit\", \"amount\": 10000}" november4
  P.insert_ $
    V3.DBCoin (V3.DBWalletKey otherWallet) "/44'/0'/0'" outpoint2Text externalAddr2Text 10000 (S.encode (Store.BlockRef 800001 1)) "coin" True False november4
  commitFixture

{- The accounts of version 3 -}

-- | The wallets of version 3. hw 0.9 named a wallet by the fingerprint of
-- the first account of its mnemonic on a coin type, m/44'/coin'/0': here
-- that of the test mnemonic on Bitcoin, on Bitcoin Cash and on the test
-- networks, and that of the second mnemonic on Bitcoin. They were derived apart from hw, with
-- the Python tool of test/data/message-signing.
btcWallet, bchWallet, testnetWallet, otherWallet :: Text
btcWallet = "892eb8e4"
bchWallet = "e5c9e2d7"
testnetWallet = "a7889c3f"
otherWallet = "807a5cfb"

-- | An account of version 3 as hw 0.9 stored it when it was created: with
-- its name, in a wallet, at an index, on a network, under its path, with
-- its key, and nothing received yet
oldAccount :: Text -> Text -> Int -> Text -> Text -> Text -> UTCTime -> V3.DBAccount
oldAccount name wallet index net derivation xpub created =
  V3.DBAccount name (V3.DBWalletKey wallet) index net derivation 0 0 xpub 0 0 0 created

-- | m/44'/0'/0' of the test mnemonic, of 1 Nov 2023, with a receiving and
-- a change address, and 50000 received in one coin
mainAccount :: V3.DBAccount
mainAccount =
  (oldAccount "main" btcWallet 0 "btc" "/44'/0'/0'" testAccountXPub fixtureTime)
    { V3.dBAccountExternal = 1,
      V3.dBAccountInternal = 1,
      V3.dBAccountBalanceConfirmed = 50000,
      V3.dBAccountBalanceCoins = 1
    }

-- | m/44'/145'/0' of the test mnemonic, of 2 Nov 2023, with a receiving
-- address
cashAccount :: V3.DBAccount
cashAccount = (oldAccount "cash" bchWallet 0 "bch" "/44'/145'/0'" testAccountXPubBch november2) {V3.dBAccountExternal = 1}

-- | The same account in the Bitcoin wallet of the mnemonic, at index 0 as
-- main is. hw 0.9 filed an account under the wallet its file named:
-- importacc of a file that named the Bitcoin wallet put cash there.
cashUnderBtcWallet :: V3.DBAccount
cashUnderBtcWallet = cashAccount {V3.dBAccountWallet = V3.DBWalletKey btcWallet}

-- | m/44'/0'/2' of the test mnemonic, of 3 Nov 2023
savingsAccount :: V3.DBAccount
savingsAccount = oldAccount "savings" btcWallet 2 "btc" "/44'/0'/2'" savingsXPub november3

-- | m/44'/0'/1' of the test mnemonic, of 5 Nov 2023
travelAccount :: V3.DBAccount
travelAccount = oldAccount "travel" btcWallet 1 "btc" "/44'/0'/1'" testAccountXPub1 november5

-- | m/44'/0'/0' of the second mnemonic, of 4 Nov 2023, with a receiving
-- address and 10000 received in one coin
otherAccount :: V3.DBAccount
otherAccount =
  (oldAccount "other" otherWallet 0 "btc" "/44'/0'/0'" testAccountXPub2 november4)
    { V3.dBAccountExternal = 1,
      V3.dBAccountBalanceConfirmed = 10000,
      V3.dBAccountBalanceCoins = 1
    }

-- | m/44'/1'/0' of the test mnemonic on Bitcoin testnet, of 1 Nov 2023
testnetAccount :: V3.DBAccount
testnetAccount = oldAccount "testing" testnetWallet 0 "btctest" "/44'/1'/0'" testAccountXPubTest fixtureTime

-- | m/44'/1'/1' of the test mnemonic on Bitcoin Cash testnet, of 2 Nov
-- 2023. Its path at index 0 would have been that of testing, and so its
-- key, which hw 0.9 held once.
bchTestnetAccount :: V3.DBAccount
bchTestnetAccount = oldAccount "testcash" testnetWallet 1 "bchtest" "/44'/1'/1'" bchTestnetXPub november2

-- | The key of m/44'/1'/1' of the test mnemonic, with the tpub prefix of
-- every test network, derived apart from hw with the Python tool of
-- test/data/message-signing
bchTestnetXPub :: Text
bchTestnetXPub = "tpubDCEeDpHvZZ49UfWyCY8mu8dLCVf81KVQtKKETQrQZnpsWQVoHUCo6aZbpXrRaBaNLzVGB28pj6Bmq639gp2NAi5scUt6LbN6HuNW4d8fkhq"

-- | An account created at the time of the fixtures, 1 Nov 2023
atFixtureTime :: V3.DBAccount -> V3.DBAccount
atFixtureTime acc = acc {V3.dBAccountCreated = fixtureTime}

-- | The key of the account savings, m/44'/0'/2' of the test mnemonic,
-- derived apart from hw with the Python tool of test/data/message-signing.
-- Its first receiving address is account2Addr of the shared helpers.
savingsXPub :: Text
savingsXPub = "xpub6CHK45s4QJWpvPoXu7KKatJo3k9XaEXauS9DMA3k4FAkzJDSFhQKMTQgqwXLCGAoeS8DPczpK68sPZRseu6ey1Qm2NnHZfBnvRXqo8xM2oV"

november2, november3, november4, november5 :: UTCTime
november2 = UTCTime (fromGregorian 2023 11 2) 0
november3 = UTCTime (fromGregorian 2023 11 3) 0
november4 = UTCTime (fromGregorian 2023 11 4) 0
november5 = UTCTime (fromGregorian 2023 11 5) 0

{- The other rows of version 3 -}

-- | The transactions of the fixtures, as hw 0.9 wrote their ids: one byte,
-- then zeros, in the order of the bytes of a hash, so the last byte of the
-- text
txid1, txid2, txid3, nosigHash5 :: Text
txid1 = "0000000000000000000000000000000000000000000000000000000000000001"
txid2 = "0000000000000000000000000000000000000000000000000000000000000002"
txid3 = "0000000000000000000000000000000000000000000000000000000000000003"
nosigHash5 = "0000000000000000000000000000000000000000000000000000000000000005"

-- | The first output of transactions 1 and 2 as hw 0.9 wrote them: the
-- hexadecimal text of their 36 bytes, the hash in its own order, the byte
-- first, then the index 0 in four bytes
outpoint1Text, outpoint2Text :: Text
outpoint1Text = "0100000000000000000000000000000000000000000000000000000000000000" <> "00000000"
outpoint2Text = "0200000000000000000000000000000000000000000000000000000000000000" <> "00000000"

-- | The 36 bytes of the first output of transaction 1
outpoint1Bytes :: ByteString
outpoint1Bytes = hexBytes outpoint1Text

-- | The summary of a payment as hw writes it: 40000 to an outside address,
-- 9000 of change and 1000 of fee, out of a coin of 50000. Other keys hold
-- amounts of their own, and only the one at the top is the summary's:
-- 9000 - 50000 = -41000.
spendingBlob :: ByteString
spendingBlob =
  "{\"type\":\"debit\",\"amount\":-41000,\
  \\"myoutputs\":{\"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn\":{\"value\":9000,\"path\":\"/1/0\",\"label\":\"\"}},\
  \\"otheroutputs\":{\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\":40000},\"nonstdoutputs\":[],\
  \\"myinputs\":{\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\":{\"value\":50000,\"path\":\"/0/0\",\"label\":\"\",\"outpoints\":[]}},\
  \\"otherinputs\":{},\"nonstdinputs\":[],\"size\":226,\"locktime\":0,\"fee\":1000,\"feebyte\":4,\"changeinfee\":null}"

-- | Every table of version 3
version3Tables :: [Text]
version3Tables = ["d_b_version", "d_b_wallet", "d_b_account", "d_b_address", "d_b_tx_info", "d_b_coin", "d_b_raw_tx", "d_b_pending_tx", "d_b_best"]
