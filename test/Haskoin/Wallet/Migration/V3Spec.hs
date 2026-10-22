{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The migration to the format version 3: a database of hw 0.9, built from
-- its real tables ("Haskoin.Wallet.Schema.V2") with the text label hw 0.9
-- stored as its version, is migrated to version 3 only and ends with the
-- version stored as a number.
module Haskoin.Wallet.Migration.V3Spec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (Entity (..), PersistValue (..))
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database (DB)
import Haskoin.Wallet.Migration (databaseVersion)
import Haskoin.Wallet.Migration.TestUtils
import qualified Haskoin.Wallet.Migration.V2 as V2
import qualified Haskoin.Wallet.Schema.V2 as V2
import qualified Haskoin.Wallet.Schema.V3 as V3
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of hw 0.9 (version 2) to version 3" $ do
    forM_ ["0.9", "0.9.0"] $ \label ->
      it ("sees the label " <> cs label <> " of hw 0.9 as version 2") $
        inMemory $ do
          hw09Database label
          runExceptT databaseVersion `dbShouldBe` Right (Just 2)

    it "backs up the database at version 2, announces the step and stores the version as the number 3" $
      inMemory $ do
        hw09Database "0.9"
        migrateToAndCollect 3 ctx `dbShouldBe` (Right (), [stepAnnounced], [2])
        storedVersions `dbShouldBe` [(PersistInt64 3, "integer")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 3)

    it "gives the version table the column of version 3" $
      inMemory $ do
        hw09Database "0.9"
        migrateTo3 ctx
        expected <- liftIO $ schemaCreatedBy V3.migrateAll
        (lookup "d_b_version" <$> databaseSchema) `dbShouldBe` lookup "d_b_version" expected

    it "keeps the transaction summaries and their labels" $
      inMemory $ do
        hw09Database "0.9"
        original <- tableRows "d_b_tx_info"
        migrateTo3 ctx
        tableRows "d_b_tx_info" `dbShouldBe` original
        labelledSummary ctx `dbShouldBe` Just labelledCreditInfo

    it "leaves every other table as it was" $
      inMemory $ do
        hw09Database "0.9"
        original <- tablesRows otherTables
        migrateTo3 ctx
        tablesRows otherTables `dbShouldBe` original

    it "ends with the tables of version 3" $
      inMemory $ do
        hw09Database "0.9"
        migrateTo3 ctx
        expected <- liftIO $ schemaCreatedBy V3.migrateAll
        databaseSchema `dbShouldBe` expected

    it "refuses a version it does not know" $
      inMemory $ do
        hw09Database "abc"
        migrateToAndCollect 3 ctx `dbShouldBe` (Left "Unknown database version abc", [], [])
        storedVersions `dbShouldBe` [(PersistText "abc", "text")]

    it "refuses a version table without a row" $
      inMemory $ do
        createTables V2.migrateAll
        commitFixture
        migrateToAndCollect 3 ctx `dbShouldBe` (Left "The database has no version", [], [])

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 3: the version stored as a number"

-- | The migration to version 3, which must succeed
migrateTo3 :: Ctx -> DB IO ()
migrateTo3 ctx = migrateToAndCollect 3 ctx `dbShouldBe` (Right (), [stepAnnounced], [2])

{- The database of hw 0.9 -}

-- | A database of hw 0.9 with the given version label, the test wallet, its
-- Bitcoin account, a labelled address and the labelled summary of a credit
hw09Database :: Text -> DB IO ()
hw09Database label = do
  createTables V2.migrateAll
  P.insert_ $ V2.DBVersion label
  P.insert_ $ V2.DBWallet "892eb8e4"
  P.insert_ btcAccount
  P.insert_ fixtureAddress
  P.insert_ creditSummary
  P.insert_ fixtureBest
  commitFixture

otherTables :: [Text]
otherTables =
  [ "d_b_wallet",
    "d_b_account",
    "d_b_address",
    "d_b_tx_info",
    "d_b_coin",
    "d_b_raw_tx",
    "d_b_pending_tx",
    "d_b_best"
  ]

testWalletKey :: V2.DBWalletId
testWalletKey = V2.DBWalletKey "892eb8e4"

btcDerivation :: Text
btcDerivation = "/44'/0'/0'"

btcAccount :: V2.DBAccount
btcAccount =
  V2.DBAccount
    { V2.dBAccountName = "main",
      V2.dBAccountWallet = testWalletKey,
      V2.dBAccountIndex = 0,
      V2.dBAccountNetwork = "btc",
      V2.dBAccountDerivation = btcDerivation,
      V2.dBAccountExternal = 1,
      V2.dBAccountInternal = 0,
      V2.dBAccountXPubKey = testAccountXPub,
      V2.dBAccountBalanceConfirmed = 50000,
      V2.dBAccountBalanceUnconfirmed = 0,
      V2.dBAccountBalanceCoins = 1,
      V2.dBAccountCreated = fixtureTime
    }

fixtureAddress :: V2.DBAddress
fixtureAddress =
  V2.DBAddress
    { V2.dBAddressIndex = 0,
      V2.dBAddressAccountWallet = testWalletKey,
      V2.dBAddressAccountDerivation = btcDerivation,
      V2.dBAddressDerivation = "/0/0",
      V2.dBAddressAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
      V2.dBAddressLabel = "salary",
      V2.dBAddressBalanceConfirmed = 50000,
      V2.dBAddressBalanceUnconfirmed = 0,
      V2.dBAddressBalanceCoins = 1,
      V2.dBAddressBalanceTxs = 1,
      V2.dBAddressBalanceReceived = 50000,
      V2.dBAddressInternal = False,
      V2.dBAddressFree = False,
      V2.dBAddressCreated = fixtureTime
    }

fixtureBest :: V2.DBBest
fixtureBest =
  V2.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105

creditTxid :: Text
creditTxid = "0000000000000000000000000000000000000000000000000000000000000011"

-- | 50000 satoshis received on /0/0, labelled "salary", as hw 0.9 wrote it:
-- with labels and the pending state, without the change absorbed into the
-- fee, which this hw added as an optional field
creditSummary :: V2.DBTxInfo
creditSummary =
  V2.DBTxInfo
    { V2.dBTxInfoAccountWallet = testWalletKey,
      V2.dBTxInfoAccountDerivation = btcDerivation,
      V2.dBTxInfoTxid = creditTxid,
      V2.dBTxInfoBlockRef = S.encode (Store.BlockRef 800000 3),
      V2.dBTxInfoConfirmed = True,
      V2.dBTxInfoBlob = cs creditBlob,
      V2.dBTxInfoCreated = fixtureTime
    }

-- | The summary as hw 0.9 wrote it
creditBlob :: String
creditBlob =
  [r|{
    "amount": 50000,
    "block": {
        "height": 800000,
        "position": 3
    },
    "confirmations": 12,
    "fee": 1000,
    "feebyte": 4,
    "myinputs": {},
    "myoutputs": {
        "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {
            "label": "salary",
            "path": "/0/0",
            "value": 50000
        }
    },
    "nonstdinputs": [],
    "nonstdoutputs": [],
    "otherinputs": {
        "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ": {
            "value": 100000
        }
    },
    "otheroutputs": {
        "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": 49000
    },
    "pending": null,
    "size": 226,
    "txid": "0000000000000000000000000000000000000000000000000000000000000011",
    "type": "credit"
}|]

-- | The summary of the credit, in the row of version 3, read with the format
-- of hw 0.9, frozen in the step module to version 2: the step to version 3
-- leaves the summaries as they are
labelledSummary :: Ctx -> DB IO (Maybe V2.TxInfo)
labelledSummary ctx = do
  rowM <- P.selectFirst [V3.DBTxInfoTxid P.==. creditTxid] []
  return $ do
    Entity _ row <- rowM
    unmarshalJSON (btc, ctx) $ BS.fromStrict $ V3.dBTxInfoBlob row

labelledCreditInfo :: V2.TxInfo
labelledCreditInfo =
  V2.TxInfo
    { V2.txInfoHash = Just (fakeTxHash 0x11),
      V2.txInfoType = V2.TxCredit,
      V2.txInfoAmount = 50000,
      V2.txInfoMyOutputs = Map.fromList [(externalAddr 0, V2.MyOutputs 50000 (Deriv :/ 0 :/ 0) "salary")],
      V2.txInfoOtherOutputs = Map.fromList [(otherAddr 0, 49000)],
      V2.txInfoNonStdOutputs = [],
      V2.txInfoMyInputs = Map.empty,
      V2.txInfoOtherInputs = Map.fromList [(otherAddr 1, V2.OtherInputs 100000 [])],
      V2.txInfoNonStdInputs = [],
      V2.txInfoSize = 226,
      V2.txInfoFee = 1000,
      V2.txInfoFeeByte = 4,
      V2.txInfoBlockRef = Store.BlockRef 800000 3,
      V2.txInfoConfirmations = 12,
      V2.txInfoPending = Nothing
    }
