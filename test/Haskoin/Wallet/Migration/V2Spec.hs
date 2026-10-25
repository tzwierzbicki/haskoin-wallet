{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The migration to the format version 2: a database of hw 0.8, built from
-- its real tables ("Haskoin.Wallet.Schema.V1") and filled with the
-- transaction summaries hw 0.8 wrote, is migrated to version 2 only and must
-- come out as hw 0.9 would have left it.
module Haskoin.Wallet.Migration.V2Spec (spec) where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, decodeStrict)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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
import qualified Haskoin.Wallet.Schema.V1 as V1
import qualified Haskoin.Wallet.Migration.V2 as V2
import qualified Haskoin.Wallet.Schema.V2 as V2
import Haskoin.Wallet.TestUtils
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "From the database of hw 0.8 (version 1) to version 2" $ do
    it "sees a database of hw 0.8 as version 1" $
      inMemory $ do
        hw08Database allSummaries
        runExceptT databaseVersion `dbShouldBe` Right (Just 1)

    it "sees an empty database of hw 0.8 as version 1" $
      inMemory $ do
        createTables V1.migrateAll
        runExceptT databaseVersion `dbShouldBe` Right (Just 1)

    it "backs up the database at version 1, announces the step and stores the version as hw 0.9 did" $
      inMemory $ do
        hw08Database allSummaries
        migrateToAndCollect 2 ctx `dbShouldBe` (Right (), [stepAnnounced], [1])
        storedVersions `dbShouldBe` [(PersistText "0.9", "text")]
        runExceptT databaseVersion `dbShouldBe` Right (Just 2)

    it "writes the summaries as hw 0.9 wrote them" $
      inMemory $ do
        hw08Database allSummaries
        migrateTo2 ctx
        blobOf creditTxid `dbShouldBe` json creditBlobOfHw09

    it "adds empty labels to the outputs and inputs of a credit" $
      inMemory $ do
        hw08Database allSummaries
        migrateTo2 ctx
        summary ctx btc creditTxid `dbShouldBe` Just creditInfo

    it "keeps the signing data of the inputs of a debit" $
      inMemory $ do
        hw08Database allSummaries
        migrateTo2 ctx
        summary ctx btc debitTxid `dbShouldBe` Just debitInfo

    it "keeps the non-standard outputs and inputs of an internal transaction" $
      inMemory $ do
        hw08Database allSummaries
        migrateTo2 ctx
        summary ctx btc internalTxid `dbShouldBe` Just internalInfo

    it "reads the addresses of each account on its own network" $
      inMemory $ do
        hw08Database allSummaries
        migrateTo2 ctx
        summary ctx bch bchCreditTxid `dbShouldBe` Just bchCreditInfo

    it "leaves the other columns of the summaries as they were" $
      inMemory $ do
        hw08Database allSummaries
        original <- summaryColumns
        migrateTo2 ctx
        summaryColumns `dbShouldBe` original

    it "leaves every other table as it was" $
      inMemory $ do
        hw08Database allSummaries
        original <- tablesRows otherTables
        migrateTo2 ctx
        tablesRows otherTables `dbShouldBe` original

    it "creates the tables hw 0.8 did not have, as hw 0.9 defined them" $
      inMemory $ do
        hw08Database allSummaries
        migrateTo2 ctx
        expected <- liftIO $ schemaCreatedBy (createTables V2.migrateAll)
        databaseSchema `dbShouldBe` expected

    it "has nothing to do in a database without transactions" $
      inMemory $ do
        hw08Database []
        migrateTo2 ctx
        tableRows "d_b_tx_info" `dbShouldBe` []
        storedVersions `dbShouldBe` [(PersistText "0.9", "text")]

    it "refuses a summary it cannot read and rolls everything back" $
      inMemory $ do
        hw08Database (unreadableSummary : allSummaries)
        original <- (,) <$> databaseSchema <*> tablesRows allTables
        migrateToAndCollect 2 ctx `dbShouldBe` (Left "OldTxInfo", [stepAnnounced], [1])
        ((,) <$> databaseSchema <*> tablesRows allTables) `dbShouldBe` original
        runExceptT databaseVersion `dbShouldBe` Right (Just 1)

{- Running the step -}

stepAnnounced :: String
stepAnnounced = "Migrating the database to version 2: the transaction summaries of hw 0.9"

-- | The migration to version 2, which must succeed
migrateTo2 :: Ctx -> DB IO ()
migrateTo2 ctx = migrateToAndCollect 2 ctx `dbShouldBe` (Right (), [stepAnnounced], [1])

-- | The summary of a transaction, in the row of version 2, read with the
-- format of hw 0.9, or Nothing when it does not parse
summary :: Ctx -> Network -> Text -> DB IO (Maybe V2.TxInfo)
summary ctx net txid = do
  rowM <- P.selectFirst [V2.DBTxInfoTxid P.==. txid] []
  return $ do
    Entity _ row <- rowM
    unmarshalJSON (net, ctx) $ BS.fromStrict $ V2.dBTxInfoBlob row

-- | The blob of a transaction summary, as JSON
blobOf :: Text -> DB IO (Maybe Value)
blobOf txid = do
  rowM <- P.selectFirst [V2.DBTxInfoTxid P.==. txid] []
  return $ do
    Entity _ row <- rowM
    decodeStrict $ V2.dBTxInfoBlob row

-- | A JSON literal, as a value: the comparison ignores spacing and key order
json :: String -> Maybe Value
json = decodeStrict . cs

-- | The columns of the summaries other than the blob
summaryColumns :: DB IO [[PersistValue]]
summaryColumns =
  queryRows
    "select account_wallet, account_derivation, txid, block_ref, confirmed, created \
    \from d_b_tx_info"

allTables, otherTables :: [Text]
allTables = "d_b_tx_info" : otherTables
otherTables =
  [ "d_b_wallet",
    "d_b_account",
    "d_b_address",
    "d_b_coin",
    "d_b_raw_tx",
    "d_b_pending_tx",
    "d_b_best"
  ]

{- The database of hw 0.8 -}

-- | A database of hw 0.8 holding the test wallet and three accounts: a
-- Bitcoin account with the given summaries, a Bitcoin Cash account whose
-- summary is among them, and a testnet account without any. The other tables
-- hold a row each, which the migration must leave alone.
hw08Database :: [V1.DBTxInfo] -> DB IO ()
hw08Database summaries = do
  createTables V1.migrateAll
  P.insert_ $ V1.DBWallet "892eb8e4"
  mapM_ P.insert_ [btcAccount, bchAccount, testnetAccount]
  mapM_ P.insert_ fixtureAddresses
  P.insert_ fixtureCoin
  mapM_ P.insert_ summaries
  P.insert_ fixturePendingTx
  P.insert_ fixtureRawTx
  P.insert_ fixtureBest
  commitFixture

testWalletKey :: V1.DBWalletId
testWalletKey = V1.DBWalletKey "892eb8e4"

btcDerivation, bchDerivation, testnetDerivation :: Text
btcDerivation = "/44'/0'/0'"
bchDerivation = "/44'/145'/0'"
testnetDerivation = "/44'/1'/0'"

btcAccount, bchAccount, testnetAccount :: V1.DBAccount
btcAccount =
  V1.DBAccount
    { V1.dBAccountName = "main",
      V1.dBAccountWallet = testWalletKey,
      V1.dBAccountIndex = 0,
      V1.dBAccountNetwork = "btc",
      V1.dBAccountDerivation = btcDerivation,
      V1.dBAccountExternal = 2,
      V1.dBAccountInternal = 1,
      V1.dBAccountXPubKey = testAccountXPub,
      V1.dBAccountBalanceConfirmed = 19100,
      V1.dBAccountBalanceUnconfirmed = 0,
      V1.dBAccountBalanceCoins = 1,
      V1.dBAccountCreated = fixtureTime
    }
bchAccount =
  V1.DBAccount
    { V1.dBAccountName = "cash",
      V1.dBAccountWallet = testWalletKey,
      V1.dBAccountIndex = 0,
      V1.dBAccountNetwork = "bch",
      V1.dBAccountDerivation = bchDerivation,
      V1.dBAccountExternal = 1,
      V1.dBAccountInternal = 0,
      V1.dBAccountXPubKey = "xpub6DXWb36CzdZvWbtSrYQkbxtapXciSuuQYJhtg4ysKAcppi5bjxxHPBNgZRyabXmF1WLotb4wVoqzGB7beYL1SLoHTd4xPCQnFRkXXt2NXTB",
      V1.dBAccountBalanceConfirmed = 70000,
      V1.dBAccountBalanceUnconfirmed = 0,
      V1.dBAccountBalanceCoins = 1,
      V1.dBAccountCreated = fixtureTime
    }
testnetAccount =
  V1.DBAccount
    { V1.dBAccountName = "testing",
      V1.dBAccountWallet = testWalletKey,
      V1.dBAccountIndex = 0,
      V1.dBAccountNetwork = "btctest",
      V1.dBAccountDerivation = testnetDerivation,
      V1.dBAccountExternal = 0,
      V1.dBAccountInternal = 0,
      V1.dBAccountXPubKey = "tpubDCEeDpHvZZ49SMZ4VkifyjwqyJjKVFkbJHzhxZJiQgcMa2vnQnMhyXcu1ZZcReuwrprnxVnJsJGro7xdrEABAtK4AGZGqv2r4H7WytWLbPm",
      V1.dBAccountBalanceConfirmed = 0,
      V1.dBAccountBalanceUnconfirmed = 0,
      V1.dBAccountBalanceCoins = 0,
      V1.dBAccountCreated = fixtureTime
    }

-- | The two addresses of the Bitcoin account that its transactions use
fixtureAddresses :: [V1.DBAddress]
fixtureAddresses =
  [ V1.DBAddress
      { V1.dBAddressIndex = 0,
        V1.dBAddressAccountWallet = testWalletKey,
        V1.dBAddressAccountDerivation = btcDerivation,
        V1.dBAddressDerivation = "/0/0",
        V1.dBAddressAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj",
        V1.dBAddressLabel = "salary",
        V1.dBAddressBalanceConfirmed = 0,
        V1.dBAddressBalanceUnconfirmed = 0,
        V1.dBAddressBalanceCoins = 0,
        V1.dBAddressBalanceTxs = 2,
        V1.dBAddressBalanceReceived = 50000,
        V1.dBAddressInternal = False,
        V1.dBAddressFree = False,
        V1.dBAddressCreated = fixtureTime
      },
    V1.DBAddress
      { V1.dBAddressIndex = 0,
        V1.dBAddressAccountWallet = testWalletKey,
        V1.dBAddressAccountDerivation = btcDerivation,
        V1.dBAddressDerivation = "/1/0",
        V1.dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
        V1.dBAddressLabel = "",
        V1.dBAddressBalanceConfirmed = 0,
        V1.dBAddressBalanceUnconfirmed = 0,
        V1.dBAddressBalanceCoins = 0,
        V1.dBAddressBalanceTxs = 2,
        V1.dBAddressBalanceReceived = 19000,
        V1.dBAddressInternal = True,
        V1.dBAddressFree = False,
        V1.dBAddressCreated = fixtureTime
      }
  ]

-- The blobs of the coins, pending transactions and raw transactions are
-- opaque to the migration, which must copy nothing and change nothing: any
-- bytes do.

fixtureCoin :: V1.DBCoin
fixtureCoin =
  V1.DBCoin
    { V1.dBCoinAccountWallet = testWalletKey,
      V1.dBCoinAccountDerivation = btcDerivation,
      V1.dBCoinOutpoint = internalTxid <> ":0",
      V1.dBCoinAddress = "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg",
      V1.dBCoinValue = 19100,
      V1.dBCoinBlockRef = S.encode (Store.BlockRef 800100 7),
      V1.dBCoinBlob = "the coin as the store sent it",
      V1.dBCoinConfirmed = True,
      V1.dBCoinLocked = False,
      V1.dBCoinCreated = fixtureTime
    }

fixturePendingTx :: V1.DBPendingTx
fixturePendingTx =
  V1.DBPendingTx
    { V1.dBPendingTxAccountWallet = testWalletKey,
      V1.dBPendingTxAccountDerivation = btcDerivation,
      V1.dBPendingTxNosigHash = "0000000000000000000000000000000000000000000000000000000000000066",
      V1.dBPendingTxBlob = "the signing data of a transaction",
      V1.dBPendingTxOnline = False,
      V1.dBPendingTxCreated = fixtureTime
    }

fixtureRawTx :: V1.DBRawTx
fixtureRawTx = V1.DBRawTx debitTxid "the raw transaction"

fixtureBest :: V1.DBBest
fixtureBest =
  V1.DBBest "btc" "00000000000000000002a7c4c1e48d76c5a37902165a270156b7a8d72728a054" 800105

{- The transaction summaries, as hw 0.8 wrote them -}

creditTxid, debitTxid, internalTxid, bchCreditTxid :: Text
creditTxid = "0000000000000000000000000000000000000000000000000000000000000011"
debitTxid = "0000000000000000000000000000000000000000000000000000000000000022"
internalTxid = "0000000000000000000000000000000000000000000000000000000000000033"
bchCreditTxid = "0000000000000000000000000000000000000000000000000000000000000055"

allSummaries :: [V1.DBTxInfo]
allSummaries = [creditSummary, debitSummary, internalSummary, bchCreditSummary]

-- | A summary row of hw 0.8: the derivation of its account, the transaction
-- id, the block of the transaction, whether it is confirmed, and the blob
summaryRow :: Text -> Text -> Store.BlockRef -> Bool -> String -> V1.DBTxInfo
summaryRow derivation txid block confirmed blob =
  V1.DBTxInfo
    { V1.dBTxInfoAccountWallet = testWalletKey,
      V1.dBTxInfoAccountDerivation = derivation,
      V1.dBTxInfoTxid = txid,
      V1.dBTxInfoBlockRef = S.encode block,
      V1.dBTxInfoConfirmed = confirmed,
      V1.dBTxInfoBlob = cs blob,
      V1.dBTxInfoCreated = fixtureTime
    }

-- | A blob that is not a summary
unreadableSummary :: V1.DBTxInfo
unreadableSummary =
  summaryRow btcDerivation "0000000000000000000000000000000000000000000000000000000000000077" (Store.BlockRef 800200 1) True "not a summary"

-- | 50000 satoshis received on /0/0, confirmed at height 800000
creditSummary :: V1.DBTxInfo
creditSummary =
  summaryRow
    btcDerivation
    creditTxid
    (Store.BlockRef 800000 3)
    True
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
    "size": 226,
    "txid": "0000000000000000000000000000000000000000000000000000000000000011",
    "type": "credit"
}|]

-- | The credit spent: 30000 satoshis paid, 19000 of change on /1/0, a fee
-- of 1000, in the mempool. The input carries its signing data, as hw 0.8
-- stored it for the transactions it built itself.
debitSummary :: V1.DBTxInfo
debitSummary =
  summaryRow
    btcDerivation
    debitTxid
    (Store.MemRef 1700000000)
    False
    [r|{
    "amount": -31000,
    "block": {
        "mempool": 1700000000
    },
    "confirmations": 0,
    "fee": 1000,
    "feebyte": 4,
    "myinputs": {
        "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {
            "path": "/0/0",
            "siginput": [
                {
                    "outpoint": {
                        "index": 0,
                        "txid": "0000000000000000000000000000000000000000000000000000000000000011"
                    },
                    "pkscript": "76a914c80d55223e2f1061363add2a7b0363c285bf298d88ac",
                    "sighash": 1,
                    "value": 50000
                }
            ],
            "value": 50000
        }
    },
    "myoutputs": {
        "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn": {
            "path": "/1/0",
            "value": 19000
        }
    },
    "nonstdinputs": [],
    "nonstdoutputs": [],
    "otherinputs": {},
    "otheroutputs": {
        "191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q": 30000
    },
    "size": 226,
    "txid": "0000000000000000000000000000000000000000000000000000000000000022",
    "type": "debit"
}|]

-- | The change moved to /0/1, with an OP_RETURN output and an input paying a
-- non-standard script, for a fee of 500. The input of the wallet has no
-- signing data, which hw 0.8 left out of the JSON when there was none.
internalSummary :: V1.DBTxInfo
internalSummary =
  summaryRow
    btcDerivation
    internalTxid
    (Store.BlockRef 800100 7)
    True
    [r|{
    "amount": -500,
    "block": {
        "height": 800100,
        "position": 7
    },
    "confirmations": 5,
    "fee": 500,
    "feebyte": 1,
    "myinputs": {
        "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn": {
            "path": "/1/0",
            "value": 19000
        }
    },
    "myoutputs": {
        "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg": {
            "path": "/0/1",
            "value": 19100
        }
    },
    "nonstdinputs": [
        {
            "address": null,
            "coinbase": false,
            "output": 1,
            "pkscript": "51",
            "sequence": 4294967295,
            "sigscript": "",
            "txid": "0000000000000000000000000000000000000000000000000000000000000044",
            "value": 600,
            "witness": []
        }
    ],
    "nonstdoutputs": [
        {
            "address": null,
            "pkscript": "6a0474657374",
            "spender": null,
            "spent": false,
            "value": 0
        }
    ],
    "otherinputs": {},
    "otheroutputs": {},
    "size": 300,
    "txid": "0000000000000000000000000000000000000000000000000000000000000033",
    "type": "internal"
}|]

-- | 70000 satoshis received by the Bitcoin Cash account on its /0/0, whose
-- addresses are written in the cashaddr format
bchCreditSummary :: V1.DBTxInfo
bchCreditSummary =
  summaryRow
    bchDerivation
    bchCreditTxid
    (Store.BlockRef 820000 1)
    True
    [r|{
    "amount": 70000,
    "block": {
        "height": 820000,
        "position": 1
    },
    "confirmations": 3,
    "fee": 10000,
    "feebyte": 52,
    "myinputs": {},
    "myoutputs": {
        "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u": {
            "path": "/0/0",
            "value": 70000
        }
    },
    "nonstdinputs": [],
    "nonstdoutputs": [],
    "otherinputs": {
        "bitcoincash:qz58yx42jwteh7xuzuyc8zkac4zdruynmyq2txkffn": {
            "value": 80000
        }
    },
    "otheroutputs": {},
    "size": 192,
    "txid": "0000000000000000000000000000000000000000000000000000000000000055",
    "type": "credit"
}|]

{- The credit as hw 0.9 wrote it -}

-- | The format of hw 0.9 adds an empty label to every output and input of the
-- wallet, and the pending state, unknown for an old transaction. It has no
-- "changeinfee" key, which came with hw 0.10.
creditBlobOfHw09 :: String
creditBlobOfHw09 =
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
            "label": "",
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

{- The same summaries in the format of hw 0.9 -}

-- The types of hw 0.9's summaries are frozen in the step module: the step
-- adds an empty label to every output and input of the wallet, and the
-- pending state, unknown for an old transaction.

creditInfo :: V2.TxInfo
creditInfo =
  V2.TxInfo
    { V2.txInfoHash = Just (fakeTxHash 0x11),
      V2.txInfoType = V2.TxCredit,
      V2.txInfoAmount = 50000,
      V2.txInfoMyOutputs = Map.fromList [(externalAddr 0, V2.MyOutputs 50000 (Deriv :/ 0 :/ 0) "")],
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

debitInfo :: V2.TxInfo
debitInfo =
  V2.TxInfo
    { V2.txInfoHash = Just (fakeTxHash 0x22),
      V2.txInfoType = V2.TxDebit,
      V2.txInfoAmount = -31000,
      V2.txInfoMyOutputs = Map.fromList [(internalAddr 0, V2.MyOutputs 19000 (Deriv :/ 1 :/ 0) "")],
      V2.txInfoOtherOutputs = Map.fromList [(otherAddr 2, 30000)],
      V2.txInfoNonStdOutputs = [],
      V2.txInfoMyInputs =
        Map.fromList
          [ ( externalAddr 0,
              V2.MyInputs
                50000
                (Deriv :/ 0 :/ 0)
                ""
                [ SigInput
                    (PayPKHash (hash160 "c80d55223e2f1061363add2a7b0363c285bf298d"))
                    50000
                    (OutPoint (fakeTxHash 0x11) 0)
                    sigHashAll
                    Nothing
                ]
            )
          ],
      V2.txInfoOtherInputs = Map.empty,
      V2.txInfoNonStdInputs = [],
      V2.txInfoSize = 226,
      V2.txInfoFee = 1000,
      V2.txInfoFeeByte = 4,
      V2.txInfoBlockRef = Store.MemRef 1700000000,
      V2.txInfoConfirmations = 0,
      V2.txInfoPending = Nothing
    }

internalInfo :: V2.TxInfo
internalInfo =
  V2.TxInfo
    { V2.txInfoHash = Just (fakeTxHash 0x33),
      V2.txInfoType = V2.TxInternal,
      V2.txInfoAmount = -500,
      V2.txInfoMyOutputs = Map.fromList [(externalAddr 1, V2.MyOutputs 19100 (Deriv :/ 0 :/ 1) "")],
      V2.txInfoOtherOutputs = Map.empty,
      V2.txInfoNonStdOutputs = [Store.StoreOutput 0 (bytes "6a0474657374") Nothing Nothing],
      V2.txInfoMyInputs = Map.fromList [(internalAddr 0, V2.MyInputs 19000 (Deriv :/ 1 :/ 0) "" [])],
      V2.txInfoOtherInputs = Map.empty,
      V2.txInfoNonStdInputs =
        [Store.StoreInput (OutPoint (fakeTxHash 0x44) 1) 0xffffffff BS.empty (bytes "51") 600 [] Nothing],
      V2.txInfoSize = 300,
      V2.txInfoFee = 500,
      V2.txInfoFeeByte = 1,
      V2.txInfoBlockRef = Store.BlockRef 800100 7,
      V2.txInfoConfirmations = 5,
      V2.txInfoPending = Nothing
    }

bchCreditInfo :: V2.TxInfo
bchCreditInfo =
  V2.TxInfo
    { V2.txInfoHash = Just (fakeTxHash 0x55),
      V2.txInfoType = V2.TxCredit,
      V2.txInfoAmount = 70000,
      V2.txInfoMyOutputs =
        Map.fromList
          [(bchAddress "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", V2.MyOutputs 70000 (Deriv :/ 0 :/ 0) "")],
      V2.txInfoOtherOutputs = Map.empty,
      V2.txInfoNonStdOutputs = [],
      V2.txInfoMyInputs = Map.empty,
      V2.txInfoOtherInputs =
        Map.fromList
          [(bchAddress "bitcoincash:qz58yx42jwteh7xuzuyc8zkac4zdruynmyq2txkffn", V2.OtherInputs 80000 [])],
      V2.txInfoNonStdInputs = [],
      V2.txInfoSize = 192,
      V2.txInfoFee = 10000,
      V2.txInfoFeeByte = 52,
      V2.txInfoBlockRef = Store.BlockRef 820000 1,
      V2.txInfoConfirmations = 3,
      V2.txInfoPending = Nothing
    }

bchAddress :: Text -> Address
bchAddress = fromJust . textToAddr bch

bytes :: Text -> ByteString
bytes = fromJust . decodeHex

hash160 :: Text -> Hash160
hash160 = forceRight . S.decode . bytes
