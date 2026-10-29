{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The transaction summaries: what a transaction of the store means to an
-- account, how hw writes a transaction as JSON, and the summaries kept in
-- the database and listed by txs. The account is account 0 of the test
-- wallet. The story of these tests is a payment of 50000 received at its
-- first receiving address, then spent: 40000 to an outside address, 9000
-- of change to its first change address, and 1000 of fee.
module Haskoin.Wallet.TxInfoSpec (spec) where

import Conduit (liftIO)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (Object, Value (Null, Object), eitherDecode, encode)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address
import Haskoin.Wallet.BestBlock
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.Util (Page (Page))
import Numeric.Natural (Natural)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), MarshalJsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryMyInputs, arbitraryMyOutputs, arbitraryTxInfo, arbitraryTxSummary, arbitraryTxType)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx -> do
  summarySpec ctx
  statusSpec
  jsonSpec
  storingSpec ctx
  listingSpec ctx
  historySpec ctx
  -- A transaction, its summary and the parts of the summary read back as
  -- they were written
  testIdentity
    def
      { jsonTests = [JsonBox arbitraryTxType, JsonBox arbitraryMyOutputs, JsonBox arbitraryMyInputs],
        marshalJsonTests = [MarshalJsonBox ((,) btc <$> arbitraryTxSummary btc), MarshalJsonBox ((,) btc <$> arbitraryTxInfo btc)]
      }

{- The story -}

-- | The addresses of the account with their paths, as the sync hands them
-- to the summary: its first receiving and its first change address
ourPaths :: Map Address SoftPath
ourPaths = Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0), (internalAddr 0, Deriv :/ 1 :/ 0)]

-- | The payment: 50000 to the first receiving address, transaction 1
paymentAt :: Ctx -> Store.BlockRef -> Store.Transaction
paymentAt ctx ref = creditTx ctx (fakeTxHash 1) ref (externalAddr 0) 50000

-- | The payment, at block 800000
payment :: Ctx -> Store.Transaction
payment ctx = paymentAt ctx (Store.BlockRef 800000 1)

-- | The payment spent, transaction 2: 40000 to an outside address, 9000 of
-- change to the first change address, 1000 of fee
spendAt :: Ctx -> Store.BlockRef -> Store.Transaction
spendAt ctx ref = spendingStoreTx ctx (fakeTxHash 2) ref (fakeTxHash 1, 0) (externalAddr 0) [(otherAddr 0, 40000), (internalAddr 0, 9000)]

-- | The spending, at block 800001
spend :: Ctx -> Store.Transaction
spend ctx = spendAt ctx (Store.BlockRef 800001 1)

-- | What the payment means to the account: 226 bytes, so 4 satoshis per
-- byte of fee
paymentSummary :: TxSummary
paymentSummary =
  TxSummary
    { summaryType = TxCredit,
      summaryAmount = 50000,
      summaryFee = 1000,
      summaryFeeByte = 4,
      summaryChangeInFee = Nothing,
      summarySize = 226,
      summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (Deriv :/ 0 :/ 0) "")],
      summaryOtherOutputs = Map.empty,
      summaryNonStdOutputs = [],
      summaryMyInputs = Map.empty,
      summaryOtherInputs = Map.fromList [(otherAddr 0, 51000)],
      summaryNonStdInputs = []
    }

-- | What spending it means to the account: the 40000 and the fee leave
spendSummary :: TxSummary
spendSummary =
  TxSummary
    { summaryType = TxDebit,
      summaryAmount = -41000,
      summaryFee = 1000,
      summaryFeeByte = 4,
      summaryChangeInFee = Nothing,
      summarySize = 226,
      summaryMyOutputs = Map.fromList [(internalAddr 0, MyOutputs 9000 (Deriv :/ 1 :/ 0) "")],
      summaryOtherOutputs = Map.fromList [(otherAddr 0, 40000)],
      summaryNonStdOutputs = [],
      summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (Deriv :/ 0 :/ 0) "" [OutPoint (fakeTxHash 1) 0])],
      summaryOtherInputs = Map.empty,
      summaryNonStdInputs = []
    }

-- | The summaries once the receiving address is labelled "Rent"
paymentToRent, spendFromRent :: TxSummary
paymentToRent = paymentSummary {summaryMyOutputs = Map.fromList [(externalAddr 0, MyOutputs 50000 (Deriv :/ 0 :/ 0) "Rent")]}
spendFromRent = spendSummary {summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (Deriv :/ 0 :/ 0) "Rent" [OutPoint (fakeTxHash 1) 0])]}

{- Helpers -}

-- | The account of these tests
account :: Ctx -> ExceptT String (DB IO) DBAccountId
account ctx = fst <$> testNewAcc ctx "main"

-- | The summary of a transaction of the store, as the sync builds it
summarized :: Store.Transaction -> TxSummary
summarized = storeSummary btc ourPaths

-- | Store the summary of a transaction for the account, as the sync does
record :: DBAccountId -> Store.Transaction -> ExceptT String (DB IO) Bool
record accId tx = lift $ repsertSummary btc accId tx.txid tx.block (storeTime tx.timestamp) (summarized tx)

-- | The id of each listed transaction, in the order of the listing
listedIds :: DBAccountId -> Page -> ExceptT String (DB IO) [TxHash]
listedIds accId page = map (chainTxid . chainOf) <$> txsPage accId page

-- | The block and the confirmations of each listed transaction
listedBlocks :: DBAccountId -> ExceptT String (DB IO) [(Store.BlockRef, Natural)]
listedBlocks accId = do
  infos <- txsPage accId (Page 10 0)
  return [(chainBlock (chainOf info), chainConfirmations (chainOf info)) | info <- infos]

-- | Whether each listed transaction is dead
listedDead :: DBAccountId -> ExceptT String (DB IO) [Bool]
listedDead accId = map (chainDead . chainOf) <$> txsPage accId (Page 10 0)

-- | The labels of the account's inputs and of its outputs in each listed
-- transaction
listedLabels :: DBAccountId -> ExceptT String (DB IO) [([Text], [Text])]
listedLabels accId = do
  infos <- txsPage accId (Page 10 0)
  return
    [ (myInputsLabel <$> Map.elems summary.summaryMyInputs, myOutputsLabel <$> Map.elems summary.summaryMyOutputs)
      | info <- infos,
        let summary = txInfoSummary info
    ]

-- | The row of a transaction of the account
storedRow :: DBAccountId -> TxHash -> DB IO (Maybe DBTxInfo)
storedRow accId tid = P.get $ DBTxInfoKey accId (hashBytes tid)

summarySpec :: Ctx -> Spec
summarySpec ctx =
  describe "Summarizing a transaction of the store" $ do
    it "describes a payment received as a credit of the amount" $
      summarized (payment ctx) `shouldBe` paymentSummary

    it "describes a payment made as a debit of the amount and the fee, with the coin spent and the change" $
      summarized (spend ctx) `shouldBe` spendSummary

    it "describes a transfer between the account's own addresses as internal, costing the fee" $ do
      let transfer = spendingStoreTx ctx (fakeTxHash 3) (Store.BlockRef 800001 2) (fakeTxHash 1, 0) (externalAddr 0) [(internalAddr 0, 49000)]
          summary = summarized transfer
      (summaryType summary, summaryAmount summary, summaryFee summary) `shouldBe` (TxInternal, -1000, 1000)

    it "tells a credit from a debit from an internal transfer by what the account loses" $ do
      txType 50000 1000 `shouldBe` TxCredit
      txType (-41000) 1000 `shouldBe` TxDebit
      txType (-1000) 1000 `shouldBe` TxInternal

    it "adds up several outputs to one address" $ do
      let twice = payoutTx ctx (fakeTxHash 4) (Store.BlockRef 800000 2) [(externalAddr 0, 20000), (externalAddr 0, 30000)]
          summary = summarized twice
      summaryMyOutputs summary `shouldBe` Map.fromList [(externalAddr 0, MyOutputs 50000 (Deriv :/ 0 :/ 0) "")]
      summaryAmount summary `shouldBe` 50000

    it "adds up several coins of one address, keeping each coin" $ do
      let coins = [inputFrom ctx (fakeTxHash 1, 0) (externalAddr 0) 30000, inputFrom ctx (fakeTxHash 4, 1) (externalAddr 0) 20000]
          summary = summarized $ storeTx (fakeTxHash 5) (Store.BlockRef 800001 3) coins [outputTo ctx (otherAddr 0) 49000] 1000
      summaryMyInputs summary
        `shouldBe` Map.fromList [(externalAddr 0, MyInputs 50000 (Deriv :/ 0 :/ 0) "" [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 4) 1])]
      (summaryType summary, summaryAmount summary) `shouldBe` (TxDebit, -50000)

    it "adds up the coins of others by address" $ do
      let coins =
            [ inputFrom ctx (fakeTxHash 0xee, 0) (otherAddr 0) 30000,
              inputFrom ctx (fakeTxHash 0xee, 1) (otherAddr 0) 21000,
              inputFrom ctx (fakeTxHash 0xef, 0) (otherAddr 1) 10000
            ]
          summary = summarized $ storeTx (fakeTxHash 6) (Store.BlockRef 800000 3) coins [outputTo ctx (externalAddr 0) 60000] 1000
      summaryOtherInputs summary `shouldBe` Map.fromList [(otherAddr 0, 51000), (otherAddr 1, 10000)]
      (summaryType summary, summaryAmount summary) `shouldBe` (TxCredit, 60000)

    it "sets apart the coinbase input and an output that pays no address" $ do
      -- A mined reward paid to the account, with the OP_RETURN output
      -- miners add. A coinbase transaction pays no fee.
      let coinbase = Store.StoreCoinbase nullOutPoint 0xffffffff BS.empty []
          opReturn = Store.StoreOutput 0 (BS.pack [0x6a, 0x04, 0x74, 0x65, 0x73, 0x74]) Nothing Nothing
          reward = storeTx (fakeTxHash 7) (Store.BlockRef 800002 0) [coinbase] [outputTo ctx (externalAddr 0) 625000000, opReturn] 0
          summary = summarized reward
      (summaryType summary, summaryAmount summary, summaryFee summary, summaryFeeByte summary) `shouldBe` (TxCredit, 625000000, 0, 0)
      summaryNonStdInputs summary `shouldBe` [coinbase]
      summaryNonStdOutputs summary `shouldBe` [opReturn]
      summaryOtherInputs summary `shouldBe` Map.empty

    it "sees an address the account does not have as an outsider's" $ do
      let summary = storeSummary btc Map.empty (payment ctx)
      involvesWallet summary `shouldBe` False
      summaryAmount summary `shouldBe` 0
      summaryOtherOutputs summary `shouldBe` Map.fromList [(externalAddr 0, 50000)]
      involvesWallet paymentSummary `shouldBe` True

    it "sets apart an output at an address the network cannot write, a SegWit one on Bitcoin Cash" $ do
      -- The store names the address whatever the network; the summary only
      -- keeps what the network can write, since that is what it stores
      let paid = outputTo ctx segwitAddr 30000
          tx = storeTx (fakeTxHash 9) (Store.BlockRef 800000 4) [inputFrom ctx (fakeTxHash 0xee, 0) (otherAddr 0) 31000] [paid] 1000
          onBch = storeSummary bch Map.empty tx
      summaryOtherOutputs onBch `shouldBe` Map.empty
      summaryNonStdOutputs onBch `shouldBe` [paid]
      summaryOtherOutputs (storeSummary btc Map.empty tx) `shouldBe` Map.fromList [(segwitAddr, 30000)]

statusSpec :: Spec
statusSpec =
  describe "The status of a transaction" $ do
    it "knows the id of a transaction on the chain" $
      txInfoHash (TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False))) `shouldBe` Just (fakeTxHash 2)

    it "gives a transaction in preparation its id once it is signed" $ do
      let unsigned = PendingStatus (fakeTxHash 7) Nothing False False Nothing []
          signed = PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False Nothing []
      (pendingSigned unsigned, txInfoHash (TxInfo spendSummary (Pending unsigned))) `shouldBe` (False, Nothing)
      (pendingSigned signed, txInfoHash (TxInfo spendSummary (Pending signed))) `shouldBe` (True, Just (fakeTxHash 2))

-- | The pending status of a transaction as written in JSON
pendingOf :: Object -> Either String Object
pendingOf written = case KeyMap.lookup "pending" written of
  Just (Object status) -> Right status
  _ -> Left "No pending status"

-- | A transaction as written in JSON, without the creation time of its
-- pending status, as hw wrote it before it kept the time
withoutCreated :: Object -> Object
withoutCreated written = case KeyMap.lookup "pending" written of
  Just (Object status) -> KeyMap.insert "pending" (Object (KeyMap.delete "created" status)) written
  _ -> written

jsonSpec :: Spec
jsonSpec =
  describe "Transactions in JSON" $ do
    it "writes a transaction on the chain as txs prints it, and reads it back" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False))
          json :: String
          json =
            [r|{
              "txid": "0000000000000000000000000000000000000000000000000000000000000002",
              "type": "debit",
              "amount": -41000,
              "fee": 1000,
              "feebyte": 4,
              "changeinfee": null,
              "size": 226,
              "myoutputs": {"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn": {"value": 9000, "path": "/1/0", "label": ""}},
              "otheroutputs": {"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": 40000},
              "nonstdoutputs": [],
              "myinputs": {
                "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {
                  "value": 50000,
                  "path": "/0/0",
                  "label": "",
                  "outpoints": [{"txid": "0000000000000000000000000000000000000000000000000000000000000001", "index": 0}]
                }
              },
              "otherinputs": {},
              "nonstdinputs": [],
              "block": {"height": 800001, "position": 1},
              "time": "2023-11-14T22:13:20Z",
              "confirmations": 3,
              "dead": false,
              "pending": null
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc info)
      unmarshalJSON btc (cs json) `shouldBe` Just info

    -- Stored before hw kept the time, until the next sync
    it "writes a transaction on the chain without its time as null" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) Nothing 3 False))
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info
      let written = eitherDecode (marshalJSON btc info) :: Either String Object
      (KeyMap.lookup "time" <$> written) `shouldBe` Right (Just Null)

    it "writes a transaction in preparation with its state, and the block of one that is nowhere yet" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 1)) True False (Just seenTime) []))
          json :: String
          json =
            [r|{
              "txid": "0000000000000000000000000000000000000000000000000000000000000001",
              "type": "credit",
              "amount": 50000,
              "fee": 1000,
              "feebyte": 4,
              "changeinfee": null,
              "size": 226,
              "myoutputs": {"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {"value": 50000, "path": "/0/0", "label": ""}},
              "otheroutputs": {},
              "nonstdoutputs": [],
              "myinputs": {},
              "otherinputs": {"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": 51000},
              "nonstdinputs": [],
              "block": {"mempool": 0},
              "time": null,
              "confirmations": 0,
              "dead": false,
              "pending": {
                "nosighash": "0000000000000000000000000000000000000000000000000000000000000007",
                "signed": true,
                "online": true,
                "conflicted": false,
                "created": "2023-11-14T22:13:20Z",
                "replaces": []
              }
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc info)
      unmarshalJSON btc (cs json) `shouldBe` Just info

    -- One the wallet does not hold, read from a file, has none
    it "writes no creation time for a transaction in preparation that has none as null" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing []))
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info
      (KeyMap.lookup "created" <$> (pendingOf =<< eitherDecode (marshalJSON btc info))) `shouldBe` Right (Just Null)

    -- As hw wrote it before it kept the time
    it "reads a transaction in preparation without its creation time" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing []))
      (unmarshalJSON btc . encode . Object . withoutCreated <$> eitherDecode (marshalJSON btc info)) `shouldBe` Right (Just info)

    it "reads the summaries stored by earlier versions of hw" $ do
      -- Those blobs carried the status too, wrote the inputs of others as
      -- objects, knew no outpoints, and the oldest have no change in fee
      let oldPayment :: String
          oldPayment =
            [r|{
              "txid": "0000000000000000000000000000000000000000000000000000000000000001",
              "type": "credit",
              "amount": 50000,
              "myoutputs": {"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {"value": 50000, "path": "/0/0", "label": ""}},
              "otheroutputs": {},
              "nonstdoutputs": [],
              "myinputs": {},
              "otherinputs": {"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": {"value": 51000}},
              "nonstdinputs": [],
              "size": 226,
              "fee": 1000,
              "feebyte": 4,
              "block": {"height": 800000, "position": 1},
              "confirmations": 0,
              "pending": null
            }|]
          oldSpend :: String
          oldSpend =
            [r|{
              "txid": "0000000000000000000000000000000000000000000000000000000000000002",
              "type": "debit",
              "amount": -41000,
              "myoutputs": {"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn": {"value": 9000, "path": "/1/0", "label": ""}},
              "otheroutputs": {"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": 40000},
              "nonstdoutputs": [],
              "myinputs": {"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {"value": 50000, "path": "/0/0", "label": ""}},
              "otherinputs": {},
              "nonstdinputs": [],
              "size": 226,
              "fee": 1000,
              "feebyte": 4,
              "changeinfee": null,
              "block": {"height": 800001, "position": 1},
              "confirmations": 0,
              "pending": null,
              "dead": false
            }|]
      unmarshalJSON btc (cs oldPayment) `shouldBe` Just paymentSummary
      unmarshalJSON btc (cs oldSpend)
        `shouldBe` Just spendSummary {summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (Deriv :/ 0 :/ 0) "" [])]}

    it "refuses a stored summary with an address that is not one of the network" $ do
      let json :: String
          json =
            [r|{
              "type": "credit", "amount": 50000, "fee": 1000, "feebyte": 4, "size": 226,
              "myoutputs": {"nonsense": {"value": 50000, "path": "/0/0", "label": ""}},
              "otheroutputs": {}, "nonstdoutputs": [], "myinputs": {}, "otherinputs": {}, "nonstdinputs": []
            }|]
          parsed :: Either String TxSummary
          parsed = parseEither (unmarshalValue btc) =<< eitherDecode (cs json)
      parsed `shouldBe` Left "Error in $: Invalid address nonsense"

storingSpec :: Ctx -> Spec
storingSpec ctx =
  describe "Storing summaries" $ do
    it "stores a summary at its block, alive" $
      inDatabaseE $ do
        accId <- account ctx
        record accId (payment ctx) `dbShouldBeE` True
        row <- lift (storedRow accId (fakeTxHash 1)) >>= liftMaybe "The summary is not stored"
        liftIO $ do
          dBTxInfoBlockRef row `shouldBe` S.encode (Store.BlockRef 800000 1)
          dBTxInfoDead row `shouldBe` False
        record accId (paymentAt ctx (Store.MemRef 1700000000)) `dbShouldBeE` True
        (fmap dBTxInfoBlockRef <$> lift (storedRow accId (fakeTxHash 1))) `dbShouldBeE` Just (S.encode (Store.MemRef 1700000000))

    it "reports no change when the same summary comes again, and a change when its block moves" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (paymentAt ctx (Store.MemRef 1700000000))
        first <- lift $ storedRow accId (fakeTxHash 1)
        record accId (paymentAt ctx (Store.MemRef 1700000000)) `dbShouldBeE` False
        lift (storedRow accId (fakeTxHash 1)) `dbShouldBeE` first
        -- Confirmed in a block: the row keeps the time it was first stored
        record accId (payment ctx) `dbShouldBeE` True
        (fmap dBTxInfoCreated <$> lift (storedRow accId (fakeTxHash 1))) `dbShouldBeE` (dBTxInfoCreated <$> first)
        listedBlocks accId `dbShouldBeE` [(Store.BlockRef 800000 1, 1)]

    it "stores the time the store gives the transaction" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        (fmap dBTxInfoTime <$> lift (storedRow accId (fakeTxHash 1))) `dbShouldBeE` Just (Just seenTime)

    -- Summaries of version 3 of the database, as the migration copied them
    it "tells the live summaries without their time, for the sync to fetch them again" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        _ <- record accId (spend ctx)
        _ <- record accId (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800002 1) (externalAddr 0) 20000)
        let untime tid = lift $ P.update (DBTxInfoKey accId (hashBytes tid)) [DBTxInfoTime P.=. Nothing]
        untime (fakeTxHash 1)
        untime (fakeTxHash 3)
        lift $ markTxInfoDead accId (fakeTxHash 3)
        untimedSummaries accId `dbShouldBeE` [fakeTxHash 1]

    it "maps the live transactions to their blocks, leaving the dead ones out" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        _ <- record accId (spend ctx)
        txInfoBlockRefs accId `dbShouldBeE` Map.fromList [(fakeTxHash 1, Store.BlockRef 800000 1), (fakeTxHash 2, Store.BlockRef 800001 1)]
        lift $ markTxInfoDead accId (fakeTxHash 2)
        txInfoBlockRefs accId `dbShouldBeE` Map.fromList [(fakeTxHash 1, Store.BlockRef 800000 1)]

    it "brings a dead summary back when the store lists the transaction again" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (spend ctx)
        lift $ markTxInfoDead accId (fakeTxHash 2)
        listedDead accId `dbShouldBeE` [True]
        record accId (spend ctx) `dbShouldBeE` True
        listedDead accId `dbShouldBeE` [False]
        txInfoBlockRefs accId `dbShouldBeE` Map.fromList [(fakeTxHash 2, Store.BlockRef 800001 1)]

    it "marks nothing when the transaction to mark dead is not there" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        lift $ markTxInfoDead accId (fakeTxHash 9)
        listedDead accId `dbShouldBeE` [False]

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the transactions" $ do
    let cfg = def :: Config
        -- Four payments: two in block 800000, one in block 800001, one in
        -- the mempool
        fourPayments accId = do
          _ <- record accId (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
          _ <- record accId (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800000 5) (externalAddr 0) 50000)
          _ <- record accId (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800001 0) (externalAddr 0) 50000)
          _ <- record accId (creditTx ctx (fakeTxHash 4) (Store.MemRef 1700000000) (externalAddr 0) 50000)
          return ()
    it "lists the mempool first, then the newest blocks first, and within a block the last first" $
      inDatabaseE $ do
        accId <- account ctx
        fourPayments accId
        listedIds accId (Page 10 0) `dbShouldBeE` [fakeTxHash 4, fakeTxHash 3, fakeTxHash 2, fakeTxHash 1]

    it "lists them a page at a time" $
      inDatabaseE $ do
        accId <- account ctx
        fourPayments accId
        listedIds accId (Page 2 0) `dbShouldBeE` [fakeTxHash 4, fakeTxHash 3]
        listedIds accId (Page 2 2) `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]
        listedIds accId (Page 2 4) `dbShouldBeE` []

    it "counts the confirmations from the best block, one when it is unknown or behind, none in the mempool" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
        _ <- record accId (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800002 1) (externalAddr 0) 50000)
        _ <- record accId (creditTx ctx (fakeTxHash 3) (Store.MemRef 1700000000) (externalAddr 0) 50000)
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 1)]
        lift $ updateBest btc (fakeBlockHash 0) 800002
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 3)]
        -- The best block of another network does not count
        lift $ updateBest btcTest (fakeBlockHash 0) 900000
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 3)]
        -- A best block behind a transaction's block still confirms it once
        lift $ updateBest btc (fakeBlockHash 0) 800001
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 2)]

    it "lists each transaction with the time the store gives it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        (map (chainTime . chainOf) <$> txsPage accId (Page 5 0)) `dbShouldBeE` [Just seenTime]

    it "gives a dead transaction no confirmations, and keeps its block" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        lift $ updateBest btc (fakeBlockHash 0) 800002
        lift $ markTxInfoDead accId (fakeTxHash 1)
        (map chainOf <$> txsPage accId (Page 5 0)) `dbShouldBeE` [ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 0 True]

    it "fills in the labels of the account's addresses as they are today" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId "Rent"
        _ <- nextChangeAddress ctx cfg accId []
        _ <- record accId (spend ctx)
        listedLabels accId `dbShouldBeE` [(["Rent"], [""])]
        _ <- setAddrLabel accId 0 "Salary"
        listedLabels accId `dbShouldBeE` [(["Salary"], [""])]

    it "refuses a stored summary that does not decode, naming the transaction" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ P.insert_ $ DBTxInfo accId (hashBytes (fakeTxHash 9)) (S.encode (Store.BlockRef 800000 1)) (Just seenTime) "nonsense" False fixtureTime
        lift $
          dbShouldFailWith "The stored summary of transaction 0000000000000000000000000000000000000000000000000000000000000009 does not decode" $
            txsPage accId (Page 5 0)

    it "lists the transactions of the account alone" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- record accId (payment ctx)
        -- A payment to the second wallet's first address
        let otherPaths = Map.fromList [(externalAddr2, Deriv :/ 0 :/ 0)]
            otherPayment = creditTx ctx (fakeTxHash 8) (Store.BlockRef 800000 2) externalAddr2 70000
        _ <- lift $ repsertSummary btc otherId (fakeTxHash 8) (Store.BlockRef 800000 2) seenTime (storeSummary btc otherPaths otherPayment)
        listedIds accId (Page 5 0) `dbShouldBeE` [fakeTxHash 1]
        listedIds otherId (Page 5 0) `dbShouldBeE` [fakeTxHash 8]

historySpec :: Ctx -> Spec
historySpec ctx =
  describe "The history of a coin" $ do
    let cfg = def :: Config
    it "shows the payment to a labelled address and its spending, newest first, through a reorganisation" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId "Rent"
        _ <- nextChangeAddress ctx cfg accId []
        -- The sync records both transactions and the best block
        record accId (payment ctx) `dbShouldBeE` True
        record accId (spend ctx) `dbShouldBeE` True
        lift $ updateBest btc (fakeBlockHash 0) 800002
        txsPage accId (Page 5 0)
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)),
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
                      ]
        -- A reorganisation leaves the spending out: the payment stands
        lift $ markTxInfoDead accId (fakeTxHash 2)
        txsPage accId (Page 5 0)
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 0 True)),
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
                      ]
        -- The spending is mined again, in the next block
        record accId (spendAt ctx (Store.BlockRef 800002 4)) `dbShouldBeE` True
        txsPage accId (Page 5 0)
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800002 4) (Just seenTime) 1 False)),
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False))
                      ]
