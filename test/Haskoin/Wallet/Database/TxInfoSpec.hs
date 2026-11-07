{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The transaction summaries: what a transaction of the store means to an
-- account, how hw writes a transaction as JSON, and the summaries kept in
-- the database and listed by txs. The account is account 0 of the test
-- wallet. The story of these tests is a payment of 50000 received at its
-- first receiving address, then spent: 40000 to an outside address, 9000
-- of change to its first change address, and 1000 of fee.
module Haskoin.Wallet.Database.TxInfoSpec (spec) where

import Conduit (liftIO)
import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (Object, Value (Array, Null, Object), eitherDecode, encode)
import Data.Foldable (toList)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (getAccountById)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Database.TxLabel (setTxLabel)
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
  balanceSpec ctx
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

-- What the payment and the spending mean to the account are
-- 'paymentSummary' and 'spendSummary', shared in TestUtils

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
listedIds :: DBAccountId -> Maybe (Store.BlockRef, TxHash) -> Maybe Natural -> ExceptT String (DB IO) [TxHash]
listedIds accId afterM countM = map (chainTxid . chainOf) <$> txsAfter accId afterM countM

-- | The block and the confirmations of each listed transaction
listedBlocks :: DBAccountId -> ExceptT String (DB IO) [(Store.BlockRef, Natural)]
listedBlocks accId = do
  infos <- txsAfter accId Nothing Nothing
  return [(chainBlock (chainOf info), chainConfirmations (chainOf info)) | info <- infos]

-- | Whether each listed transaction is dead
listedDead :: DBAccountId -> ExceptT String (DB IO) [Bool]
listedDead accId = map (chainDead . chainOf) <$> txsAfter accId Nothing Nothing

-- | The labels of the account's inputs and of its outputs in each listed
-- transaction
listedLabels :: DBAccountId -> ExceptT String (DB IO) [([Text], [Text])]
listedLabels accId = do
  infos <- txsAfter accId Nothing Nothing
  return
    [ (myInputsLabel <$> Map.elems summary.summaryMyInputs, myOutputsLabel <$> Map.elems summary.summaryMyOutputs)
      | info <- infos,
        let summary = txInfoSummary info
    ]

-- | The balance after each listed transaction
listedBalances :: DBAccountId -> Maybe TxsCursor -> ExceptT String (DB IO) [Maybe Integer]
listedBalances accId afterM = map (listedTxBalance . fst) <$> listedTxsAfter accId afterM Nothing

-- | Give the account these confirmed and unconfirmed balances, as a sync
-- does
holding :: DBAccountId -> Word64 -> Word64 -> ExceptT String (DB IO) ()
holding accId confirmed unconfirmed =
  lift $ P.update accId [DBAccountBalanceConfirmed P.=. confirmed, DBAccountBalanceUnconfirmed P.=. unconfirmed]

-- | Two transaction ids as hw shows them, which start with ab and with cd
idAb, idCd :: TxHash
idAb = typedTxid "ab00000000000000000000000000000000000000000000000000000000000000"
idCd = typedTxid "cd00000000000000000000000000000000000000000000000000000000000000"

-- | A transaction id as the user types it
typedTxid :: Text -> TxHash
typedTxid = fromMaybe (error "typedTxid") . hexToTxHash

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

    it "carries the lock time of the transaction, as the store reports it" $ do
      let locked = storeTxLockedAt 814037 (fakeTxHash 1) (Store.BlockRef 800000 1) [inputFrom ctx (fakeTxHash 0xee, 0) (otherAddr 0) 51000] [outputTo ctx (externalAddr 0) 50000] 1000
      summaryLockTime (summarized locked) `shouldBe` 814037
      summaryLockTime (summarized (payment ctx)) `shouldBe` 0

    it "sees an address the account does not have as an outsider's" $ do
      let summary = storeSummary btc Map.empty (payment ctx)
      involvesWallet summary `shouldBe` False
      summaryAmount summary `shouldBe` 0
      summaryOtherOutputs summary `shouldBe` Map.fromList [(externalAddr 0, 50000)]
      involvesWallet paymentSummary `shouldBe` True

    it "sets apart an output at an address the network cannot write, a SegWit one on Bitcoin Cash, keeping no address for it" $ do
      -- The store names the address whatever the network; the summary only
      -- keeps what the network can write, since that is what it stores.
      -- The output is kept as one that pays no address.
      let paid = outputTo ctx segwitAddr 30000
          tx = storeTx (fakeTxHash 9) (Store.BlockRef 800000 4) [inputFrom ctx (fakeTxHash 0xee, 0) (otherAddr 0) 31000] [paid] 1000
          onBch = storeSummary bch Map.empty tx
      summaryOtherOutputs onBch `shouldBe` Map.empty
      summaryNonStdOutputs onBch `shouldBe` [Store.StoreOutput 30000 (addressScript ctx segwitAddr) Nothing Nothing]
      summaryOtherOutputs (storeSummary btc Map.empty tx) `shouldBe` Map.fromList [(segwitAddr, 30000)]

    it "sets apart an input from an address the network cannot write, keeping no address for it" $ do
      let spent = inputFrom ctx (fakeTxHash 0xee, 0) segwitAddr 31000
          tx = storeTx (fakeTxHash 9) (Store.BlockRef 800000 4) [spent] [outputTo ctx (otherAddr 0) 30000] 1000
          onBch = storeSummary bch Map.empty tx
      summaryOtherInputs onBch `shouldBe` Map.empty
      summaryNonStdInputs onBch `shouldBe` [Store.StoreInput (OutPoint (fakeTxHash 0xee) 0) 0xffffffff BS.empty (addressScript ctx segwitAddr) 31000 [] Nothing]

    -- The JSON of a summary writes every address it holds on its network,
    -- which has no text for a SegWit address on Bitcoin Cash
    it "writes the summary of such a transaction in JSON, the output and the input without an address" $ do
      let tx = storeTx (fakeTxHash 9) (Store.BlockRef 800000 4) [inputFrom ctx (fakeTxHash 0xee, 0) segwitAddr 31000] [outputTo ctx segwitAddr 30000] 1000
          json = encode (marshalValue bch (storeSummary bch Map.empty tx))
          addressesOf key = case eitherDecode json of
            Right (Object o) | Just (Array items) <- KeyMap.lookup key o -> [KeyMap.lookup "address" item | Object item <- toList items]
            _ -> []
      addressesOf "nonstdoutputs" `shouldBe` [Just Null]
      addressesOf "nonstdinputs" `shouldBe` [Just Null]

statusSpec :: Spec
statusSpec =
  describe "The status of a transaction" $ do
    it "knows the id of a transaction on the chain" $
      txInfoHash (TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False)) "" Map.empty) `shouldBe` Just (fakeTxHash 2)

    it "gives a transaction in preparation its id once it is signed" $ do
      let unsigned = PendingStatus (fakeTxHash 7) Nothing False False Nothing []
          signed = PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False Nothing []
      (pendingSigned unsigned, txInfoHash (TxInfo spendSummary (Pending unsigned) "" Map.empty)) `shouldBe` (False, Nothing)
      (pendingSigned signed, txInfoHash (TxInfo spendSummary (Pending signed) "" Map.empty)) `shouldBe` (True, Just (fakeTxHash 2))

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
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False)) "" Map.empty
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
              "locktime": 0,
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
              "pending": null,
              "label": "",
              "inputlabels": {}
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc info)
      unmarshalJSON btc (cs json) `shouldBe` Just info

    -- Stored before hw kept the time, until the next sync
    it "writes a transaction on the chain without its time as null" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) Nothing 3 False)) "" Map.empty
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info
      let written = eitherDecode (marshalJSON btc info) :: Either String Object
      (KeyMap.lookup "time" <$> written) `shouldBe` Right (Just Null)

    it "writes a transaction in preparation with its state, and the block of one that is nowhere yet" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 1)) True False (Just seenTime) [])) "" Map.empty
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
              "locktime": 0,
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
              },
              "label": "",
              "inputlabels": {}
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc info)
      unmarshalJSON btc (cs json) `shouldBe` Just info

    it "writes the label of a transaction among its other keys" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "Salary" Map.empty
      (KeyMap.lookup "label" <$> (eitherDecode (marshalJSON btc info) :: Either String Object)) `shouldBe` Right (Just "Salary")
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info

    -- As hw wrote it before transactions had labels
    it "reads a transaction written without a label as one without a label" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
      (unmarshalJSON btc . encode . Object . KeyMap.delete "label" <$> eitherDecode (marshalJSON btc info)) `shouldBe` Right (Just info)

    -- One the wallet does not hold, read from a file, has none
    it "writes no creation time for a transaction in preparation that has none as null" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info
      (KeyMap.lookup "created" <$> (pendingOf =<< eitherDecode (marshalJSON btc info))) `shouldBe` Right (Just Null)

    -- As hw wrote it before it kept the time
    it "reads a transaction in preparation without its creation time" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty
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
        listedIds accId Nothing Nothing `dbShouldBeE` [fakeTxHash 4, fakeTxHash 3, fakeTxHash 2, fakeTxHash 1]

    it "lists them a page at a time, each page after the last transaction of the one before" $
      inDatabaseE $ do
        accId <- account ctx
        fourPayments accId
        listedIds accId Nothing (Just 2) `dbShouldBeE` [fakeTxHash 4, fakeTxHash 3]
        listedIds accId (Just (Store.BlockRef 800001 0, fakeTxHash 3)) (Just 2) `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]
        listedIds accId (Just (Store.BlockRef 800000 1, fakeTxHash 1)) (Just 2) `dbShouldBeE` []

    it "names the block of a transaction of the account, for --after, and refuses one the account does not have" $
      inDatabaseE $ do
        accId <- account ctx
        fourPayments accId
        txBlock accId (fakeTxHash 3) `dbShouldBeE` Store.BlockRef 800001 0
        txBlock accId (fakeTxHash 4) `dbShouldBeE` Store.MemRef 1700000000
        lift $
          dbShouldFailWith "The transaction 0000000000000000000000000000000000000000000000000000000000000005 is not in the account" $
            txBlock accId (fakeTxHash 5)

    it "counts the confirmations from the best block, one when it is unknown or behind, none in the mempool" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
        _ <- record accId (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800002 1) (externalAddr 0) 50000)
        _ <- record accId (creditTx ctx (fakeTxHash 3) (Store.MemRef 1700000000) (externalAddr 0) 50000)
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 1)]
        lift $ updateBest btc (fakeBlockHash 0) 800002 1700000000
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 3)]
        -- The best block of another network does not count
        lift $ updateBest btcTest (fakeBlockHash 0) 900000 1700000000
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 3)]
        -- A best block behind a transaction's block still confirms it once
        lift $ updateBest btc (fakeBlockHash 0) 800001 1700000000
        listedBlocks accId `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 2)]

    it "lists each transaction with the time the store gives it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        (map (chainTime . chainOf) <$> txsAfter accId Nothing Nothing) `dbShouldBeE` [Just seenTime]

    it "gives a dead transaction no confirmations, and keeps its block" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        lift $ updateBest btc (fakeBlockHash 0) 800002 1700000000
        lift $ markTxInfoDead accId (fakeTxHash 1)
        (map chainOf <$> txsAfter accId Nothing Nothing) `dbShouldBeE` [ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 0 True]

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
        lift $ P.insert_ $ DBTxInfo accId (hashBytes (fakeTxHash 9)) (S.encode (Store.BlockRef 800000 1)) (Just seenTime) 0 "nonsense" False fixtureTime
        lift $
          dbShouldFailWith "The stored summary of transaction 0000000000000000000000000000000000000000000000000000000000000009 does not decode" $
            txsAfter accId Nothing Nothing

    it "gives each transaction the label the account gave it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        _ <- record accId (spend ctx)
        lift $ setTxLabel accId (fakeTxHash 1) "Salary"
        -- The spending first: it is in the newer block
        (map txInfoLabel <$> txsAfter accId Nothing Nothing) `dbShouldBeE` ["", "Salary"]

    -- The spending spends the coin of the payment
    it "fills in the labels the coins each transaction spends show, by the transaction that paid them" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        _ <- record accId (spend ctx)
        lift $ setTxLabel accId (fakeTxHash 1) "Salary"
        (map txInfoInputLabels <$> txsAfter accId Nothing Nothing) `dbShouldBeE` [Map.fromList [(fakeTxHash 1, "Salary")], Map.empty]

    it "shows on a coin the label of the payment that paid it to the account" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        lift $ setTxLabel accId (fakeTxHash 1) "Salary"
        lift $ coinLabel btc accId (fakeTxHash 1) `dbShouldBe` "Salary"

    -- The label says what the payment was for, not what its change is
    it "shows no label on the change of a payment the account made" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (spend ctx)
        lift $ setTxLabel accId (fakeTxHash 2) "Rent"
        lift $ coinLabel btc accId (fakeTxHash 2) `dbShouldBe` ""

    it "shows no label on a coin of a transaction the account has no label for, or does not have" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        lift $ coinLabel btc accId (fakeTxHash 1) `dbShouldBe` ""
        lift $ setTxLabel accId (fakeTxHash 3) "Elsewhere"
        lift $ coinLabel btc accId (fakeTxHash 3) `dbShouldBe` ""

    -- The sync rewrites a summary when its transaction moves to another
    -- block, and marks it dead when the transaction is replaced
    it "keeps the label of a transaction whose summary the sync rewrites, then marks dead" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (spend ctx)
        lift $ setTxLabel accId (fakeTxHash 2) "Rent"
        _ <- record accId (spendAt ctx (Store.BlockRef 800002 4))
        (map txInfoLabel <$> txsAfter accId Nothing Nothing) `dbShouldBeE` ["Rent"]
        lift $ markTxInfoDead accId (fakeTxHash 2)
        (map txInfoLabel <$> txsAfter accId Nothing Nothing) `dbShouldBeE` ["Rent"]

    -- Without a best block, a transaction in a block has one confirmation
    it "gives one transaction of the account as txs lists it, with its label" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        lift $ setTxLabel accId (fakeTxHash 1) "Salary"
        storedTxInfo accId (fakeTxHash 1)
          `dbShouldBeE` Just (TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False)) "Salary" Map.empty)

    it "gives no transaction the account does not have" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- record accId (payment ctx)
        storedTxInfo accId (fakeTxHash 9) `dbShouldBeE` Nothing

    it "finds the ids of the account that start with the digits typed, in either case, for the shell" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ do
          _ <- repsertSummary btc accId idAb (Store.BlockRef 800000 1) seenTime paymentSummary
          _ <- repsertSummary btc accId idCd (Store.BlockRef 800000 2) seenTime paymentSummary
          txidsStarting accId "ab" 21 `dbShouldBe` [idAb]
          txidsStarting accId "AB" 21 `dbShouldBe` [idAb]
          txidsStarting accId "ef" 21 `dbShouldBe` []
          -- In the order of the ids, up to the count
          txidsStarting accId "" 21 `dbShouldBe` [idAb, idCd]
          txidsStarting accId "" 1 `dbShouldBe` [idAb]

    it "lists the transactions of the account alone" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- record accId (payment ctx)
        -- A payment to the second wallet's first address
        let otherPaths = Map.fromList [(externalAddr2, Deriv :/ 0 :/ 0)]
            otherPayment = creditTx ctx (fakeTxHash 8) (Store.BlockRef 800000 2) externalAddr2 70000
        _ <- lift $ repsertSummary btc otherId (fakeTxHash 8) (Store.BlockRef 800000 2) seenTime (storeSummary btc otherPaths otherPayment)
        listedIds accId Nothing Nothing `dbShouldBeE` [fakeTxHash 1]
        listedIds otherId Nothing Nothing `dbShouldBeE` [fakeTxHash 8]

balanceSpec :: Ctx -> Spec
balanceSpec ctx =
  describe "The balances of txs" $ do
    -- The payment and its spending: 50000 - 41000 = 9000 left
    let paidAndSpent = do
          accId <- account ctx
          _ <- record accId (payment ctx)
          _ <- record accId (spend ctx)
          holding accId 9000 0
          return accId
        -- A payment of 70000, one block above the spending
        laterPayment = creditTx ctx (fakeTxHash 3) (Store.BlockRef 800002 1) (externalAddr 0) 70000
        -- A payment of 70000 listed between the payment and its spending
        paymentBetween = creditTx ctx (fakeTxHash 3) (Store.BlockRef 800000 5) (externalAddr 0) 70000

    it "gives the newest transaction what the account holds, confirmed or not" $
      inDatabaseE $ do
        accId <- paidAndSpent
        holding accId 6000 3000
        -- 6000 + 3000 = 9000
        (take 1 <$> listedBalances accId Nothing) `dbShouldBeE` [Just 9000]

    it "gives each older transaction the balance without the one above it" $
      inDatabaseE $ do
        accId <- paidAndSpent
        -- The spending took 41000: 9000 + 41000 = 50000
        listedBalances accId Nothing `dbShouldBeE` [Just 9000, Just 50000]

    it "gives a dead transaction no balance, and takes nothing off for it" $
      inDatabaseE $ do
        accId <- paidAndSpent
        _ <- record accId paymentBetween
        lift $ markTxInfoDead accId (fakeTxHash 3)
        listedBalances accId Nothing `dbShouldBeE` [Just 9000, Nothing, Just 50000]

    it "hands each transaction on with the balance before it, a dead one included" $
      inDatabaseE $ do
        accId <- paidAndSpent
        _ <- record accId paymentBetween
        lift $ markTxInfoDead accId (fakeTxHash 3)
        (map snd <$> listedTxsAfter accId Nothing Nothing) `dbShouldBeE` [50000, 50000, 0]

    it "goes on from the balance the cursor carries, for the next chunk" $
      inDatabaseE $ do
        accId <- paidAndSpent
        listedBalances accId (Just (TxsCursor (Store.BlockRef 800001 1) (fakeTxHash 2) 12345)) `dbShouldBeE` [Just 12345]

    it "starts after a transaction with the balance before it, for --after" $
      inDatabaseE $ do
        accId <- paidAndSpent
        acc <- getAccountById accId
        -- 9000 + 41000 = 50000
        txsCursorAt accId acc (fakeTxHash 2) `dbShouldBeE` TxsCursor (Store.BlockRef 800001 1) (fakeTxHash 2) 50000

    it "takes off the transactions above the one --after names" $
      inDatabaseE $ do
        accId <- paidAndSpent
        _ <- record accId laterPayment
        holding accId 79000 0
        acc <- getAccountById accId
        -- 79000 - 70000 + 41000 = 50000
        txsCursorAt accId acc (fakeTxHash 2) `dbShouldBeE` TxsCursor (Store.BlockRef 800001 1) (fakeTxHash 2) 50000

    it "takes off no dead transaction for --after" $
      inDatabaseE $ do
        accId <- paidAndSpent
        _ <- record accId laterPayment
        lift $ markTxInfoDead accId (fakeTxHash 3)
        acc <- getAccountById accId
        -- 9000 + 41000 = 50000, the dead 70000 left out
        txsCursorAt accId acc (fakeTxHash 2) `dbShouldBeE` TxsCursor (Store.BlockRef 800001 1) (fakeTxHash 2) 50000

    it "takes off the transaction --after names even when it is dead: nothing" $
      inDatabaseE $ do
        accId <- paidAndSpent
        _ <- record accId laterPayment
        lift $ markTxInfoDead accId (fakeTxHash 3)
        acc <- getAccountById accId
        txsCursorAt accId acc (fakeTxHash 3) `dbShouldBeE` TxsCursor (Store.BlockRef 800002 1) (fakeTxHash 3) 9000

    -- Two transactions of one block go by their ids: ab before cd
    it "takes off the transactions of one block as far as the one --after names, in the order of their ids" $
      inDatabaseE $ do
        accId <- account ctx
        lift $ do
          _ <- repsertSummary btc accId idAb (Store.MemRef 1700000000) seenTime paymentSummary
          void $ repsertSummary btc accId idCd (Store.MemRef 1700000000) seenTime spendSummary
        holding accId 9000 0
        acc <- getAccountById accId
        -- 9000 - 50000 = -41000: listed in this order, the balance dips
        txsCursorAt accId acc idAb `dbShouldBeE` TxsCursor (Store.MemRef 1700000000) idAb (-41000)
        -- -41000 + 41000 = 0
        txsCursorAt accId acc idCd `dbShouldBeE` TxsCursor (Store.MemRef 1700000000) idCd 0

    it "leaves the transactions of other accounts out of what --after adds up" $
      inDatabaseE $ do
        accId <- paidAndSpent
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- lift $ repsertSummary btc otherId (fakeTxHash 8) (Store.BlockRef 800002 1) seenTime paymentSummary
        acc <- getAccountById accId
        txsCursorAt accId acc (fakeTxHash 2) `dbShouldBeE` TxsCursor (Store.BlockRef 800001 1) (fakeTxHash 2) 50000

    it "stores what a transaction adds or takes in its row, for --after to add up" $
      inDatabaseE $ do
        accId <- paidAndSpent
        (fmap dBTxInfoAmount <$> lift (storedRow accId (fakeTxHash 1))) `dbShouldBeE` Just 50000
        (fmap dBTxInfoAmount <$> lift (storedRow accId (fakeTxHash 2))) `dbShouldBeE` Just (-41000)

    it "stores the new amount when the sync rebuilds a summary" $
      inDatabaseE $ do
        accId <- paidAndSpent
        _ <- lift $ repsertSummary btc accId (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime paymentSummary
        (fmap dBTxInfoAmount <$> lift (storedRow accId (fakeTxHash 2))) `dbShouldBeE` Just 50000

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
        lift $ updateBest btc (fakeBlockHash 0) 800002 1700000000
        txsAfter accId Nothing Nothing
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty,
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
                      ]
        -- A reorganisation leaves the spending out: the payment stands
        lift $ markTxInfoDead accId (fakeTxHash 2)
        txsAfter accId Nothing Nothing
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 0 True)) "" Map.empty,
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
                      ]
        -- The spending is mined again, in the next block
        record accId (spendAt ctx (Store.BlockRef 800002 4)) `dbShouldBeE` True
        txsAfter accId Nothing Nothing
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800002 4) (Just seenTime) 1 False)) "" Map.empty,
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty
                      ]
