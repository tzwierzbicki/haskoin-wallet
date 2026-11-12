{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The transaction summaries: what a transaction of the store means to a
-- portfolio on its network, a pool, how hw writes a transaction as JSON,
-- and the summaries kept in the database and listed by txs. The pool is
-- what portfolio 0 of the test wallet holds on Bitcoin. The story of
-- these tests is a payment of 50000 received at the first receiving
-- address of its bip44 account, then spent: 40000 to an outside address,
-- 9000 of change to its first change address, and 1000 of fee.
module Haskoin.Wallet.Database.TxInfoSpec (spec) where

import Conduit (liftIO)
import Control.Monad (forM_, void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Aeson (Object, Value (Array, Null, Object), eitherDecode, encode, toJSON)
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.Word (Word64)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account (poolAccountOf)
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), portfolioPool)
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), legacyPath)
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
  jsonSpec ctx
  storingSpec ctx
  elsewhereSpec ctx
  standingSpec
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

-- | The addresses of the pool with their paths, as the sync hands them
-- to the summary: the first receiving and the first change address of its
-- bip44 account
ourPaths :: Map Address PoolPath
ourPaths = Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0)), (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0))]

-- | The same addresses with the first receiving address of the bip84
-- account of the pool
bothTypesPaths :: Map Address PoolPath
bothTypesPaths = Map.insert (segwitExternalAddr 0) (PoolPath Bip84 (Deriv :/ 0 :/ 0)) ourPaths

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

-- | A payment between two portfolios of the wallet, transaction 2: the
-- coin of the payment, of portfolio 0, pays 40000 to the first receiving
-- address of portfolio 1, with 9000 of change to the first change address
-- of portfolio 0 and 1000 of fee
betweenPortfolios :: Ctx -> Store.Transaction
betweenPortfolios ctx = spendingStoreTx ctx (fakeTxHash 2) (Store.BlockRef 800001 1) (fakeTxHash 1, 0) (externalAddr 0) [(account1Addr, 40000), (internalAddr 0, 9000)]

-- What the payment and the spending mean to the portfolio are
-- 'paymentSummary' and 'spendSummary', shared in TestUtils

{- Helpers -}

-- | The summary of a transaction of the store, as the sync builds it
summarized :: Store.Transaction -> TxSummary
summarized = storeSummary btc ourPaths

-- | Store the summary of a transaction for the pool, as the sync does
record :: Pool -> Store.Transaction -> ExceptT String (DB IO) Bool
record pool tx = lift $ repsertSummary pool tx.txid tx.block (storeTime tx.timestamp) (summarized tx)

-- | The id of each listed transaction, in the order of the listing: those
-- after the transaction given, or from the top, up to the count
listedIds :: Pool -> Maybe TxHash -> Maybe Natural -> ExceptT String (DB IO) [TxHash]
listedIds pool afterM countM = map (chainTxid . chainOf) <$> txsAfter pool afterM countM

-- | The balance txs goes on from after a transaction: the balance before
-- that transaction
balanceBefore :: Pool -> TxHash -> ExceptT String (DB IO) Integer
balanceBefore pool tid = txsCursorBalance <$> txsCursorAt pool tid

-- | The block and the confirmations of each listed transaction
listedBlocks :: Pool -> ExceptT String (DB IO) [(Store.BlockRef, Natural)]
listedBlocks pool = do
  infos <- txsAfter pool Nothing Nothing
  return [(chainBlock (chainOf info), chainConfirmations (chainOf info)) | info <- infos]

-- | Whether each listed transaction is dead
listedDead :: Pool -> ExceptT String (DB IO) [Bool]
listedDead pool = map (chainDead . chainOf) <$> txsAfter pool Nothing Nothing

-- | The labels of the portfolio's inputs and of its outputs in each listed
-- transaction
listedLabels :: Pool -> ExceptT String (DB IO) [([Text], [Text])]
listedLabels pool = do
  infos <- txsAfter pool Nothing Nothing
  return
    [ (myInputsLabel <$> Map.elems summary.summaryMyInputs, myOutputsLabel <$> Map.elems summary.summaryMyOutputs)
      | info <- infos,
        let summary = txInfoSummary info
    ]

-- | The balance after each listed transaction
listedBalances :: Pool -> Maybe TxsCursor -> ExceptT String (DB IO) [Maybe Integer]
listedBalances pool afterM = map (listedTxBalance . fst) <$> listedTxsAfter pool afterM Nothing

-- | Give the bip44 account of the pool these confirmed and unconfirmed
-- balances, as a sync does
holding :: Pool -> Word64 -> Word64 -> ExceptT String (DB IO) ()
holding pool = holdingIn pool Bip44

-- | Give the account of a type of the pool these confirmed and unconfirmed
-- balances, as a sync does
holdingIn :: Pool -> AccountKind -> Word64 -> Word64 -> ExceptT String (DB IO) ()
holdingIn pool kind confirmed unconfirmed = do
  (accId, _) <- poolAccountOf pool kind
  lift $ P.update accId [DBAccountBalanceConfirmed P.=. confirmed, DBAccountBalanceUnconfirmed P.=. unconfirmed]

-- | Two transaction ids as hw shows them, which start with ab and with cd
idAb, idCd :: TxHash
idAb = typedTxid "ab00000000000000000000000000000000000000000000000000000000000000"
idCd = typedTxid "cd00000000000000000000000000000000000000000000000000000000000000"

-- | The lowest and the highest txids of the short id ab000000, and the
-- lowest of the next short id, ab000001
lowestAb, highestAb, nextAb :: TxHash
lowestAb = idAb
highestAb = typedTxid "ab000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
nextAb = typedTxid "ab00000100000000000000000000000000000000000000000000000000000000"

-- | The row of a transaction of the pool
storedRow :: Pool -> TxHash -> DB IO (Maybe DBTxInfo)
storedRow pool tid = P.get (txInfoKey pool tid)

summarySpec :: Ctx -> Spec
summarySpec ctx =
  describe "Summarizing a transaction of the store" $ do
    it "describes a payment received as a credit of the amount" $
      summarized (payment ctx) `shouldBe` paymentSummary

    it "describes a payment made as a debit of the amount and the fee, with the coin spent and the change" $
      summarized (spend ctx) `shouldBe` spendSummary

    it "describes a transfer between the portfolio's own addresses as internal, costing the fee" $ do
      let transfer = spendingStoreTx ctx (fakeTxHash 3) (Store.BlockRef 800001 2) (fakeTxHash 1, 0) (externalAddr 0) [(internalAddr 0, 49000)]
          summary = summarized transfer
      (summaryType summary, summaryAmount summary, summaryFee summary) `shouldBe` (TxInternal, -1000, 1000)

    -- The coin of 50000 at the first receiving address of the bip44
    -- account goes to the first receiving address of the bip84 account of
    -- the same portfolio, less the fee of 1000
    it "describes a payment from a bip44 address to a bip84 address of the same portfolio as internal, costing the fee" $ do
      let transfer = spendingStoreTx ctx (fakeTxHash 3) (Store.BlockRef 800001 2) (fakeTxHash 1, 0) (externalAddr 0) [(segwitExternalAddr 0, 49000)]
          summary = storeSummary btc bothTypesPaths transfer
      (summaryType summary, summaryAmount summary, summaryFee summary) `shouldBe` (TxInternal, -1000, 1000)

    it "gives each address of the portfolio in a transaction the type of its account" $ do
      let transfer = spendingStoreTx ctx (fakeTxHash 3) (Store.BlockRef 800001 2) (fakeTxHash 1, 0) (externalAddr 0) [(segwitExternalAddr 0, 49000)]
          summary = storeSummary btc bothTypesPaths transfer
      (myInputsPath <$> summaryMyInputs summary) `shouldBe` Map.fromList [(externalAddr 0, PoolPath Bip44 (Deriv :/ 0 :/ 0))]
      (myOutputsPath <$> summaryMyOutputs summary) `shouldBe` Map.fromList [(segwitExternalAddr 0, PoolPath Bip84 (Deriv :/ 0 :/ 0))]

    -- Portfolio 0 spends its coin of 50000: 40000 to the first receiving
    -- address of portfolio 1, 9000 of change, 1000 of fee
    it "describes a payment from portfolio 0 to portfolio 1 as a debit in portfolio 0" $ do
      let summary = storeSummary btc ourPaths (betweenPortfolios ctx)
      -- 40000 paid + 1000 of fee = 41000
      (summaryType summary, summaryAmount summary) `shouldBe` (TxDebit, -41000)
      summaryOtherOutputs summary `shouldBe` Map.fromList [(account1Addr, 40000)]

    it "describes the same payment as a credit in portfolio 1" $ do
      let summary = storeSummary btc (Map.fromList [(account1Addr, legacyPath (Deriv :/ 0 :/ 0))]) (betweenPortfolios ctx)
      (summaryType summary, summaryAmount summary) `shouldBe` (TxCredit, 40000)
      summaryOtherInputs summary `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    it "names the types of the portfolio's addresses a transaction involves, each once, bip44 first" $ do
      summaryKinds paymentSummary `shouldBe` [Bip44]
      summaryKinds spendSummary `shouldBe` [Bip44]
      let transfer = spendingStoreTx ctx (fakeTxHash 3) (Store.BlockRef 800001 2) (fakeTxHash 1, 0) (externalAddr 0) [(segwitExternalAddr 0, 49000)]
      summaryKinds (storeSummary btc bothTypesPaths transfer) `shouldBe` [Bip44, Bip84]
      summaryKinds (storeSummary btc Map.empty (payment ctx)) `shouldBe` []

    it "tells a credit from a debit from an internal transfer by what the portfolio loses" $ do
      txType 50000 1000 `shouldBe` TxCredit
      txType (-41000) 1000 `shouldBe` TxDebit
      txType (-1000) 1000 `shouldBe` TxInternal

    it "adds up several outputs to one address" $ do
      let twice = payoutTx ctx (fakeTxHash 4) (Store.BlockRef 800000 2) [(externalAddr 0, 20000), (externalAddr 0, 30000)]
          summary = summarized twice
      summaryMyOutputs summary `shouldBe` Map.fromList [(externalAddr 0, MyOutputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "")]
      summaryAmount summary `shouldBe` 50000

    it "adds up several coins of one address, keeping each coin" $ do
      let coins = [inputFrom ctx (fakeTxHash 1, 0) (externalAddr 0) 30000, inputFrom ctx (fakeTxHash 4, 1) (externalAddr 0) 20000]
          summary = summarized $ storeTx (fakeTxHash 5) (Store.BlockRef 800001 3) coins [outputTo ctx (otherAddr 0) 49000] 1000
      summaryMyInputs summary
        `shouldBe` Map.fromList [(externalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "" [OutPoint (fakeTxHash 1) 0, OutPoint (fakeTxHash 4) 1])]
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

    it "sees an address the portfolio does not have as an outsider's" $ do
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
      txInfoHash (TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False)) "" Map.empty Map.empty) `shouldBe` Just (fakeTxHash 2)

    it "gives a transaction in preparation its id once it is signed" $ do
      let unsigned = PendingStatus (fakeTxHash 7) Nothing False False Nothing []
          signed = PendingStatus (fakeTxHash 7) (Just (fakeTxHash 2)) False False Nothing []
      (pendingSigned unsigned, txInfoHash (TxInfo spendSummary (Pending unsigned) "" Map.empty Map.empty)) `shouldBe` (False, Nothing)
      (pendingSigned signed, txInfoHash (TxInfo spendSummary (Pending signed) "" Map.empty Map.empty)) `shouldBe` (True, Just (fakeTxHash 2))

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

jsonSpec :: Ctx -> Spec
jsonSpec ctx =
  describe "Transactions in JSON" $ do
    it "writes a transaction on the chain as txs prints it, and reads it back" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
          json :: String
          json =
            [r|{
              "network": "btc",
              "types": ["bip44"],
              "txid": "0000000000000000000000000000000000000000000000000000000000000002",
              "type": "debit",
              "amount": -41000,
              "fee": 1000,
              "feebyte": 4,
              "changeinfee": null,
              "size": 226,
              "locktime": 0,
              "myoutputs": {"17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn": {"value": 9000, "path": "/1/0", "type": "bip44", "label": ""}},
              "otheroutputs": {"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU": 40000},
              "nonstdoutputs": [],
              "myinputs": {
                "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {
                  "value": 50000,
                  "path": "/0/0",
                  "type": "bip44",
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
              "inputlabels": {},
              "otherportfolios": {}
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc info)
      unmarshalJSON btc (cs json) `shouldBe` Just info

    -- Stored before hw kept the time, until the next sync
    it "writes a transaction on the chain without its time as null" $ do
      let info = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) Nothing 3 False)) "" Map.empty Map.empty
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info
      let written = eitherDecode (marshalJSON btc info) :: Either String Object
      (KeyMap.lookup "time" <$> written) `shouldBe` Right (Just Null)

    it "writes a transaction in preparation with its state, and the block of one that is nowhere yet" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) (Just (fakeTxHash 1)) True False (Just seenTime) [])) "" Map.empty Map.empty
          json :: String
          json =
            [r|{
              "network": "btc",
              "types": ["bip44"],
              "txid": "0000000000000000000000000000000000000000000000000000000000000001",
              "type": "credit",
              "amount": 50000,
              "fee": 1000,
              "feebyte": 4,
              "changeinfee": null,
              "size": 226,
              "locktime": 0,
              "myoutputs": {"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj": {"value": 50000, "path": "/0/0", "type": "bip44", "label": ""}},
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
              "inputlabels": {},
              "otherportfolios": {}
            }|]
      eitherDecode (cs json) `shouldBe` Right (marshalValue btc info)
      unmarshalJSON btc (cs json) `shouldBe` Just info

    -- The network is that of the pool the transaction is shown for
    it "writes the network of a transaction among its keys" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
      (KeyMap.lookup "network" <$> (eitherDecode (marshalJSON bch info) :: Either String Object)) `shouldBe` Right (Just "bch")
      (KeyMap.lookup "network" <$> (eitherDecode (marshalJSON btcTest4 info) :: Either String Object)) `shouldBe` Right (Just "btctest4")

    it "writes the types of the portfolio's addresses a transaction involves among its keys, both for one between a bip44 and a bip84 address" $ do
      let transfer = spendingStoreTx ctx (fakeTxHash 3) (Store.BlockRef 800001 2) (fakeTxHash 1, 0) (externalAddr 0) [(segwitExternalAddr 0, 49000)]
          info = TxInfo (storeSummary btc bothTypesPaths transfer) (OnChain (ChainStatus (fakeTxHash 3) (Store.BlockRef 800001 2) (Just seenTime) 3 False)) "" Map.empty Map.empty
      (KeyMap.lookup "types" <$> (eitherDecode (marshalJSON btc info) :: Either String Object)) `shouldBe` Right (Just (toJSON ["bip44" :: Text, "bip84"]))
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info

    it "writes no type for a transaction that involves no address of the portfolio" $ do
      let info = TxInfo (storeSummary btc Map.empty (payment ctx)) (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
      (KeyMap.lookup "types" <$> (eitherDecode (marshalJSON btc info) :: Either String Object)) `shouldBe` Right (Just (toJSON ([] :: [Text])))

    it "writes an address of a bip84 account of the portfolio with its type, and reads it back" $ do
      let output = MyOutputs 49000 (PoolPath Bip84 (Deriv :/ 0 :/ 0)) ""
          json :: String
          json = "{\"value\": 49000, \"path\": \"/0/0\", \"type\": \"bip84\", \"label\": \"\"}"
      eitherDecode (cs json) `shouldBe` Right (toJSON output)
      eitherDecode (cs json) `shouldBe` Right output

    it "writes an address of another portfolio with its wallet, its portfolio, its path, its type and its label, and reads it back" $ do
      let held = HeldElsewhere (PortfolioRef 0 1) (legacyPath (Deriv :/ 0 :/ 0)) "Savings"
          json :: String
          json = "{\"wallet\": 0, \"portfolio\": 1, \"path\": \"/0/0\", \"type\": \"bip44\", \"label\": \"Savings\"}"
      eitherDecode (cs json) `shouldBe` Right (toJSON held)
      eitherDecode (cs json) `shouldBe` Right held

    -- The spending pays 40000 to the first receiving address of portfolio
    -- 1 of the wallet, which stays among the outputs to others
    it "writes the addresses another portfolio holds by address under otherportfolios, and reads them back" $ do
      let toPortfolio1 = spendSummary {summaryOtherOutputs = Map.fromList [(account1Addr, 40000)]}
          held = Map.fromList [(account1Addr, HeldElsewhere (PortfolioRef 0 1) (legacyPath (Deriv :/ 0 :/ 0)) "Savings")]
          info = TxInfo toPortfolio1 (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 3 False)) "" Map.empty held
          written = eitherDecode (marshalJSON btc info) :: Either String Object
          json :: String
          json = "{\"17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo\": {\"wallet\": 0, \"portfolio\": 1, \"path\": \"/0/0\", \"type\": \"bip44\", \"label\": \"Savings\"}}"
          outputs :: String
          outputs = "{\"17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo\": 40000}"
      (KeyMap.lookup "otherportfolios" <$> written) `shouldBe` (Just <$> eitherDecode (cs json))
      (KeyMap.lookup "otheroutputs" <$> written) `shouldBe` (Just <$> eitherDecode (cs outputs))
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info

    it "writes the label of a transaction among its other keys" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "Salary" Map.empty Map.empty
      (KeyMap.lookup "label" <$> (eitherDecode (marshalJSON btc info) :: Either String Object)) `shouldBe` Right (Just "Salary")
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info

    -- As hw wrote it before transactions had labels
    it "reads a transaction written without a label as one without a label" $ do
      let info = TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
      (unmarshalJSON btc . encode . Object . KeyMap.delete "label" <$> eitherDecode (marshalJSON btc info)) `shouldBe` Right (Just info)

    -- One the wallet does not hold, read from a file, has none
    it "writes no creation time for a transaction in preparation that has none as null" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
      unmarshalJSON btc (marshalJSON btc info) `shouldBe` Just info
      (KeyMap.lookup "created" <$> (pendingOf =<< eitherDecode (marshalJSON btc info))) `shouldBe` Right (Just Null)

    -- As hw wrote it before it kept the time
    it "reads a transaction in preparation without its creation time" $ do
      let info = TxInfo paymentSummary (Pending (PendingStatus (fakeTxHash 7) Nothing False False Nothing [])) "" Map.empty Map.empty
      (unmarshalJSON btc . encode . Object . withoutCreated <$> eitherDecode (marshalJSON btc info)) `shouldBe` Right (Just info)

    it "refuses a stored summary with an address that is not one of the network" $ do
      let json :: String
          json =
            [r|{
              "type": "credit", "amount": 50000, "fee": 1000, "feebyte": 4, "size": 226, "locktime": 0,
              "myoutputs": {"nonsense": {"value": 50000, "path": "/0/0", "type": "bip44", "label": ""}},
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
        pool <- testPool ctx
        record pool (payment ctx) `dbShouldBeE` True
        row <- lift (storedRow pool (fakeTxHash 1)) >>= liftMaybe "The summary is not stored"
        liftIO $ do
          dBTxInfoBlockRef row `shouldBe` S.encode (Store.BlockRef 800000 1)
          dBTxInfoDead row `shouldBe` False
        record pool (paymentAt ctx (Store.MemRef 1700000000)) `dbShouldBeE` True
        (fmap dBTxInfoBlockRef <$> lift (storedRow pool (fakeTxHash 1))) `dbShouldBeE` Just (S.encode (Store.MemRef 1700000000))

    it "reports no change when the same summary comes again, and a change when its block moves" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (paymentAt ctx (Store.MemRef 1700000000))
        first <- lift $ storedRow pool (fakeTxHash 1)
        record pool (paymentAt ctx (Store.MemRef 1700000000)) `dbShouldBeE` False
        lift (storedRow pool (fakeTxHash 1)) `dbShouldBeE` first
        -- Confirmed in a block: the row keeps the time it was first stored
        record pool (payment ctx) `dbShouldBeE` True
        (fmap dBTxInfoCreated <$> lift (storedRow pool (fakeTxHash 1))) `dbShouldBeE` (dBTxInfoCreated <$> first)
        listedBlocks pool `dbShouldBeE` [(Store.BlockRef 800000 1, 1)]

    it "stores the time the store gives the transaction" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        (fmap dBTxInfoTime <$> lift (storedRow pool (fakeTxHash 1))) `dbShouldBeE` Just seenTime

    it "maps the live transactions to their blocks, leaving the dead ones out" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        _ <- record pool (spend ctx)
        txInfoBlockRefs pool `dbShouldBeE` Map.fromList [(fakeTxHash 1, Store.BlockRef 800000 1), (fakeTxHash 2, Store.BlockRef 800001 1)]
        lift $ markTxInfoDead pool 800001 (fakeTxHash 2)
        txInfoBlockRefs pool `dbShouldBeE` Map.fromList [(fakeTxHash 1, Store.BlockRef 800000 1)]

    it "brings a dead summary back when the store lists the transaction again" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (spend ctx)
        lift $ markTxInfoDead pool 800001 (fakeTxHash 2)
        listedDead pool `dbShouldBeE` [True]
        record pool (spend ctx) `dbShouldBeE` True
        listedDead pool `dbShouldBeE` [False]
        txInfoBlockRefs pool `dbShouldBeE` Map.fromList [(fakeTxHash 2, Store.BlockRef 800001 1)]

    it "marks nothing when the transaction to mark dead is not there" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        lift $ markTxInfoDead pool 800000 (fakeTxHash 9)
        listedDead pool `dbShouldBeE` [False]

    -- The block puts the spending, at position 2, before the payment it
    -- spends, at position 5
    it "keeps the turn a transaction has in its block when its summary is stored again there" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (paymentAt ctx (Store.BlockRef 800000 5))
        _ <- record pool (spendAt ctx (Store.BlockRef 800000 2))
        orderBlock pool 800000
        record pool (spendAt ctx (Store.BlockRef 800000 2)) `dbShouldBeE` False
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]

standingSpec :: Spec
standingSpec =
  describe "Where a transaction stands in txs" $ do
    it "moves no row when each spends from rows before it only" $
      spendersLast fst snd [('a', ""), ('b', "a"), ('c', "ab")] `shouldBe` [('a', ""), ('b', "a"), ('c', "ab")]

    it "moves a row that spends from a later one to right after it, and no other" $
      spendersLast fst snd [('b', "a"), ('c', ""), ('a', ""), ('d', "")] `shouldBe` [('c', ""), ('a', ""), ('b', "a"), ('d', "")]

    it "moves a row to after the last of the rows it spends from" $
      spendersLast fst snd [('c', "ab"), ('a', ""), ('d', ""), ('b', "")] `shouldBe` [('a', ""), ('d', ""), ('b', ""), ('c', "ab")]

    it "moves a row that spends from a moved one after it" $
      spendersLast fst snd [('c', "b"), ('b', "a"), ('a', "")] `shouldBe` [('a', ""), ('b', "a"), ('c', "b")]

    it "leaves a row that spends from none of the rows given where it is" $
      spendersLast fst snd [('b', "z"), ('a', "")] `shouldBe` [('b', "z"), ('a', "")]

    -- Position 1 pays, position 5 spends it: the order of a block of
    -- Bitcoin
    it "gives the transactions of a block their own positions when each comes after the ones it spends from" $
      blockStandings 800000 [(fakeTxHash 2, 5, [fakeTxHash 1]), (fakeTxHash 1, 1, [])]
        `shouldBe` [(fakeTxHash 1, InBlock 800000 1 0), (fakeTxHash 2, InBlock 800000 5 0)]

    -- Transaction 2, at position 2, spends transaction 1, at position 5.
    -- Transaction 3, at position 3, spends neither.
    it "gives a transaction the block puts before one it spends from the turn right after that one" $
      blockStandings 800000 [(fakeTxHash 2, 2, [fakeTxHash 1]), (fakeTxHash 3, 3, []), (fakeTxHash 1, 5, [])]
        `shouldBe` [(fakeTxHash 3, InBlock 800000 3 0), (fakeTxHash 1, InBlock 800000 5 0), (fakeTxHash 2, InBlock 800000 5 1)]

    -- Transaction 2 spends transaction 1, and transaction 3 spends
    -- transaction 2. Transaction 4, at position 7, comes after them all.
    it "gives each of a chain of moved transactions one step more, and the next transaction its own position" $
      blockStandings 800000 [(fakeTxHash 3, 1, [fakeTxHash 2]), (fakeTxHash 2, 2, [fakeTxHash 1]), (fakeTxHash 1, 5, []), (fakeTxHash 4, 7, [])]
        `shouldBe` [ (fakeTxHash 1, InBlock 800000 5 0),
                     (fakeTxHash 2, InBlock 800000 5 1),
                     (fakeTxHash 3, InBlock 800000 5 2),
                     (fakeTxHash 4, InBlock 800000 7 0)
                   ]

    it "gives a transaction that spends from another block its own position" $
      blockStandings 800000 [(fakeTxHash 2, 2, [fakeTxHash 9])] `shouldBe` [(fakeTxHash 2, InBlock 800000 2 0)]

    it "stores an unconfirmed transaction as one byte" $
      standingBytes InMempool `shouldBe` hexBytes "00"

    -- 0xffffffff - 800000 = 0xfff3caff, 0xffffffff - 1 = 0xfffffffe,
    -- 0xffffffff - 0 = 0xffffffff
    it "stores a confirmed transaction as its height, then its turn, each counted down from the highest" $
      standingBytes (InBlock 800000 1 0) `shouldBe` hexBytes "01fff3caff01fffffffeffffffff"

    -- 0xffffffffffffffff - 1700000000 = 0xffffffff9aac0eff
    it "stores a dead transaction as the height it died at, then its time, each counted down from the highest" $
      standingBytes (DiedAt 800000 seenTime) `shouldBe` hexBytes "01fff3caff00ffffffff9aac0eff"

    it "sorts the stored standings in the order of txs" $ do
      let inOrder =
            [ InMempool,
              -- Died once the sync had reached block 800001
              DiedAt 800001 (addingSeconds 600),
              DiedAt 800001 seenTime,
              -- The transactions of block 800001, the last turn first
              InBlock 800001 7 1,
              InBlock 800001 7 0,
              InBlock 800001 2 0,
              DiedAt 800000 seenTime,
              InBlock 800000 9 0
            ]
      sort (standingBytes <$> reverse inOrder) `shouldBe` (standingBytes <$> inOrder)

    it "stands a summary in the mempool, or at its own position in its block" $ do
      refStanding (Store.MemRef 1700000000) `shouldBe` InMempool
      refStanding (Store.BlockRef 800000 3) `shouldBe` InBlock 800000 3 0
  where
    -- 14 Nov 2023 22:13:20 UTC and the seconds given: 600 make 22:23:20
    addingSeconds seconds = UTCTime (fromGregorian 2023 11 14) (22 * 3600 + 13 * 60 + 20 + seconds)

listingSpec :: Ctx -> Spec
listingSpec ctx =
  describe "Listing the transactions" $ do
    let cfg = def :: Config
        -- Four payments: two in block 800000, one in block 800001, one in
        -- the mempool
        fourPayments pool = do
          _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
          _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800000 5) (externalAddr 0) 50000)
          _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800001 0) (externalAddr 0) 50000)
          _ <- record pool (creditTx ctx (fakeTxHash 4) (Store.MemRef 1700000000) (externalAddr 0) 50000)
          return ()
    it "lists the mempool first, then the newest blocks first, and within a block the last first" $
      inDatabaseE $ do
        pool <- testPool ctx
        fourPayments pool
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 4, fakeTxHash 3, fakeTxHash 2, fakeTxHash 1]

    it "lists them a page at a time, each page after the last transaction of the one before" $
      inDatabaseE $ do
        pool <- testPool ctx
        fourPayments pool
        listedIds pool Nothing (Just 2) `dbShouldBeE` [fakeTxHash 4, fakeTxHash 3]
        listedIds pool (Just (fakeTxHash 3)) (Just 2) `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]
        listedIds pool (Just (fakeTxHash 1)) (Just 2) `dbShouldBeE` []

    it "goes on after the last unconfirmed transaction with the newest confirmed one" $
      inDatabaseE $ do
        pool <- testPool ctx
        fourPayments pool
        listedIds pool (Just (fakeTxHash 4)) (Just 1) `dbShouldBeE` [fakeTxHash 3]

    it "refuses to go on after a transaction the pool does not have" $
      inDatabaseE $ do
        pool <- testPool ctx
        fourPayments pool
        lift $
          dbShouldFailWith "The transaction 0000000000000000000000000000000000000000000000000000000000000005 is not in the portfolio on btc" $
            txsCursorAt pool (fakeTxHash 5)

    -- Block 800000 is dated 1700000600, ten minutes after block 800001
    -- above it, dated 1700000000
    it "lists the confirmed transactions by their blocks, the newest first, whatever the times of the blocks" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800001 1) (externalAddr 0) 50000)
        _ <- record pool (seenAt 1700000600 (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800000 1) (externalAddr 0) 30000))
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 1, fakeTxHash 2]
        listedIds pool (Just (fakeTxHash 1)) Nothing `dbShouldBeE` [fakeTxHash 2]

    -- The block of payment 1 is dated an hour ahead, as miners of test
    -- networks date them: 1700000000 + 3600 = 1700003600. Payment 2 is in
    -- the mempool since 1700000600.
    it "lists the unconfirmed transactions first, though a confirmed one has a later time" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (seenAt 1700003600 (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800001 1) (externalAddr 0) 50000))
        _ <- record pool (seenAt 1700000600 (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000600) (externalAddr 0) 30000))
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]

    it "lists the unconfirmed transactions the newest first, by the time the store first saw each" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (seenAt 1700000600 (creditTx ctx (fakeTxHash 1) (Store.MemRef 1700000600) (externalAddr 0) 50000))
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000000) (externalAddr 0) 30000)
        _ <- record pool (seenAt 1700001200 (creditTx ctx (fakeTxHash 3) (Store.MemRef 1700001200) (externalAddr 0) 20000))
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 3, fakeTxHash 1, fakeTxHash 2]

    -- The store saw the payment, transaction 1, and its spending,
    -- transaction 2, in the same second. By their ids alone the payment
    -- would come first.
    it "lists an unconfirmed transaction above the one it spends from, though the store saw both in one second" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (paymentAt ctx (Store.MemRef 1700000000))
        _ <- record pool (spendAt ctx (Store.MemRef 1700000000))
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]
        listedIds pool (Just (fakeTxHash 2)) Nothing `dbShouldBeE` [fakeTxHash 1]

    -- The store dates the spending ten minutes before the payment it
    -- spends, as a second store behind the same address may
    it "lists an unconfirmed transaction above the one it spends from, though the store dates it earlier" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (seenAt 1700000600 (paymentAt ctx (Store.MemRef 1700000600)))
        _ <- record pool (spendAt ctx (Store.MemRef 1700000000))
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]

    -- A block of Bitcoin Cash is sorted by transaction id. This one puts
    -- the spending at position 2, an unrelated payment at position 3, and
    -- the payment the spending spends at position 5.
    it "lists a transaction above the one it spends from in its block, though the block puts it first, and moves no other" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (spendAt ctx (Store.BlockRef 800000 2))
        _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800000 3) (externalAddr 0) 70000)
        _ <- record pool (paymentAt ctx (Store.BlockRef 800000 5))
        orderBlock pool 800000
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1, fakeTxHash 3]
        listedIds pool (Just (fakeTxHash 2)) Nothing `dbShouldBeE` [fakeTxHash 1, fakeTxHash 3]

    it "moves the transactions of a block the pool has no other transaction in nowhere" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (paymentAt ctx (Store.BlockRef 800000 5))
        _ <- record pool (spendAt ctx (Store.BlockRef 800001 2))
        orderBlock pool 800000
        orderBlock pool 800001
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]

    -- Payment 2 was last seen alive, in the mempool, when the pool had
    -- reached block 800000, where payment 1 is. Payment 3 came later, in
    -- block 800001.
    it "lists a transaction that died in the mempool above the blocks the pool knew when it was last alive, and below the later ones" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000000) (externalAddr 0) 30000)
        lift $ markTxInfoDead pool 800000 (fakeTxHash 2)
        _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800001 1) (externalAddr 0) 20000)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 3, fakeTxHash 2, fakeTxHash 1]
        listedIds pool (Just (fakeTxHash 2)) Nothing `dbShouldBeE` [fakeTxHash 1]

    it "lists a dead transaction below the unconfirmed ones" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000)
        _ <- record pool (seenAt 1700000600 (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000600) (externalAddr 0) 30000))
        lift $ markTxInfoDead pool 800000 (fakeTxHash 2)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 1, fakeTxHash 2]

    -- Both were last seen alive when the pool had reached block 800000.
    -- The store first saw transaction 1 at 1700000000 and transaction 2 at
    -- 1700000600.
    it "lists the transactions that died in the mempool at one height the newest first" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.MemRef 1700000000) (externalAddr 0) 50000)
        _ <- record pool (seenAt 1700000600 (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000600) (externalAddr 0) 30000))
        lift $ markTxInfoDead pool 800000 (fakeTxHash 1)
        lift $ markTxInfoDead pool 800000 (fakeTxHash 2)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]

    -- Payment 2 died at block 800000. Marked dead again at block 800005,
    -- it stays under payment 3 of block 800003.
    it "leaves a dead transaction where it first died when it is marked dead again" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000000) (externalAddr 0) 30000)
        lift $ markTxInfoDead pool 800000 (fakeTxHash 2)
        _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800003 1) (externalAddr 0) 20000)
        lift $ markTxInfoDead pool 800005 (fakeTxHash 2)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 3, fakeTxHash 2]

    -- Payment 2, of block 800001, was left out by a reorganisation the
    -- pool saw at block 800005. It stays between payment 3 of block 800002
    -- and payment 1 of block 800000.
    it "leaves a confirmed transaction that dies at the place of its block" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800001 1) (externalAddr 0) 30000)
        _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800002 1) (externalAddr 0) 20000)
        lift $ markTxInfoDead pool 800005 (fakeTxHash 2)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 3, fakeTxHash 2, fakeTxHash 1]

    -- Payment 2 died at block 800000, then the store listed it again, in
    -- block 800002, above payment 3 of block 800001
    it "lists a transaction that comes back to life at its block" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.MemRef 1700000000) (externalAddr 0) 30000)
        lift $ markTxInfoDead pool 800000 (fakeTxHash 2)
        _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.BlockRef 800001 1) (externalAddr 0) 20000)
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800002 1) (externalAddr 0) 30000)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 3]

    it "counts the confirmations from the best block, one when it is unknown or behind, none in the mempool" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800000 1) (externalAddr 0) 50000)
        _ <- record pool (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800002 1) (externalAddr 0) 50000)
        _ <- record pool (creditTx ctx (fakeTxHash 3) (Store.MemRef 1700000000) (externalAddr 0) 50000)
        listedBlocks pool `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 1)]
        lift $ updateBest btc (fakeBlockHash 0) 800002 1700000000
        listedBlocks pool `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 3)]
        -- The best block of another network does not count
        lift $ updateBest btcTest (fakeBlockHash 0) 900000 1700000000
        listedBlocks pool `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 3)]
        -- A best block behind a transaction's block still confirms it once
        lift $ updateBest btc (fakeBlockHash 0) 800001 1700000000
        listedBlocks pool `dbShouldBeE` [(Store.MemRef 1700000000, 0), (Store.BlockRef 800002 1, 1), (Store.BlockRef 800000 1, 2)]

    it "lists each transaction with the time the store gives it" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        (map (chainTime . chainOf) <$> txsAfter pool Nothing Nothing) `dbShouldBeE` [Just seenTime]

    it "gives a dead transaction no confirmations, and keeps its block" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        lift $ updateBest btc (fakeBlockHash 0) 800002 1700000000
        lift $ markTxInfoDead pool 800002 (fakeTxHash 1)
        (map chainOf <$> txsAfter pool Nothing Nothing) `dbShouldBeE` [ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 0 True]

    it "fills in the labels of the portfolio's addresses as they are today" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, _) <- testNewAcc ctx
        _ <- genExtAddress ctx cfg accId "Rent"
        _ <- nextChangeAddress ctx cfg accId []
        _ <- record pool (spend ctx)
        listedLabels pool `dbShouldBeE` [(["Rent"], [""])]
        _ <- setAddrLabel accId 0 "Salary"
        listedLabels pool `dbShouldBeE` [(["Salary"], [""])]

    it "refuses a stored summary that does not decode, naming the transaction" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ P.insert_ $ DBTxInfo pool.poolPortfolio "btc" (hashBytes (fakeTxHash 9)) (S.encode (Store.BlockRef 800000 1)) (standingBytes (InBlock 800000 1 0)) seenTime 0 "nonsense" False fixtureTime
        lift $
          dbShouldFailWith "The stored summary of transaction 0000000000000000000000000000000000000000000000000000000000000009 does not decode" $
            txsAfter pool Nothing Nothing

    it "gives each transaction the label the portfolio gave it" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        _ <- record pool (spend ctx)
        lift $ setTxLabel pool (fakeTxHash 1) "Salary"
        -- The spending first: it is in the newer block
        (map txInfoLabel <$> txsAfter pool Nothing Nothing) `dbShouldBeE` ["", "Salary"]

    -- The spending spends the coin of the payment
    it "fills in the labels the coins each transaction spends show, by the transaction that paid them" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        _ <- record pool (spend ctx)
        lift $ setTxLabel pool (fakeTxHash 1) "Salary"
        (map txInfoInputLabels <$> txsAfter pool Nothing Nothing) `dbShouldBeE` [Map.fromList [(fakeTxHash 1, "Salary")], Map.empty]

    it "shows on a coin the label of the payment that paid it to the portfolio" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        lift $ setTxLabel pool (fakeTxHash 1) "Salary"
        lift $ coinLabel pool (fakeTxHash 1) `dbShouldBe` "Salary"

    -- The label says what the payment was for, not what its change is
    it "shows no label on the change of a payment the portfolio made" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (spend ctx)
        lift $ setTxLabel pool (fakeTxHash 2) "Rent"
        lift $ coinLabel pool (fakeTxHash 2) `dbShouldBe` ""

    it "shows no label on a coin of a transaction the portfolio has no label for, or does not have" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        lift $ coinLabel pool (fakeTxHash 1) `dbShouldBe` ""
        lift $ setTxLabel pool (fakeTxHash 3) "Elsewhere"
        lift $ coinLabel pool (fakeTxHash 3) `dbShouldBe` ""

    -- The sync rewrites a summary when its transaction moves to another
    -- block, and marks it dead when the transaction is replaced
    it "keeps the label of a transaction whose summary the sync rewrites, then marks dead" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (spend ctx)
        lift $ setTxLabel pool (fakeTxHash 2) "Rent"
        _ <- record pool (spendAt ctx (Store.BlockRef 800002 4))
        (map txInfoLabel <$> txsAfter pool Nothing Nothing) `dbShouldBeE` ["Rent"]
        lift $ markTxInfoDead pool 800002 (fakeTxHash 2)
        (map txInfoLabel <$> txsAfter pool Nothing Nothing) `dbShouldBeE` ["Rent"]

    -- Without a best block, a transaction in a block has one confirmation
    it "gives one transaction of the portfolio as txs lists it, with its label" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        lift $ setTxLabel pool (fakeTxHash 1) "Salary"
        storedTxInfo pool (fakeTxHash 1)
          `dbShouldBeE` Just (TxInfo paymentSummary (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 1 False)) "Salary" Map.empty Map.empty)

    it "gives no transaction the portfolio does not have" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (payment ctx)
        storedTxInfo pool (fakeTxHash 9) `dbShouldBeE` Nothing

    it "finds the ids of the portfolio that start with the digits typed, in either case, for the shell" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ do
          _ <- repsertSummary pool idAb (Store.BlockRef 800000 1) seenTime paymentSummary
          _ <- repsertSummary pool idCd (Store.BlockRef 800000 2) seenTime paymentSummary
          txidsStarting pool "ab" 21 `dbShouldBe` [idAb]
          txidsStarting pool "AB" 21 `dbShouldBe` [idAb]
          txidsStarting pool "ef" 21 `dbShouldBe` []
          -- In the order of the ids, up to the count
          txidsStarting pool "" 21 `dbShouldBe` [idAb, idCd]
          txidsStarting pool "" 1 `dbShouldBe` [idAb]

    -- A short id names the txids that start with its 8 digits, ab000000
    -- here: the lowest and the highest of them, and not ab000001, the next
    -- short id
    it "finds the txids of the portfolio that have the short id, from the lowest to the highest, and no other" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ do
          forM_ [lowestAb, highestAb, nextAb, idCd] $ \tid ->
            repsertSummary pool tid (Store.BlockRef 800000 1) seenTime paymentSummary
          (sort . map txHashToHex <$> txidsWith pool (typedShortId "ab000000"))
            `dbShouldBe` [ "ab00000000000000000000000000000000000000000000000000000000000000",
                           "ab000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         ]

    it "finds the txids that have the short id in every portfolio, a txid two portfolios hold once" $
      inDatabaseE $ do
        pool <- testPool ctx
        otherPool <- (`portfolioPool` btc) <$> portfolioOf ctx testMnemonic2
        lift $ do
          _ <- repsertSummary pool lowestAb (Store.BlockRef 800000 1) seenTime paymentSummary
          _ <- repsertSummary otherPool lowestAb (Store.BlockRef 800000 1) seenTime paymentSummary
          _ <- repsertSummary otherPool highestAb (Store.BlockRef 800000 2) seenTime paymentSummary
          (sort . map txHashToHex <$> walletTxidsWith (typedShortId "ab000000"))
            `dbShouldBe` [ "ab00000000000000000000000000000000000000000000000000000000000000",
                           "ab000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                         ]

    it "lists the transactions of the pool alone" $
      inDatabaseE $ do
        pool <- testPool ctx
        otherPool <- (`portfolioPool` btc) <$> portfolioOf ctx testMnemonic2
        _ <- record pool (payment ctx)
        -- A payment to the second wallet's first address
        let otherPaths = Map.fromList [(externalAddr2, legacyPath (Deriv :/ 0 :/ 0))]
            otherPayment = creditTx ctx (fakeTxHash 8) (Store.BlockRef 800000 2) externalAddr2 70000
        _ <- lift $ repsertSummary otherPool (fakeTxHash 8) (Store.BlockRef 800000 2) seenTime (storeSummary btc otherPaths otherPayment)
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 1]
        listedIds otherPool Nothing Nothing `dbShouldBeE` [fakeTxHash 8]

    -- A transaction from before the fork of Bitcoin Cash has the same id
    -- on both chains
    it "keeps the transactions of each network of a portfolio apart" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        _ <- record (portfolioPool portfolio btc) (payment ctx)
        listedIds (portfolioPool portfolio btc) Nothing Nothing `dbShouldBeE` [fakeTxHash 1]
        listedIds (portfolioPool portfolio bch) Nothing Nothing `dbShouldBeE` []

-- A transaction of a portfolio may pay, or spend from, an address of
-- another portfolio of the database. Its summary counts that address
-- among those of others, and the database tells where it is. The test
-- wallet is wallet 0. Its portfolio 0 holds the pool of these tests, and
-- its portfolio 1 has 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, 'account1Addr',
-- as the first receiving address of its bip44 account on btc.
elsewhereSpec :: Ctx -> Spec
elsewhereSpec ctx =
  describe "The addresses of a transaction that another portfolio holds" $ do
    let cfg = def :: Config
        -- Portfolio 1 of the test wallet, created after portfolio 0, with
        -- its first receiving address given out under the label
        portfolio1GivesOut label = do
          portfolio1 <- nextPortfolioOf ctx testMnemonic
          (accId, _) <- accountOf portfolio1 btc Bip44
          void $ genExtAddress ctx cfg accId label

    it "are named with their portfolio, their path there and their label there" $
      inDatabaseE $ do
        pool <- testPool ctx
        portfolio1GivesOut "Savings"
        lift (heldElsewhere pool [account1Addr])
          `dbShouldBeE` Map.fromList [(account1Addr, HeldElsewhere (PortfolioRef 0 1) (legacyPath (Deriv :/ 0 :/ 0)) "Savings")]

    it "leave out an address of another wallet, which no portfolio holds" $
      inDatabaseE $ do
        pool <- testPool ctx
        portfolio1GivesOut ""
        (Map.keys <$> lift (heldElsewhere pool [account1Addr, otherAddr 0])) `dbShouldBeE` [account1Addr]

    -- The first receiving address of portfolio 0, which holds the pool
    it "leave out an address of the portfolio of the transaction itself" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, _) <- testNewAcc ctx
        _ <- genExtAddress ctx cfg accId ""
        lift (heldElsewhere pool [externalAddr 0]) `dbShouldBeE` Map.empty

    -- The database knows an address once its portfolio gave it out
    it "leave out an address the other portfolio has not given out" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- nextPortfolioOf ctx testMnemonic
        lift (heldElsewhere pool [account1Addr]) `dbShouldBeE` Map.empty

    -- The second test wallet is created second, so it is wallet 1. Its
    -- first receiving address is 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2.
    it "name an address of a portfolio of another wallet with that wallet" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, _) <- testNewAcc2 ctx
        _ <- genExtAddress ctx cfg accId ""
        lift (heldElsewhere pool [externalAddr2])
          `dbShouldBeE` Map.fromList [(externalAddr2, HeldElsewhere (PortfolioRef 1 0) (legacyPath (Deriv :/ 0 :/ 0)) "")]

    -- The test networks share their keys, so mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih
    -- is an address of portfolio 0 of the test wallet on each of them. It
    -- gave the address out on btctest alone. The transaction is of the
    -- second test wallet.
    it "are looked for on the network of the transaction alone" $
      inDatabaseE $ do
        (accId, _) <- newAccountOf ctx testMnemonic btcTest
        _ <- genExtAddress ctx cfg accId ""
        portfolio2 <- portfolioOf ctx testMnemonic2
        lift (heldElsewhere (portfolioPool portfolio2 btcTest4) [testnetExternalAddr]) `dbShouldBeE` Map.empty
        lift (heldElsewhere (portfolioPool portfolio2 btcTest) [testnetExternalAddr])
          `dbShouldBeE` Map.fromList [(testnetExternalAddr, HeldElsewhere (PortfolioRef 0 0) (legacyPath (Deriv :/ 0 :/ 0)) "")]

    -- Portfolio 0 pays 40000 to the address of portfolio 1
    it "are filled in for the address a payment of the history pays" $
      inDatabaseE $ do
        pool <- testPool ctx
        portfolio1GivesOut "Savings"
        _ <- record pool (betweenPortfolios ctx)
        (fmap txInfoElsewhere <$> storedTxInfo pool (fakeTxHash 2))
          `dbShouldBeE` Just (Map.fromList [(account1Addr, HeldElsewhere (PortfolioRef 0 1) (legacyPath (Deriv :/ 0 :/ 0)) "Savings")])

    -- Portfolio 1 receives the same payment, which spends the coin at the
    -- first receiving address of portfolio 0, labelled Rent there
    it "are filled in for the address a payment received was spent from" $
      inDatabaseE $ do
        _ <- testPool ctx
        (accId, _) <- testNewAcc ctx
        _ <- genExtAddress ctx cfg accId "Rent"
        portfolio1 <- nextPortfolioOf ctx testMnemonic
        let pool1 = portfolioPool portfolio1 btc
            tx = betweenPortfolios ctx
            received = storeSummary btc (Map.fromList [(account1Addr, legacyPath (Deriv :/ 0 :/ 0))]) tx
        _ <- lift $ repsertSummary pool1 tx.txid tx.block (storeTime tx.timestamp) received
        (fmap txInfoElsewhere <$> storedTxInfo pool1 (fakeTxHash 2))
          `dbShouldBeE` Just (Map.fromList [(externalAddr 0, HeldElsewhere (PortfolioRef 0 0) (legacyPath (Deriv :/ 0 :/ 0)) "Rent")])

    it "are none for a payment to another wallet" $
      inDatabaseE $ do
        pool <- testPool ctx
        portfolio1GivesOut ""
        _ <- record pool (spend ctx)
        (fmap txInfoElsewhere <$> storedTxInfo pool (fakeTxHash 2)) `dbShouldBeE` Just Map.empty

balanceSpec :: Ctx -> Spec
balanceSpec ctx =
  describe "The balances of txs" $ do
    -- The payment and its spending: 50000 - 41000 = 9000 left
    let paidAndSpent = do
          pool <- testPool ctx
          _ <- record pool (payment ctx)
          _ <- record pool (spend ctx)
          holding pool 9000 0
          return pool
        -- A payment of 70000, one block above the spending
        laterPayment = creditTx ctx (fakeTxHash 3) (Store.BlockRef 800002 1) (externalAddr 0) 70000
        -- A payment of 70000 listed between the payment and its spending
        paymentBetween = creditTx ctx (fakeTxHash 3) (Store.BlockRef 800000 5) (externalAddr 0) 70000
        -- Three payments whose times and blocks disagree, and what the
        -- pool holds after them: payment 1 of 50000 in block 800001,
        -- dated 1700000000, payment 2 of 20000 in block 800000 under it,
        -- dated 1700000600, payment 3 of 30000 in the mempool
        timedPayments pool = do
          _ <- record pool (creditTx ctx (fakeTxHash 1) (Store.BlockRef 800001 1) (externalAddr 0) 50000)
          _ <- record pool (seenAt 1700000600 (creditTx ctx (fakeTxHash 2) (Store.BlockRef 800000 1) (externalAddr 0) 20000))
          _ <- record pool (seenAt 1700001200 (creditTx ctx (fakeTxHash 3) (Store.MemRef 1700001200) (externalAddr 0) 30000))
          holding pool 70000 30000

    it "gives the newest transaction what the portfolio holds, confirmed or not" $
      inDatabaseE $ do
        pool <- paidAndSpent
        holding pool 6000 3000
        -- 6000 + 3000 = 9000
        (take 1 <$> listedBalances pool Nothing) `dbShouldBeE` [Just 9000]

    -- 6000 in the bip44 account and 3000 in the bip84 one: 9000
    it "gives the newest transaction what both accounts of the pool hold together" $
      inDatabaseE $ do
        pool <- paidAndSpent
        holding pool 6000 0
        holdingIn pool Bip84 2000 1000
        (take 1 <$> listedBalances pool Nothing) `dbShouldBeE` [Just 9000]
        lift (poolTotal pool) `dbShouldBeE` 9000

    it "gives each older transaction the balance without the one above it" $
      inDatabaseE $ do
        pool <- paidAndSpent
        -- The spending took 41000: 9000 + 41000 = 50000
        listedBalances pool Nothing `dbShouldBeE` [Just 9000, Just 50000]

    -- 50000 + 20000 + 30000 = 100000 in all, listed as payment 3, then
    -- payment 1, then payment 2
    it "walks the balances down in the order of the blocks, whatever their times" $
      inDatabaseE $ do
        pool <- testPool ctx
        timedPayments pool
        -- 100000 - 30000 = 70000, then 70000 - 50000 = 20000
        listedBalances pool Nothing `dbShouldBeE` [Just 100000, Just 70000, Just 20000]

    it "takes off the unconfirmed transactions and the confirmed ones of the blocks above for --after" $
      inDatabaseE $ do
        pool <- testPool ctx
        timedPayments pool
        -- 100000 - 30000 - 50000 = 20000 after payment 2, and nothing
        -- before it
        balanceBefore pool (fakeTxHash 1) `dbShouldBeE` 20000
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 0

    -- The payment of 50000 is in block 800000, dated 1700000420. Its
    -- spending, which takes 41000, is in block 800001, dated seven minutes
    -- before it, 1700000000. Listed by time, the spending would come
    -- under the payment, with 9000 - 50000 = -41000 beside it.
    it "shows no balance the portfolio never held when a block is dated before the one under it" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (seenAt 1700000420 (payment ctx))
        _ <- record pool (spend ctx)
        holding pool 9000 0
        listedIds pool Nothing Nothing `dbShouldBeE` [fakeTxHash 2, fakeTxHash 1]
        -- 9000 + 41000 = 50000
        listedBalances pool Nothing `dbShouldBeE` [Just 9000, Just 50000]

    it "gives a dead transaction no balance, and takes nothing off for it" $
      inDatabaseE $ do
        pool <- paidAndSpent
        _ <- record pool paymentBetween
        lift $ markTxInfoDead pool 800000 (fakeTxHash 3)
        listedBalances pool Nothing `dbShouldBeE` [Just 9000, Nothing, Just 50000]

    it "hands each transaction on with the balance before it, a dead one included" $
      inDatabaseE $ do
        pool <- paidAndSpent
        _ <- record pool paymentBetween
        lift $ markTxInfoDead pool 800000 (fakeTxHash 3)
        (map (txsCursorBalance . snd) <$> listedTxsAfter pool Nothing Nothing) `dbShouldBeE` [50000, 50000, 0]

    it "goes on from the balance the cursor carries, for the next chunk" $
      inDatabaseE $ do
        pool <- paidAndSpent
        cursor <- txsCursorAt pool (fakeTxHash 2)
        listedBalances pool (Just cursor {txsCursorBalance = 12345}) `dbShouldBeE` [Just 12345]

    it "starts after a transaction with the balance before it, for --after" $
      inDatabaseE $ do
        pool <- paidAndSpent
        -- 9000 + 41000 = 50000
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 50000

    it "takes off the transactions above the one --after names" $
      inDatabaseE $ do
        pool <- paidAndSpent
        _ <- record pool laterPayment
        holding pool 79000 0
        -- 79000 - 70000 + 41000 = 50000
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 50000

    it "takes off no dead transaction for --after" $
      inDatabaseE $ do
        pool <- paidAndSpent
        _ <- record pool laterPayment
        lift $ markTxInfoDead pool 800002 (fakeTxHash 3)
        -- 9000 + 41000 = 50000, the dead 70000 left out
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 50000

    it "takes off the transaction --after names even when it is dead: nothing" $
      inDatabaseE $ do
        pool <- paidAndSpent
        _ <- record pool laterPayment
        lift $ markTxInfoDead pool 800002 (fakeTxHash 3)
        balanceBefore pool (fakeTxHash 3) `dbShouldBeE` 9000

    -- The store saw the payment and its spending in the same second. The
    -- spending is listed above the payment it spends.
    it "takes off the unconfirmed transactions as far as the one --after names, a spending before its payment" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (paymentAt ctx (Store.MemRef 1700000000))
        _ <- record pool (spendAt ctx (Store.MemRef 1700000000))
        holding pool 0 9000
        listedBalances pool Nothing `dbShouldBeE` [Just 9000, Just 50000]
        -- 9000 + 41000 = 50000
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 50000
        -- 50000 - 50000 = 0
        balanceBefore pool (fakeTxHash 1) `dbShouldBeE` 0

    -- The block puts the spending, at position 2, before the payment it
    -- spends, at position 5, as a block of Bitcoin Cash may
    it "walks the balances of a block down from a spending to the payment it spends, though the block puts it first" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- record pool (paymentAt ctx (Store.BlockRef 800000 5))
        _ <- record pool (spendAt ctx (Store.BlockRef 800000 2))
        orderBlock pool 800000
        holding pool 9000 0
        -- 9000 + 41000 = 50000
        listedBalances pool Nothing `dbShouldBeE` [Just 9000, Just 50000]
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 50000

    it "leaves the transactions of other portfolios out of what --after adds up" $
      inDatabaseE $ do
        pool <- paidAndSpent
        otherPool <- (`portfolioPool` btc) <$> portfolioOf ctx testMnemonic2
        _ <- lift $ repsertSummary otherPool (fakeTxHash 8) (Store.BlockRef 800002 1) seenTime paymentSummary
        balanceBefore pool (fakeTxHash 2) `dbShouldBeE` 50000

    it "stores what a transaction adds or takes in its row, for --after to add up" $
      inDatabaseE $ do
        pool <- paidAndSpent
        (fmap dBTxInfoAmount <$> lift (storedRow pool (fakeTxHash 1))) `dbShouldBeE` Just 50000
        (fmap dBTxInfoAmount <$> lift (storedRow pool (fakeTxHash 2))) `dbShouldBeE` Just (-41000)

    it "stores the new amount when the sync rebuilds a summary" $
      inDatabaseE $ do
        pool <- paidAndSpent
        _ <- lift $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime paymentSummary
        (fmap dBTxInfoAmount <$> lift (storedRow pool (fakeTxHash 2))) `dbShouldBeE` Just 50000

historySpec :: Ctx -> Spec
historySpec ctx =
  describe "The history of a coin" $ do
    let cfg = def :: Config
    it "shows the payment to a labelled address and its spending, newest first, through a reorganisation" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, _) <- testNewAcc ctx
        _ <- genExtAddress ctx cfg accId "Rent"
        _ <- nextChangeAddress ctx cfg accId []
        -- The sync records both transactions and the best block
        record pool (payment ctx) `dbShouldBeE` True
        record pool (spend ctx) `dbShouldBeE` True
        lift $ updateBest btc (fakeBlockHash 0) 800002 1700000000
        txsAfter pool Nothing Nothing
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty Map.empty,
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
                      ]
        -- A reorganisation leaves the spending out: the payment stands
        lift $ markTxInfoDead pool 800002 (fakeTxHash 2)
        txsAfter pool Nothing Nothing
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 0 True)) "" Map.empty Map.empty,
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
                      ]
        -- The spending is mined again, in the next block
        record pool (spendAt ctx (Store.BlockRef 800002 4)) `dbShouldBeE` True
        txsAfter pool Nothing Nothing
          `dbShouldBeE` [ TxInfo spendFromRent (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800002 4) (Just seenTime) 1 False)) "" Map.empty Map.empty,
                        TxInfo paymentToRent (OnChain (ChainStatus (fakeTxHash 1) (Store.BlockRef 800000 1) (Just seenTime) 3 False)) "" Map.empty Map.empty
                      ]
