{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rules of a replacement that do not need the database: whether a
-- transaction can be replaced, the lowest fee rate of its replacement, the
-- fee the network asks of the replacement, what pays the fee the change
-- cannot, and what bumpfee says before it asks. What bumpfee builds is
-- covered with the hw binary, in "Haskoin.Wallet.Workflow.BumpFeeSpec".
module Haskoin.Wallet.Tx.BumpFeeSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Word (Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.PrettyPrinter (reducedDoc)
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.BumpFee
import Haskoin.Wallet.Database.TxInfo (MyInputs (..), TxSummary (..), TxType (..))
import Numeric.Natural (Natural)
import System.Random (mkStdGen)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  replaceableSpec ctx
  labelSpec ctx
  feeSpec ctx
  reductionSpec ctx
  fundingSpec ctx
  reducedDocSpec

-- | The stuck payment that bumpfee replaces: a payment of the account of
-- 2260 of fee for its 226 bytes, which spends the coin of 50000 at /0/0
stuckSummary :: TxSummary
stuckSummary =
  TxSummary
    { summaryType = TxDebit,
      summaryAmount = -22260,
      summaryFee = 2260,
      summaryFeeByte = 10,
      summaryChangeInFee = Just 0,
      summarySize = 226,
      summaryLockTime = 0,
      summaryMyOutputs = Map.empty,
      summaryOtherOutputs = Map.empty,
      summaryNonStdOutputs = [],
      summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (Deriv :/ 0 :/ 0) "" [OutPoint (fakeTxHash 1) 0])],
      summaryOtherInputs = Map.empty,
      summaryNonStdInputs = []
    }

-- | The payment, its inputs marked replaceable or final
payment :: Ctx -> Bool -> Sent
payment ctx marked =
  Sent
    { sentTx = (if marked then builtTx ctx 0 else spendingTx ctx) [(fakeTxHash 1, 0)] [(otherAddr 0, 20000), (internalAddr 0, 27740)],
      sentSummary = stuckSummary,
      sentParents = [],
      sentLabel = ""
    }

replaceableSpec :: Ctx -> Spec
replaceableSpec ctx =
  describe "A transaction bumpfee replaces" $ do
    it "is a payment of the account marked replaceable" $
      checkReplaceable (payment ctx True) `shouldBe` Right ()

    it "is not one whose inputs are final, which the store would keep showing" $
      checkReplaceable (payment ctx False)
        `shouldBe` Left "The transaction is not marked replaceable (BIP 125): the store would keep showing it in place of its replacement until a block confirms one of them"

    it "is not one that spends a coin of another wallet" $ do
      let received = (payment ctx True) {sentSummary = stuckSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 30000)]}}
      checkReplaceable received
        `shouldBe` Left "Only a payment from the coins of the account can be replaced: this transaction spends coins of another wallet"

labelSpec :: Ctx -> Spec
labelSpec ctx =
  describe "The label of a replacement" $ do
    let labelled label = (payment ctx True) {sentLabel = label}
    it "is the label of the transaction it replaces" $
      replacementLabel [labelled "Rent"] `shouldBe` "Rent"

    -- The stuck payment comes first, then the transactions that spend its
    -- change, which the replacement pays the recipients of too
    it "is the labels of the transactions it replaces, the stuck one first, each once, a semicolon between them" $
      replacementLabel [labelled "Rent", labelled "Groceries", labelled "", labelled "Rent"] `shouldBe` "Rent; Groceries"

    it "is none when none of them has a label" $
      replacementLabel [labelled "", labelled ""] `shouldBe` ""

feeSpec :: Ctx -> Spec
feeSpec ctx =
  describe "The fee of a replacement" $ do
    -- 2260 for 226 bytes is 10 a byte: the replacement pays more
    it "is at a rate above that of the transaction it replaces" $
      minimumRate (payment ctx True) `shouldBe` 11

    -- 2261 for 226 bytes is a little above 10 a byte
    it "is at a rate above that of a transaction that pays a fraction more" $
      minimumRate (payment ctx True) {sentSummary = stuckSummary {summaryFee = 2261}} `shouldBe` 11

    -- 2260 replaced, and 226 bytes at 1 satoshi a byte: 2486
    it "pays the fees of the transactions it replaces, and its own size at 1 satoshi a byte" $ do
      checkBumpFee (plan 2260) (paid 2486 226) `shouldBe` Right ()
      checkBumpFee (plan 2260) (paid 2485 226)
        `shouldBe` Left "The replacement pays 2485 satoshis of fee, and must pay 2486: the fees of the transactions it replaces, and its own 226 virtual bytes. Give a --fee-rate of at least 11"
  where
    plan :: Natural -> BumpPlan
    plan fees = BumpPlan Bip44 [] [] Map.empty [] Map.empty Nothing fees [] ""
    paid fee size = stuckSummary {summaryFee = fee, summarySize = size}

{- What pays the fee the change cannot -}

-- | A confirmed coin of the account: the first output of a fake
-- transaction, at the external address of the given index
coinAt :: Ctx -> Word8 -> Int -> Natural -> Store.Unspent
coinAt ctx w i = storeCoin ctx (fakeTxHash w, 0) (Just 800000) (externalAddr i)

-- | The plan of a send-all of the coin of 50000 at /0/0 to three addresses
-- of other wallets, 30000, 10000 and 7400: one input and three outputs,
-- 260 bytes, which paid 2600 of fee at 10 satoshis a byte. There is no
-- change, and no coin left in the account.
sendAllPlan :: Ctx -> BumpPlan
sendAllPlan ctx =
  BumpPlan
    { planKind = Bip44,
      planCoins = [coinAt ctx 1 0 50000],
      planParents = [],
      planCoinPaths = Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0)],
      planRecipients = [(otherAddr 0, 30000), (otherAddr 1, 10000), (otherAddr 2, 7400)],
      planRecipientPaths = Map.empty,
      planChange = Nothing,
      planFees = 2600,
      planReplaces = [fakeTxHash 2],
      planLabel = ""
    }

-- | The plan of a send-all of the same coin to one address, 48080: 192
-- bytes, which paid 1920
oneRecipientPlan :: Ctx -> BumpPlan
oneRecipientPlan ctx = (sendAllPlan ctx) {planRecipients = [(otherAddr 0, 48080)], planFees = 1920}

-- | The plan of the payment of 20000 of 'stuckSummary', with its change
-- of 27740 at /1/0
paymentPlan :: Ctx -> BumpPlan
paymentPlan ctx = (sendAllPlan ctx) {planRecipients = [(otherAddr 0, 20000)], planChange = Just (internalAddr 0, Deriv :/ 1 :/ 0), planFees = 2260}

-- | A way the recipients of 'sendAllPlan' pay what is missing: who, how
-- much, and what each of the three is left with
way :: PaidBy -> Natural -> (Natural, Natural, Natural) -> Reduction
way paidBy missing (first, second, third) =
  Reduction paidBy missing [(otherAddr 0, 30000, first), (otherAddr 1, 10000, second), (otherAddr 2, 7400, third)]

-- | The ways at 11 satoshis a byte: the 260 bytes pay 2860, and 47400 +
-- 2860 - 50000 = 260 are missing. Alone, a recipient pays all of it.
-- Together, each pays 86, and the first the 2 left over.
waysAt11 :: [Reduction]
waysAt11 =
  [ way (OneRecipient 0) 260 (29740, 10000, 7400),
    way (OneRecipient 1) 260 (30000, 9740, 7400),
    way (OneRecipient 2) 260 (30000, 10000, 7140),
    way AllRecipients 260 (29912, 9914, 7314)
  ]

-- | The ways at 100 satoshis a byte: 26000 of fee, 23400 missing. Only the
-- first recipient has that much. Together, each would pay 7800, more than
-- the third receives.
waysAt100 :: [Reduction]
waysAt100 = [way (OneRecipient 0) 23400 (6600, 10000, 7400)]

-- | The one way of the lone recipient of 'oneRecipientPlan' at 20 satoshis
-- a byte: the 192 bytes pay 3840, and 48080 + 3840 - 50000 = 1920 are
-- missing
loneWayAt20 :: Reduction
loneWayAt20 = Reduction AllRecipients 1920 [(otherAddr 0, 48080, 46160)]

reductionSpec :: Ctx -> Spec
reductionSpec ctx =
  describe "What the recipients pay of the fee the coins cannot" $ do
    it "is paid by one of them alone, or by all of them in equal shares, the first also paying the remainder" $
      reductions ctx (sendAllPlan ctx) 11 546 `shouldBe` Right waysAt11

    -- At 40 satoshis a byte, 10400 of fee: 7800 missing, more than the
    -- 7400 of the third recipient. Together, each pays 2600.
    it "is not paid alone by a recipient that has less than that" $
      reductions ctx (sendAllPlan ctx) 40 546
        `shouldBe` Right
          [ way (OneRecipient 0) 7800 (22200, 10000, 7400),
            way (OneRecipient 1) 7800 (30000, 2200, 7400),
            way AllRecipients 7800 (27400, 7400, 4800)
          ]

    it "is not paid by all of them when one would be left below the dust limit" $
      reductions ctx (sendAllPlan ctx) 100 546 `shouldBe` Right waysAt100

    -- At 11 satoshis a byte, the third recipient keeps 7140 paying alone,
    -- and 7314 paying its share
    it "leaves no recipient below the --dust-limit" $
      reductions ctx (sendAllPlan ctx) 11 7300
        `shouldBe` Right
          [ way (OneRecipient 0) 260 (29740, 10000, 7400),
            way (OneRecipient 1) 260 (30000, 9740, 7400),
            way AllRecipients 260 (29912, 9914, 7314)
          ]

    -- At 200 satoshis a byte, 52000 of fee: 49400 missing, more than any
    -- recipient has, and 16468 for the first and 16466 for the others
    -- together, more than the second has
    it "is refused when no recipient can pay it, together or alone" $
      reductions ctx (sendAllPlan ctx) 200 546
        `shouldBe` Left "The recipients cannot pay the 49400 satoshis missing, together or alone, without one being left with less than the dust limit of 546 satoshis"

    it "is paid by a lone recipient, the one way" $
      reductions ctx (oneRecipientPlan ctx) 20 546 `shouldBe` Right [loneWayAt20]

    -- At 300 satoshis a byte, 57600 of fee: 55680 missing, more than the
    -- recipient receives
    it "is refused when the lone recipient cannot pay it" $
      reductions ctx (oneRecipientPlan ctx) 300 546
        `shouldBe` Left "The recipient cannot pay the 55680 satoshis missing without being left with less than the dust limit of 546 satoshis"

    -- The payment keeps 27740 of change: at 20 satoshis a byte, its 226
    -- bytes pay 4520, with 25480 to spare
    it "is nothing when the coins pay the fee" $
      reductions ctx (paymentPlan ctx) 20 546
        `shouldBe` Left "The coins of the transaction pay its fee: the recipients have nothing to pay"

fundingSpec :: Ctx -> Spec
fundingSpec ctx =
  describe "What pays the fee of a replacement" $ do
    it "is the change, when it can" $
      fundings ctx gen (paymentPlan ctx) [] 20 546 `shouldBe` Right [ChangePays]

    -- With the coin of 30000 at /0/1 added, two inputs and two outputs,
    -- 374 bytes, pay 7480 at 20 satoshis a byte, and 80000 - 48080 - 7480
    -- = 24440 go to the change
    it "is a coin of the account first, then the recipients" $
      fundings ctx gen (oneRecipientPlan ctx) [added30000] 20 546
        `shouldBe` Right [CoinsAdded [added30000], RecipientsReduced loneWayAt20]

    it "is the recipients alone when no coin of the account would do" $
      fundings ctx gen (oneRecipientPlan ctx) [] 20 546 `shouldBe` Right [RecipientsReduced loneWayAt20]

    -- At 300 satoshis a byte, the coin of 200000 pays the 112200 of 374
    -- bytes, with 89720 of change; the recipient would be left with nothing
    it "is a coin of the account alone when the recipients cannot pay" $
      fundings ctx gen (oneRecipientPlan ctx) [added200000] 300 546 `shouldBe` Right [CoinsAdded [added200000]]

    it "is refused when neither the coins nor the recipients can pay, telling both" $
      fundings ctx gen (oneRecipientPlan ctx) [] 300 546
        `shouldBe` Left "The coins of the transaction cannot pay its fee at 300 sat/vB, and no confirmed coins of the account would. The recipient cannot pay the 55680 satoshis missing without being left with less than the dust limit of 546 satoshis"
  where
    -- The coins to add are taken the largest first: nothing is drawn
    gen = mkStdGen 0
    added30000 = (coinAt ctx 3 1 30000, externalAddr 1, Deriv :/ 0 :/ 1)
    added200000 = (coinAt ctx 4 1 200000, externalAddr 1, Deriv :/ 0 :/ 1)

reducedDocSpec :: Spec
reducedDocSpec =
  describe "What bumpfee says before it asks which recipient pays" $ do
    -- In satoshis, as --unit sat asks
    it "numbers the recipients, each with what it receives, then what it would receive paying alone and paying its share when all can" $ do
      renderedLines (reducedDoc btc UnitSatoshi 11 waysAt11)
        `shouldBe` [ "The coins of the transaction cannot pay its fee at 11 sat/vB, and no coin of the account is added. The 260 sats missing can come out of what the recipients receive, one of them paying it alone, or all of them in equal shares:",
                     "",
                     "  1  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  30000 sats",
                     "     29740 sats paying it alone, 29912 sats paying its share",
                     "",
                     "  2  1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  10000 sats",
                     "     9740 sats paying it alone, 9914 sats paying its share",
                     "",
                     "  3  191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q  7400 sats",
                     "     7140 sats paying it alone, 7314 sats paying its share"
                   ]
      reducedQuestion waysAt11 `shouldBe` "Who pays it? Type 1, 2, 3 or all, or no"

    it "tells which recipients cannot pay alone, and offers no share when all cannot" $ do
      renderedLines (reducedDoc btc UnitSatoshi 100 waysAt100)
        `shouldBe` [ "The coins of the transaction cannot pay its fee at 100 sat/vB, and no coin of the account is added. The 23400 sats missing can come out of what the recipients receive, one of them paying it alone:",
                     "",
                     "  1  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  30000 sats",
                     "     6600 sats paying it alone",
                     "",
                     "  2  1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  10000 sats",
                     "     too little to pay it alone",
                     "",
                     "  3  191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q  7400 sats",
                     "     too little to pay it alone"
                   ]
      reducedQuestion waysAt100 `shouldBe` "Who pays it? Type 1, or no"

    -- In bitcoins, the unit by default: 1920 satoshis are 0.00001920 btc
    it "tells a lone recipient what it would receive, and asks yes or no" $ do
      renderedLines (reducedDoc btc UnitBitcoin 20 [loneWayAt20])
        `shouldBe` [ "The coins of the transaction cannot pay its fee at 20 sat/vB, and no coin of the account is added. The recipient can pay the 0.00001920 btc missing out of what it receives:",
                     "",
                     "  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00048080 btc",
                     "    0.00046160 btc paying it"
                   ]
      reducedQuestion [loneWayAt20] `shouldBe` "Reduce what it receives?"
