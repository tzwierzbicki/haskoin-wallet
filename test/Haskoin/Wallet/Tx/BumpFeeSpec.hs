{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The rules of a replacement that do not need the database: whether a
-- transaction can be replaced, the lowest fee rate of its replacement, the
-- fee the network asks of the replacement, what pays the fee the change
-- cannot, and what bumpfee says before it asks. Then the replacement of a
-- payment of a portfolio that has coins of both types, bip44 and bip84,
-- which reads the portfolio in the database. What bumpfee builds from the
-- command line is covered with the hw binary, in
-- "Haskoin.Wallet.Workflow.BumpFeeSpec".
module Haskoin.Wallet.Tx.BumpFeeSpec (spec) where

import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Default (def)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word64, Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.PrettyPrinter (reducedDoc, sharedDoc)
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database (DB, Pool)
import Haskoin.Wallet.Database.Address (AddrType (..), extendChainTo)
import Haskoin.Wallet.Database.RawTx (insertRawTx)
import Haskoin.Wallet.Database.TxInfo (MyInputs (..), TxInfo (..), TxSummary (..), TxType (..), repsertSummary)
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath, legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.BumpFee
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..))
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Signing (parseTxSignData)
import Numeric.Natural (Natural)
import System.Random (StdGen, mkStdGen)
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  replaceableSpec ctx
  replacedSpec
  labelSpec ctx
  feeSpec ctx
  reductionSpec ctx
  fundingSpec ctx
  recipientsPaySpec ctx
  bothTypesSpec ctx
  reducedDocSpec
  sharedDocSpec

-- | The stuck payment that bumpfee replaces: a payment of the portfolio of
-- 2260 of fee for its 226 bytes, which spends the coin of 50000 at /0/0
-- of its bip44 account
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
      summaryMyInputs = Map.fromList [(externalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "" [OutPoint (fakeTxHash 1) 0])],
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
    let -- The payment with an input of each sequence
        withSequences sequences =
          let tx = spendingTx ctx [(fakeTxHash 1, i) | i <- [0 .. fromIntegral (length sequences) - 1]] [(otherAddr 0, 20000)]
           in Tx tx.version [TxIn input.outpoint input.script s | (input, s) <- zip tx.inputs sequences] tx.outputs tx.witness tx.locktime
    it "is a payment of the portfolio marked replaceable" $
      checkReplaceable (payment ctx True) `shouldBe` Right ()

    it "is not one whose inputs are final, which the store would keep showing" $
      checkReplaceable (payment ctx False)
        `shouldBe` Left "The transaction is not marked replaceable (BIP 125): the store would keep showing it in place of its replacement until a block confirms one of them"

    -- BIP 125: a transaction signals that it may be replaced when one of
    -- its inputs has a sequence below 0xfffffffe
    it "is marked replaceable by an input of the sequence 0xfffffffd, the highest that signals" $
      marksReplaceable (withSequences [0xfffffffd]) `shouldBe` True
    it "is not marked replaceable by inputs of the sequence 0xfffffffe, which enables the lock time alone" $
      marksReplaceable (withSequences [0xfffffffe, 0xfffffffe]) `shouldBe` False
    it "is marked replaceable by one input that signals among final ones" $
      marksReplaceable (withSequences [0xffffffff, 0xfffffffd, 0xffffffff]) `shouldBe` True

    it "is not one that spends a coin of another wallet" $ do
      let received = (payment ctx True) {sentSummary = stuckSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 30000)]}}
      checkReplaceable received
        `shouldBe` Left "Only a payment from the coins of the portfolio can be replaced: this transaction spends coins of another wallet"

replacedSpec :: Spec
replacedSpec =
  describe "The refusal to bump a transaction that was replaced" $ do
    it "names the replacement, as the transaction to bump instead" $
      replacedRefusal [b2d4f6a8Txid]
        `shouldBe` "The transaction was replaced by b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001: bump that one instead"

    it "names every replacement when several transactions replaced it, and leaves the choice to the user" $
      replacedRefusal [b2d4f6a8Txid, a1075db5Second]
        `shouldBe` "The transaction was replaced by b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001 and a1075db5c0ffee00000000000000000000000000000000000000000000000002"

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

-- | A confirmed coin of the bip44 account of the portfolio: the first
-- output of a fake transaction, at the external address of the given index
coinAt :: Ctx -> Word8 -> Int -> Natural -> Store.Unspent
coinAt ctx w i = storeCoin ctx (fakeTxHash w, 0) (Just 800000) (externalAddr i)

-- | The plan of a send-all of the coin of 50000 at /0/0 to three addresses
-- of other wallets, 30000, 10000 and 7400: one input and three outputs,
-- 260 bytes, which paid 2600 of fee at 10 satoshis a byte. There is no
-- change, and no coin left in the portfolio.
sendAllPlan :: Ctx -> BumpPlan
sendAllPlan ctx =
  BumpPlan
    { planDefaultKind = Bip44,
      planCoins = [coinAt ctx 1 0 50000],
      planParents = [],
      planCoinPaths = Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0))],
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
paymentPlan ctx = (sendAllPlan ctx) {planRecipients = [(otherAddr 0, 20000)], planChange = Just (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0)), planFees = 2260}

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
      reductions ctx (sendAllPlan ctx) 11 `shouldBe` Right waysAt11

    -- At 40 satoshis a byte, 10400 of fee: 7800 missing, more than the
    -- 7400 of the third recipient. Together, each pays 2600.
    it "is not paid alone by a recipient that has less than that" $
      reductions ctx (sendAllPlan ctx) 40
        `shouldBe` Right
          [ way (OneRecipient 0) 7800 (22200, 10000, 7400),
            way (OneRecipient 1) 7800 (30000, 2200, 7400),
            way AllRecipients 7800 (27400, 7400, 4800)
          ]

    it "is not paid by all of them when one would be left below the dust limit" $
      reductions ctx (sendAllPlan ctx) 100 `shouldBe` Right waysAt100

    -- At 37 satoshis a byte, 9620 of fee: 47400 + 9620 - 50000 = 7020
    -- missing. The third recipient has enough, and alone would keep 7400 -
    -- 7020 = 380, below the 546 of its output. Together, each pays 2340.
    it "leaves no recipient below the dust limit of its output" $
      reductions ctx (sendAllPlan ctx) 37
        `shouldBe` Right
          [ way (OneRecipient 0) 7020 (22980, 10000, 7400),
            way (OneRecipient 1) 7020 (30000, 2980, 7400),
            way AllRecipients 7020 (27660, 7660, 5060)
          ]

    -- At 200 satoshis a byte, 52000 of fee: 49400 missing, more than any
    -- recipient has, and 16468 for the first and 16466 for the others
    -- together, more than the second has
    it "is refused when no recipient can pay it, together or alone" $
      reductions ctx (sendAllPlan ctx) 200
        `shouldBe` Left "The recipients cannot pay the 49400 satoshis missing, together or alone, without one being left with less than the dust limit of 546 satoshis"

    it "is paid by a lone recipient, the one way" $
      reductions ctx (oneRecipientPlan ctx) 20 `shouldBe` Right [loneWayAt20]

    -- At 300 satoshis a byte, 57600 of fee: 55680 missing, more than the
    -- recipient receives
    it "is refused when the lone recipient cannot pay it" $
      reductions ctx (oneRecipientPlan ctx) 300
        `shouldBe` Left "The recipient cannot pay the 55680 satoshis missing without being left with less than the dust limit of 546 satoshis"

    -- The payment keeps 27740 of change: at 20 satoshis a byte, its 226
    -- bytes pay 4520, with 25480 to spare
    it "is nothing when the coins pay the fee" $
      reductions ctx (paymentPlan ctx) 20
        `shouldBe` Left "The coins of the transaction pay its fee: the recipients have nothing to pay"

fundingSpec :: Ctx -> Spec
fundingSpec ctx =
  describe "What pays the fee of a replacement" $ do
    it "is the change, when it can" $
      fundings ctx gen (paymentPlan ctx) [] 20 SenderPays `shouldBe` Right [ChangePays]

    -- With the coin of 30000 at /0/1 added, two inputs and two outputs,
    -- 374 bytes, pay 7480 at 20 satoshis a byte, and 80000 - 48080 - 7480
    -- = 24440 go to the change
    it "is a coin of the portfolio first, then the recipients" $
      fundings ctx gen (oneRecipientPlan ctx) [added30000] 20 SenderPays
        `shouldBe` Right [CoinsAdded [added30000], RecipientsReduced loneWayAt20]

    -- The payment spent a bip44 coin. With the bip84 coin of 30000 added,
    -- 10 + 148 + 41 + 2 * 34 = 267 bytes and (2 + 1 + 108) / 4 of witness
    -- data, 28, are 295 virtual bytes, 5900 at 20 satoshis a byte, which
    -- the 80000 - 48080 of the two coins pay
    it "is a bip84 coin of the portfolio as well as a bip44 one, whatever the type of the coins the payment spent" $
      fundings ctx gen (oneRecipientPlan ctx) [addedSegwit30000] 20 SenderPays
        `shouldBe` Right [CoinsAdded [addedSegwit30000], RecipientsReduced loneWayAt20]

    it "is the recipients alone when no coin of the portfolio would do" $
      fundings ctx gen (oneRecipientPlan ctx) [] 20 SenderPays `shouldBe` Right [RecipientsReduced loneWayAt20]

    -- At 300 satoshis a byte, the coin of 200000 pays the 112200 of 374
    -- bytes, with 89720 of change; the recipient would be left with nothing
    it "is a coin of the portfolio alone when the recipients cannot pay" $
      fundings ctx gen (oneRecipientPlan ctx) [added200000] 300 SenderPays `shouldBe` Right [CoinsAdded [added200000]]

    it "is refused when neither the coins nor the recipients can pay, telling both" $
      fundings ctx gen (oneRecipientPlan ctx) [] 300 SenderPays
        `shouldBe` Left "The coins of the transaction cannot pay its fee at 300 sat/vB, and no confirmed coins of the portfolio would. The recipient cannot pay the 55680 satoshis missing without being left with less than the dust limit of 546 satoshis"
  where
    gen = noDraw
    added30000 = addedCoin30000 ctx
    added200000 = addedCoin200000 ctx
    addedSegwit30000 = (storeCoin ctx (fakeTxHash 5, 0) (Just 800000) (segwitExternalAddr 0) 30000, segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))

-- | The generator of the funding tests. The coins to add are taken the
-- largest first: nothing is drawn.
noDraw :: StdGen
noDraw = mkStdGen 0

-- | Confirmed coins of 30000 and of 200000 at /0/1 of the bip44 account,
-- which a replacement can add
addedCoin30000, addedCoin200000 :: Ctx -> (Store.Unspent, Address, PoolPath)
addedCoin30000 ctx = (coinAt ctx 3 1 30000, externalAddr 1, legacyPath (Deriv :/ 0 :/ 1))
addedCoin200000 ctx = (coinAt ctx 4 1 200000, externalAddr 1, legacyPath (Deriv :/ 0 :/ 1))

-- | The way the three recipients of 'sendAllPlan' pay the 260 missing at
-- 11 satoshis a byte in equal shares: 260 / 3 is 86 each, and the first
-- also pays the 2 left over. 30000 - 88 = 29912, 10000 - 86 = 9914, and
-- 7400 - 86 = 7314.
sharedAt11 :: Reduction
sharedAt11 = way AllRecipients 260 (29912, 9914, 7314)

recipientsPaySpec :: Ctx -> Spec
recipientsPaySpec ctx =
  describe "What pays the fee of a replacement with --recipients-pay" $ do
    -- With its change, the payment is 226 bytes: 4520 at 20 satoshis a
    -- byte, 2260 more than the 2260 it paid. The recipient pays them, and
    -- keeps 20000 - 2260 = 17740. The change keeps its 27740.
    it "has the recipients pay what the higher fee adds, though the change could pay it" $
      fundings ctx gen (paymentPlan ctx) [] 20 RecipientsPay
        `shouldBe` Right [RecipientsReduced (Reduction AllRecipients 2260 [(otherAddr 0, 20000, 17740)])]

    it "is all the recipients in equal shares, the first also paying the remainder, with nothing asked" $
      fundings ctx gen (sendAllPlan ctx) [] 11 RecipientsPay `shouldBe` Right [RecipientsReduced sharedAt11]

    -- The coin of 30000 would pay, as it does without --recipients-pay.
    -- The lone recipient pays the 1920 missing instead.
    it "adds no coin of the portfolio, though one would do" $
      fundings ctx gen (oneRecipientPlan ctx) [added30000] 20 RecipientsPay `shouldBe` Right [RecipientsReduced loneWayAt20]

    -- At 100 satoshis a byte, 23400 are missing: 7800 each, more than the
    -- 7400 of the third recipient. The first one alone has enough.
    it "is refused when the shares would leave a recipient below the dust limit, pointing to one recipient paying alone" $
      fundings ctx gen (sendAllPlan ctx) [] 100 RecipientsPay
        `shouldBe` Left "The recipients cannot pay the 23400 satoshis the higher fee adds in equal shares without one being left with less than the dust limit of 546 satoshis. Without --recipients-pay, bumpfee offers one of the recipients paying it alone"

    -- At 300 satoshis a byte, 55680 are missing, more than the recipient
    -- receives, and the coin of 200000 would pay
    it "is refused when the recipients cannot pay, pointing to the coins of the portfolio" $
      fundings ctx gen (oneRecipientPlan ctx) [added200000] 300 RecipientsPay
        `shouldBe` Left "The recipient cannot pay the 55680 satoshis the higher fee adds without being left with less than the dust limit of 546 satoshis. Without --recipients-pay, bumpfee offers to add confirmed coins of the portfolio"

    -- At 97 satoshis a byte, the 226 bytes pay 21922, 19662 more than the
    -- 2260 paid: the recipient would keep 20000 - 19662 = 338. The change
    -- would keep 50000 - 20000 - 21922 = 8078.
    it "is refused when the recipient cannot pay what the higher fee adds, pointing to the change" $
      fundings ctx gen (paymentPlan ctx) [] 97 RecipientsPay
        `shouldBe` Left "The recipient cannot pay the 19662 satoshis the higher fee adds without being left with less than the dust limit of 546 satoshis. Without --recipients-pay, the change pays it"

    -- The 226 bytes at 20 satoshis a byte pay 4520, less than the 5000 the
    -- transactions replaced paid: the replacement must pay 5000 + 226 =
    -- 5226, 5226 / 226 = 23.1 satoshis a byte, so 24
    it "is refused, as the replacement would be, at a rate that does not pay the fees of the transactions replaced" $
      fundings ctx gen (paymentPlan ctx) {planFees = 5000} [] 20 RecipientsPay
        `shouldBe` Left "The replacement pays 4520 satoshis of fee, and must pay 5226: the fees of the transactions it replaces, and its own 226 virtual bytes. Give a --fee-rate of at least 24"

    it "is refused as without --recipients-pay when nothing would pay" $
      fundings ctx gen (oneRecipientPlan ctx) [] 300 RecipientsPay
        `shouldBe` Left "The coins of the transaction cannot pay its fee at 300 sat/vB, and no confirmed coins of the portfolio would. The recipient cannot pay the 55680 satoshis missing without being left with less than the dust limit of 546 satoshis"
  where
    gen = noDraw
    added30000 = addedCoin30000 ctx
    added200000 = addedCoin200000 ctx

{- A portfolio with coins of both types -}

-- The portfolio has a coin of 30000 at the first receiving address of its
-- bip44 account and one of 20000 at that of its bip84 account, both in a
-- block. The stuck payment spends the two: 40000 to another wallet and
-- 7080 of change to the first change address of the bip84 account. It is
-- 10 + 148 + 41 + 34 + 31 = 264 bytes and (2 + 1 + 108) / 4 of witness
-- data, 28: 292 virtual bytes, which paid 2920 at 10 satoshis a byte.

-- | The transactions the two coins come from
legacyFunding, segwitFunding :: Ctx -> Tx
legacyFunding ctx = fundingWorth ctx [30000]
segwitFunding ctx = segwitFundingWorth ctx [20000]

-- | The stuck payment of both coins, as the network has it
stuckOfBothTypes :: Ctx -> IO Sent
stuckOfBothTypes ctx = do
  let tx = builtTx ctx 0 [(txHash (legacyFunding ctx), 0), (txHash (segwitFunding ctx), 0)] [(otherAddr 0, 40000), (segwitInternalAddr 0, 7080)]
      parents = [legacyFunding ctx, segwitFunding ctx]
      tsd = TxSignData tx parents [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)] [segwitPath (Deriv :/ 1 :/ 0)] (Just 10) Nothing [] noLabel
  info <- expectRight $ parseTxSignData btc ctx (testPoolPubKeys ctx) tsd
  return $ Sent tx info.txInfoSummary parents ""

-- | Portfolio 0 of the test wallet on Bitcoin, with the first receiving
-- address of each of its two accounts and the first change address of the
-- bip84 one, and the two funding transactions in its history, in block
-- 800000
poolOfBothTypes :: Ctx -> ExceptT String (DB IO) Pool
poolOfBothTypes ctx = do
  pool <- testPool ctx
  (legacyId, _) <- testNewAcc ctx
  (segwitId, _) <- testNewSegwitAcc ctx
  extendChainTo ctx legacyId AddrExternal 1
  extendChainTo ctx segwitId AddrExternal 1
  extendChainTo ctx segwitId AddrInternal 1
  -- What the summaries say does not matter here: the plan reads the block
  -- of each transaction
  _ <- lift $ repsertSummary pool (txHash (legacyFunding ctx)) (Store.BlockRef 800000 1) seenTime paymentSummary
  _ <- lift $ repsertSummary pool (txHash (segwitFunding ctx)) (Store.BlockRef 800000 2) seenTime paymentSummary
  return pool

-- | The plan of the replacement of the stuck payment in that portfolio,
-- with the configuration given
planOfBothTypes :: Ctx -> Config -> ExceptT String (DB IO) (Pool, BumpPlan)
planOfBothTypes ctx cfg = do
  pool <- poolOfBothTypes ctx
  stuck <- liftIO $ stuckOfBothTypes ctx
  plan <- bumpPlan ctx cfg pool [stuck]
  return (pool, plan)

-- | The outputs of a transaction of Bitcoin as addresses and amounts, by
-- amount, since the builder shuffles them
paidTo :: Ctx -> TxSignData -> [(Address, Word64)]
paidTo ctx tsd = sortOn snd [(forceRight (scriptToAddressBS btc ctx out.script), out.value) | out <- tsd.txSignDataTx.outputs]

bothTypesSpec :: Ctx -> Spec
bothTypesSpec ctx =
  describe "The replacement of a payment that spent coins of both types" $ do
    it "keeps the coins of both types, each with the path of its own account" $
      inDatabaseE $ do
        (_, plan) <- planOfBothTypes ctx def
        liftIO $ do
          [(coin.outpoint, coin.value) | coin <- plan.planCoins]
            `shouldBe` [(OutPoint (txHash (legacyFunding ctx)) 0, 30000), (OutPoint (txHash (segwitFunding ctx)) 0, 20000)]
          plan.planCoinPaths
            `shouldBe` Map.fromList [(externalAddr 0, legacyPath (Deriv :/ 0 :/ 0)), (segwitExternalAddr 0, segwitPath (Deriv :/ 0 :/ 0))]

    it "keeps the change address of the payment, with its type, and pays the same recipient" $
      inDatabaseE $ do
        (_, plan) <- planOfBothTypes ctx def
        liftIO $ do
          plan.planChange `shouldBe` Just (segwitInternalAddr 0, segwitPath (Deriv :/ 1 :/ 0))
          plan.planRecipients `shouldBe` [(otherAddr 0, 40000)]
          plan.planFees `shouldBe` 2920

    -- At 20 satoshis a byte the 292 virtual bytes pay 5840, and 50000 -
    -- 40000 - 5840 = 4160 go to the change. The coins are of both types
    -- and the default type is bip44: the change stays a bip84 one all the
    -- same, at the address the payment replaced paid it to.
    it "spends the coins of both types again, and pays its change at the bip84 address of the payment it replaces" $
      inDatabaseE $ do
        (pool, plan) <- planOfBothTypes ctx def
        tsd <- buildBump ctx def pool (mkStdGen 0) plan ChangePays 20 0
        liftIO $ do
          ((.outpoint) <$> tsd.txSignDataTx.inputs)
            `shouldBe` [OutPoint (txHash (legacyFunding ctx)) 0, OutPoint (txHash (segwitFunding ctx)) 0]
          paidTo ctx tsd `shouldBe` [(segwitInternalAddr 0, 4160), (otherAddr 0, 40000)]
          tsd.txSignDataInputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)]
          tsd.txSignDataOutputPaths `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    it "names the payment it replaces, and carries the transactions of both coins" $
      inDatabaseE $ do
        (pool, plan) <- planOfBothTypes ctx def
        stuck <- liftIO $ stuckOfBothTypes ctx
        tsd <- buildBump ctx def pool (mkStdGen 0) plan ChangePays 20 0
        liftIO $ do
          tsd.txSignDataReplaces `shouldBe` [sentId stuck]
          tsd.txSignDataInputs `shouldBe` [legacyFunding ctx, segwitFunding ctx]

    -- The send-all of the bip44 coin of 50000 to one address, 48080, had
    -- no change. With the bip84 coin of 30000 added, the replacement has
    -- coins of both types, so its new change is of the default type,
    -- bip44: 295 virtual bytes pay 5900 at 20 satoshis a byte, and 80000 -
    -- 48080 - 5900 = 26020 go to the first change address of the bip44
    -- account.
    it "adds a bip84 coin to a payment of a bip44 coin, with new change of the default type" $
      inDatabaseE $ do
        pool <- poolOfBothTypes ctx
        lift $ insertRawTx (segwitFundingWorth ctx [30000])
        tsd <- buildBump ctx def pool (mkStdGen 0) (oneRecipientPlan ctx) (CoinsAdded [addedSegwit]) 20 0
        liftIO $ do
          paidTo ctx tsd `shouldBe` [(internalAddr 0, 26020), (otherAddr 0, 48080)]
          tsd.txSignDataInputPaths `shouldBe` [legacyPath (Deriv :/ 0 :/ 0), segwitPath (Deriv :/ 0 :/ 0)]
          tsd.txSignDataOutputPaths `shouldBe` [legacyPath (Deriv :/ 1 :/ 0)]

    -- The same with bip84 as the default type of the configuration: the
    -- change is a P2WPKH output, 3 bytes less, so 292 virtual bytes pay
    -- 5840, and 80000 - 48080 - 5840 = 26080 go to the first change
    -- address of the bip84 account
    it "pays new change of a replacement with coins of both types at the default type of the configuration" $
      inDatabaseE $ do
        pool <- poolOfBothTypes ctx
        lift $ insertRawTx (segwitFundingWorth ctx [30000])
        let plan = (oneRecipientPlan ctx) {planDefaultKind = Bip84}
        tsd <- buildBump ctx def pool (mkStdGen 0) plan (CoinsAdded [addedSegwit]) 20 0
        liftIO $ do
          paidTo ctx tsd `shouldBe` [(segwitInternalAddr 0, 26080), (otherAddr 0, 48080)]
          tsd.txSignDataOutputPaths `shouldBe` [segwitPath (Deriv :/ 1 :/ 0)]

    it "takes the default type of new change from the key default-type of the configuration, bip44 without it" $
      inDatabaseE $ do
        (_, plain) <- planOfBothTypes ctx def
        liftIO $ plain.planDefaultKind `shouldBe` Bip44
        stuck <- liftIO $ stuckOfBothTypes ctx
        pool <- testPool ctx
        preferring <- bumpPlan ctx (configWith ["default-type" .= ("bip84" :: Text)]) pool [stuck]
        liftIO $ preferring.planDefaultKind `shouldBe` Bip84
  where
    -- The coin of 30000 at the first receiving address of the bip84
    -- account, confirmed
    addedSegwit :: (Store.Unspent, Address, PoolPath)
    addedSegwit =
      ( storeCoin ctx (txHash (segwitFundingWorth ctx [30000]), 0) (Just 800000) (segwitExternalAddr 0) 30000,
        segwitExternalAddr 0,
        segwitPath (Deriv :/ 0 :/ 0)
      )

reducedDocSpec :: Spec
reducedDocSpec =
  describe "What bumpfee says before it asks which recipient pays" $ do
    -- In satoshis, as --unit sat asks
    it "numbers the recipients, each with what it receives, then what it would receive paying alone and paying its share when all can" $ do
      renderedLines (reducedDoc btc UnitSatoshi 11 waysAt11)
        `shouldBe` [ "The coins of the transaction cannot pay its fee at 11 sat/vB, and no coin of the portfolio is added. The 260 sats missing can come out of what the recipients receive, one of them paying it alone, or all of them in equal shares:",
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
        `shouldBe` [ "The coins of the transaction cannot pay its fee at 100 sat/vB, and no coin of the portfolio is added. The 23400 sats missing can come out of what the recipients receive, one of them paying it alone:",
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
        `shouldBe` [ "The coins of the transaction cannot pay its fee at 20 sat/vB, and no coin of the portfolio is added. The recipient can pay the 0.00001920 btc missing out of what it receives:",
                     "",
                     "  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00048080 btc",
                     "    0.00046160 btc paying it"
                   ]
      reducedQuestion [loneWayAt20] `shouldBe` "Reduce what it receives?"

sharedDocSpec :: Spec
sharedDocSpec =
  describe "What bumpfee says when the recipients pay in equal shares without a question" $ do
    -- In satoshis, as --unit sat asks
    it "names each recipient with what it receives and what it receives paying its share, numbering none" $
      renderedLines (sharedDoc RecipientsPay btc UnitSatoshi 11 sharedAt11)
        `shouldBe` [ "As --recipients-pay asks, the recipients pay the 260 sats the higher fee adds at 11 sat/vB, in equal shares, out of what they receive:",
                     "",
                     "  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  30000 sats",
                     "    29912 sats paying its share",
                     "",
                     "  1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ  10000 sats",
                     "    9914 sats paying its share",
                     "",
                     "  191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q  7400 sats",
                     "    7314 sats paying its share"
                   ]

    -- In bitcoins, the unit by default: 1920 satoshis are 0.00001920 btc
    it "tells a lone recipient that it pays it all" $
      renderedLines (sharedDoc RecipientsPay btc UnitBitcoin 20 loneWayAt20)
        `shouldBe` [ "As --recipients-pay asks, the recipient pays the 0.00001920 btc the higher fee adds at 20 sat/vB, out of what it receives:",
                     "",
                     "  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00048080 btc",
                     "    0.00046160 btc paying it"
                   ]

    -- Without --recipients-pay, the sender pays: --yes took the shares
    -- because nothing else could pay what is missing
    it "says that --yes had the recipients pay in equal shares what the coins cannot" $
      take 1 (renderedLines (sharedDoc SenderPays btc UnitSatoshi 11 sharedAt11))
        `shouldBe` ["The coins of the transaction cannot pay its fee at 11 sat/vB, and no coin of the portfolio is added. With --yes, the recipients pay the 260 sats missing in equal shares, out of what they receive:"]
