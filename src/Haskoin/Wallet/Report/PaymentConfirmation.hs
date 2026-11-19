{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The payment confirmation of the command report: one payment, on one
-- page or two, as a report ("Haskoin.Wallet.Report.Layout").
--
-- It is the advice a bank prints for one transfer, for a wallet no bank
-- holds. The two statements are for a tax office or an accountant, who
-- want everything. This one is for the other side of one payment, or for
-- the invoice it settles in the books of the holder: it says that an
-- amount was paid to an address on a day, and how anyone can check it.
--
-- = Which side the wallets are on
--
-- A payment has a payer and a recipient, and the wallets are one of the
-- two (the type t'PaymentSide').
--
-- * The wallets made the transaction: each of its inputs spends one of
--   their coins. The report is a /confirmation/ of the payment they
--   sent. It lists what the transaction pays to the addresses of
--   others.
--
-- * The wallets spend nothing in the transaction, and it pays some of
--   their addresses. The report is a /receipt/ of the payment they
--   received. It lists what the transaction pays to their addresses.
--
-- A transaction that spends coins of the wallets and coins of others
-- has no single payer, and is refused. So is one that only moves coins
-- between the addresses of the wallets, which pays nobody.
--
-- "The wallets" are the portfolios the report covers (the type
-- t'StatementScope'): every portfolio of the database by default, as for
-- the statements. A payment from one portfolio to another is then a
-- transfer, and no payment. The options --wallet and --portfolio narrow the
-- side down, and the same transaction becomes a payment: sent, when they
-- name the portfolio that paid, and received, when they name the one that
-- was paid. A wallet may be that of a business, and another that of its
-- owner.
--
-- = What it leaves out
--
-- It lists no change. What a transaction returns to its payer is no
-- payment, and the recipient has no use for it.
--
-- It uses no label and no contact of the wallet. The statements
-- describe a transaction by its label, since they go to the accountant
-- of the holder. A confirmation goes to the other side, and a label is
-- a private note. So it names the two sides and the purpose of the
-- payment only as the options give them.
--
-- It takes confirmed transactions alone. An unconfirmed one can still
-- be replaced, and a confirmation of it could be of a payment that
-- never happened.
--
-- With addresses given, it lists what the transaction pays to those
-- alone. A transaction may pay several suppliers at once, and the
-- confirmation one of them gets need not tell what the others were
-- paid. It then says that the transaction makes other payments, and how
-- much they add up to.
--
-- = What it shares with the statements
--
-- The style is that of every report. The reference, the prices, the way
-- a block is named and the title that says "Digital Asset" are those of
-- the statements ("Haskoin.Wallet.Report.Statement"). The facts and the
-- notes are its own: a statement speaks of "the holder", and this
-- report of "the payer" and "the recipient".
module Haskoin.Wallet.Report.PaymentConfirmation
  ( PaymentSide (..),
    PaidAddress (..),
    PaymentConfirmation (..),
    paymentConfirmation,
    paymentValued,
    paymentBlocksUnnamed,
    namingPaymentBlock,
    paymentReference,
    paymentReport,
  )
where

import Control.Monad (forM, unless, when)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (LocalTime (..), TimeZone, ZonedTime (..), utcToLocalTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Journal (satoshisText)
import Haskoin.Wallet.Cli.Sheet (ZoneAt)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), WalletNaming (..), portfolioRefName, walletName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Price (priceText, valueAt, valueText)
import Haskoin.Wallet.Report.Statement
import Numeric.Natural (Natural)

{- The payment -}

-- | The side of a payment the wallets are on
data PaymentSide
  = -- | The wallets made the transaction, and paid others
    PaymentSent
  | -- | Others made the transaction, and paid the wallets
    PaymentReceived
  deriving (Eq, Show)

-- | An address a payment pays, as its network writes it, and what the
-- transaction pays to it in satoshis. Two outputs to one address count
-- as one.
data PaidAddress = PaidAddress
  { paidAddress :: !Text,
    paidAmount :: !Natural
  }
  deriving (Eq, Show)

-- | A payment, as its confirmation states it
data PaymentConfirmation = PaymentConfirmation
  { paymentSide :: !PaymentSide,
    paymentNetwork :: !Network,
    paymentTxid :: !TxHash,
    -- | The portfolios whose side the confirmation is on: every wallet,
    -- one wallet or one portfolio
    paymentScope :: !StatementScope,
    -- | When the block of the transaction is dated, on the clock of the
    -- computer, with the zone the clock has then. Nothing for a block
    -- without a time.
    paymentTime :: !(Maybe (LocalTime, TimeZone)),
    -- | The block the transaction is in
    paymentBlock :: !BlockNamed,
    -- | That block and the blocks built on it, up to the last sync
    paymentConfirmations :: !Natural,
    -- | The block the last sync of the network reached
    paymentSynced :: !(Maybe BlockNamed),
    -- | The addresses the confirmation lists, the one paid the most
    -- first: those of others for a payment sent, and those of the
    -- wallets for a payment received
    paymentPaid :: ![PaidAddress],
    -- | What a payment sent pays to no address, as an output that
    -- carries data or a script of its own does. It is part of what was
    -- paid.
    paymentUnaddressed :: !Natural,
    -- | What the transaction pays that the confirmation leaves out,
    -- because the addresses to list were given: the other addresses of
    -- the same side, and for a payment sent, what goes to no address
    paymentLeftOut :: !Natural,
    -- | The fee of the transaction, which the payer paid
    paymentFee :: !Natural,
    -- | What a payment sent returns to the payer as change
    paymentChange :: !Natural,
    -- | Who holds the wallets, as the user says it, or nothing: the
    -- payer of a payment sent, the recipient of a payment received
    paymentHolder :: !Text,
    -- | The other side, as the user says it, or nothing
    paymentCounterparty :: !Text,
    -- | What the payment is for, as the user says it, or nothing
    paymentPurpose :: !Text,
    -- | The price the amounts are valued at, when the user gave one
    paymentValuation :: !(Maybe Valuation)
  }
  deriving (Eq, Show)

-- | What the confirmation lists adds up to: the amount paid, or the
-- amount received
paymentTotal :: PaymentConfirmation -> Natural
paymentTotal payment = sum (paidAmount <$> paymentPaid payment) + paymentUnaddressed payment

-- | Whether the confirmation leaves out a part of what the transaction
-- pays to the same side
paymentPartial :: PaymentConfirmation -> Bool
paymentPartial payment = paymentLeftOut payment > 0

{- Working it out -}

-- | The confirmation of the payment a transaction made, from the side of
-- the portfolios a scope covers, listing what it pays to the addresses
-- given, or to every address when none is given. The histories are
-- those of every portfolio of the database, covered or not, each with
-- the transaction as the portfolio sees it when it does. They may hold
-- other transactions, which are left aside. The networks come each with
-- the block the last sync reached. The function gives the zone of the
-- clock of the computer at a time.
--
-- It is refused when the wallet does not have the transaction, when the
-- transaction is not in a block, when the scope has no part in it, when
-- it has no single payer, when it pays nobody, and when an address
-- given is not one it pays. Each refusal says which.
paymentConfirmation :: StatementScope -> ZoneAt -> TxHash -> [Text] -> [(Network, Maybe SyncedTo)] -> [PortfolioHistory] -> Either String PaymentConfirmation
paymentConfirmation asked zoneAt txid listed networks histories = do
  (net, firstInfo) <- maybe (Left ("The wallet has no transaction " <> named)) Right (listToMaybe [(viewNet, info) | (viewNet, _, info) <- everyView])
  (status, height) <- confirmedStatus firstInfo
  -- The same keys hold coins on every test network, and a transaction
  -- is of one of them
  let views = [(ref, info) | (viewNet, ref, info) <- everyView, viewNet == net]
      scoped = [view | view@(ref, _) <- views, covers scope ref]
      summaries = txInfoSummary . snd <$> scoped
  when (null scoped) . Left $
    capitalised scopeName <> " has no part in the transaction " <> named <> ": it is a transaction of " <> cs (namesOf (fst <$> views))
  let spends = any (not . Map.null . summaryMyInputs) summaries
      madeByScope = walletTransaction summaries
      -- Every summary of a transaction has the same fee and the same
      -- outputs. They differ in which outputs they count as their own.
      anySummary = txInfoSummary firstInfo
      own = Map.unions [myOutputsValue <$> summaryMyOutputs summary | summary <- summaries]
      everyOutput = Map.union (myOutputsValue <$> summaryMyOutputs anySummary) (summaryOtherOutputs anySummary)
      toOthers = everyOutput `Map.difference` own
      unaddressed = sum [fromIntegral value | Store.StoreOutput value _ _ _ <- summaryNonStdOutputs anySummary]
  when (spends && not madeByScope) . Left $
    "The transaction " <> named <> " spends coins of " <> scopeName <> " and coins of others: a confirmation needs one payer, whose coins the transaction spends alone"
  let side = if spends then PaymentSent else PaymentReceived
      -- What the confirmation may list, and what it lists besides the
      -- addresses when none is given
      (candidates, besides) = case side of
        PaymentSent -> (toOthers, unaddressed)
        PaymentReceived -> (own, 0)
  when (Map.null candidates && besides == 0) . Left $
    "The transaction " <> named <> " pays no one outside " <> scopeName <> ": it moves coins between its own addresses." <> transferHint scoped
  chosen <- forM (nub listed) $ \typed -> do
    address <- maybe (Left (cs typed <> " is not an address of " <> net.name)) Right (textToAddr net typed)
    unless (address `Map.member` candidates) . Left $
      "--address names " <> cs typed <> ", which is not among the addresses the " <> documentOf side <> " lists: " <> cs (Text.intercalate ", " (paidAddress <$> paidOf net candidates))
    return address
  let kept = if null listed then candidates else Map.restrictKeys candidates (Map.keysSet (Map.fromList [(address, ()) | address <- chosen]))
      keptBesides = if null listed then besides else 0
      leftOut = (sum (Map.elems candidates) + besides) - (sum (Map.elems kept) + keptBesides)
      fee = summaryFee anySummary
      change = case side of
        PaymentSent -> sum (Map.elems own)
        PaymentReceived -> 0
  -- What left the payer must be what the transaction pays, its fee
  -- included. A history that says otherwise is not sound, and a
  -- confirmation must not state its amounts.
  when (side == PaymentSent) $
    unless (negate (sum (summaryAmount <$> summaries)) == toInteger (sum (Map.elems toOthers) + unaddressed + fee)) . Left $
      "The amounts of the transaction " <> named <> " do not add up: hw sync --full reads it again"
  return
    PaymentConfirmation
      { paymentSide = side,
        paymentNetwork = net,
        paymentTxid = txid,
        paymentScope = scope,
        paymentTime = (\time -> (utcToLocalTime (zoneAt time) time, zoneAt time)) <$> chainTime status,
        paymentBlock = countedBlock zoneAt (syncedOn net) [firstInfo] height,
        paymentConfirmations = chainConfirmations status,
        paymentSynced = syncedBlock zoneAt <$> syncedOn net,
        paymentPaid = paidOf net kept,
        paymentUnaddressed = keptBesides,
        paymentLeftOut = leftOut,
        paymentFee = fee,
        paymentChange = change,
        paymentHolder = "",
        paymentCounterparty = "",
        paymentPurpose = "",
        paymentValuation = Nothing
      }
  where
    named :: String
    named = cs (txHashToHex txid)
    scope = scopeAmong histories asked
    naming = databaseNaming histories
    -- The transaction as each portfolio that has it sees it, on each
    -- network
    everyView =
      [ (net, historyRef history, info)
        | history <- histories,
          (net, transactions) <- historyPools history,
          info <- listedTxInfo <$> transactions,
          namedBy info
      ]
    -- Whether the hash given names a transaction: its txid does, and
    -- while it is in preparation, so does the hash of the transaction
    -- without its signatures, which is how the user knows it then
    namedBy info = case txInfoStatus info of
      OnChain status -> chainTxid status == txid
      Pending status -> pendingNosigHash status == txid || pendingTxid status == Just txid
    syncedOn net = listToMaybe [synced | (syncedNet, Just synced) <- networks, syncedNet == net]
    -- A transaction in a block, with the height of the block, or why it
    -- is not one
    confirmedStatus info = case txInfoStatus info of
      Pending _ ->
        Left ("The transaction " <> named <> " is in preparation, and has paid nothing yet: a payment is confirmed once its transaction is in a block")
      OnChain status
        | chainDead status ->
            Left ("The transaction " <> named <> " was replaced or dropped from the network, and paid nothing: hw show tells whether another transaction took its place")
        | Store.BlockRef height _ <- chainBlock status -> Right (status, height)
        | otherwise ->
            Left ("The transaction " <> named <> " is not confirmed yet, and could still be replaced: a payment is confirmed once its transaction is in a block. Run hw sync once it is")
    -- The side of the confirmation, as a sentence names it
    scopeName :: String
    scopeName = case scope of
      EveryWallet -> if naming == WithWallets then "the wallets" else "the wallet"
      OneWallet wallet -> cs (walletName wallet)
      OnePortfolio ref -> cs (portfolioRefName WithWallets ref)
    namesOf :: [PortfolioRef] -> Text
    namesOf refs = Text.intercalate " and " (portfolioRefName WithWallets <$> nub refs)
    -- A payment from one portfolio of the side to another is a payment
    -- once the side is one of the two
    transferHint :: [(PortfolioRef, TxInfo)] -> String
    transferHint scoped
      | length scoped > 1 = " To confirm it as a payment between " <> cs (namesOf (fst <$> scoped)) <> ", name the one that paid, or the one that was paid, with --wallet and --portfolio"
      | otherwise = ""

-- | The addresses of a map as a confirmation lists them: the one paid
-- the most first, and those paid the same in the order of their text
paidOf :: Network -> Map.Map Address Natural -> [PaidAddress]
paidOf net paid =
  sortOn
    (\line -> (Down (paidAmount line), paidAddress line))
    [PaidAddress text value | (address, value) <- Map.assocs paid, Just text <- [addrToText net address]]

-- | What the report of a side is called in a sentence
documentOf :: PaymentSide -> String
documentOf side = case side of
  PaymentSent -> "confirmation"
  PaymentReceived -> "receipt"

-- | A text with its first letter in capitals, to start a sentence with
capitalised :: String -> String
capitalised text = cs (Text.toUpper (Text.take 1 (cs text)) <> Text.drop 1 (cs text))

{- The price and the block -}

-- | The confirmation with its amounts valued at the price given. A
-- payment is in one asset, so it takes one price, that of its network.
-- A price of another network is refused, since it would be left aside
-- without a word, and so are two prices of one network.
paymentValued :: Valuation -> PaymentConfirmation -> Either String PaymentConfirmation
paymentValued valuation payment = do
  case [priced | priced <- fst <$> valuationPrices valuation, priced /= net] of
    other : _ -> Left ("The payment is on " <> net.name <> ", and --price names " <> other.name <> ": give the price of a coin of " <> net.name <> " alone")
    [] -> Right ()
  case valuationPrices valuation of
    [_] -> Right ()
    [] -> Left ("Give the price of a coin of " <> net.name <> " with --price " <> net.name <> "=PRICE")
    _ -> Left ("--price names " <> net.name <> " " <> show (length (valuationPrices valuation)) <> " times: give one price")
  return payment {paymentValuation = Just valuation}
  where
    net = paymentNetwork payment

-- | The block of the payment when the confirmation does not know its
-- hash yet, with its network: what the command asks the server
paymentBlocksUnnamed :: PaymentConfirmation -> [(Network, BlockHeight)]
paymentBlocksUnnamed payment =
  [(paymentNetwork payment, blockNamedHeight block) | let block = paymentBlock payment, blockNamedHash block == Nothing]

-- | The confirmation with the hash given for its block, among those
-- given for a network and a height. A block whose hash is already known
-- keeps it, and one no hash is given for stays without.
namingPaymentBlock :: [(Network, BlockHeight, BlockHash)] -> PaymentConfirmation -> PaymentConfirmation
namingPaymentBlock hashes payment = payment {paymentBlock = withBlockHash hashes (paymentNetwork payment) (paymentBlock payment)}

{- The reference -}

-- | The reference of a confirmation: eight hexadecimal digits in two
-- groups, as in 3F9A-52C1, worked out from what it states
-- ('referenceOf'): the transaction, the side, the two names, the
-- purpose, the scope when it is narrowed down, each address listed and
-- its amount, the fee of a payment sent and the price.
--
-- The time the confirmation was made is left out, and so are the
-- confirmations of the block, which grow with every sync. So a
-- confirmation made twice has one reference, as long as it states the
-- same payment in the same words.
paymentReference :: PaymentConfirmation -> Text
paymentReference payment =
  referenceOf $
    [ "payment " <> txHashToHex (paymentTxid payment),
      "network " <> cs (paymentNetwork payment).name,
      "side " <> cs (documentOf (paymentSide payment)),
      "holder " <> paymentHolder payment,
      "counterparty " <> paymentCounterparty payment,
      "purpose " <> paymentPurpose payment
    ]
      <> scopeStated (paymentScope payment)
      <> [Text.unwords ["paid", paidAddress line, shown (paidAmount line)] | line <- paymentPaid payment]
      <> ["unaddressed " <> shown (paymentUnaddressed payment) | paymentUnaddressed payment > 0]
      -- A receipt states no fee, so its reference states none either
      <> ["fee " <> shown (paymentFee payment) | paymentSide payment == PaymentSent]
      <> valuationStated (paymentValuation payment)
  where
    shown :: (Show a) => a -> Text
    shown = Text.pack . show

{- The report -}

-- | The confirmation as a report, made at the time given: the facts
-- about the payment, the addresses it pays with what each is paid and
-- their total, the transaction and its block, and the notes that say
-- who made the report and how to check it.
--
-- A payment sent is titled as a confirmation, and a payment received
-- as a receipt. The two read the same way, with the words of their
-- side: "paid to" and "received at", "the payer" and "the recipient".
--
-- A payment sent states its fee and what left the payer in all. A
-- payment received states no fee: the payer paid it, and it is no part
-- of what the recipient got.
paymentReport :: ZonedTime -> PaymentConfirmation -> Report
paymentReport prepared payment =
  Report
    { reportKind = kind,
      reportTitle = title,
      reportSubtitle = subtitle,
      reportFooter = title <> " \xB7 " <> subtitle <> " \xB7 Ref. " <> reference,
      reportBlocks =
        [FactsBlock facts, SectionBlock "Payment", TableBlock paid]
          <> [FactsBlock amounts | not (null amounts)]
          <> [SectionBlock "Blockchain data", FactsBlock chain, SectionBlock "Notes", NotesBlock notes]
    }
  where
    side = paymentSide payment
    net = paymentNetwork payment
    unit = assetUnit net
    valuation = paymentValuation payment
    price = priceOf valuation net
    reference = paymentReference payment
    partial = paymentPartial payment
    total = paymentTotal payment
    -- The words of each side
    (kind, title, document, preparer, heldBy) = case side of
      PaymentSent -> ("Confirmation of a cryptocurrency payment", statementTitle "Payment Confirmation", "confirmation", "payer", "made from")
      PaymentReceived -> ("Receipt of a cryptocurrency payment", statementTitle "Payment Receipt", "receipt", "recipient", "received in") :: (Text, Text, Text, Text, Text)
    -- The payer and the recipient, as the user named them
    (payer, recipient) = case side of
      PaymentSent -> (paymentHolder payment, paymentCounterparty payment)
      PaymentReceived -> (paymentCounterparty payment, paymentHolder payment)
    subtitle = case paymentTime payment of
      Just (time, _) -> "Payment of " <> longDate (localDay time)
      Nothing -> "Payment in block " <> shown (blockNamedHeight (paymentBlock payment))
    -- An amount of satoshis in coins, with its eight decimals and its
    -- unit
    amount :: Natural -> Text
    amount satoshis = satoshisText (toInteger satoshis) <> " " <> unit
    valueOf satoshis = (\priced -> valueText (valueAt priced satoshis)) <$> price
    shown :: (Show a) => a -> Text
    shown = Text.pack . show

    facts =
      [Fact "Payer" [payer] | not (Text.null payer)]
        <> [Fact "Recipient" [recipient] | not (Text.null recipient)]
        <> [Fact "Purpose" [paymentPurpose payment] | not (Text.null (paymentPurpose payment))]
        <> [Fact "Payment date" [longTime time <> inZone zone] | Just (time, zone) <- [paymentTime payment]]
        <> [ Fact
               (case side of PaymentSent -> "Amount paid"; PaymentReceived -> "Amount received")
               -- The value comes with its price, on one line, so that
               -- the reader sees at once what the value rests on
               ( amount total
                   : [ value <> " " <> valuationCurrency prices <> ", at " <> priceText priced <> " " <> valuationCurrency prices <> " per " <> unit
                       | Just prices <- [valuation],
                         Just priced <- [price],
                         Just value <- [valueOf total]
                     ]
               ),
             Fact "Asset" [assetTitle net],
             Fact "Prepared on" [longTime (zonedTimeToLocalTime prepared) <> inZone (zonedTimeZone prepared)],
             Fact "Prepared by" ["The " <> preparer <> ", from public blockchain data. Not audited."],
             Fact "Reference" [reference]
           ]

    -- The addresses the payment pays, each with what it is paid, and
    -- their total
    paid =
      Table
        ( [ Column (case side of PaymentSent -> "Paid to"; PaymentReceived -> "Received at") 9 AlignLeft,
            Column ("Amount (" <> unit <> ")") 4 AlignRight
          ]
            <> [Column ("Value (" <> valuationCurrency prices <> ")") 4 AlignRight | Just prices <- [valuation]]
        )
        ( [paidRow BodyRow (paidAddress line) (paidAmount line) | line <- paymentPaid payment]
            <> [paidRow BodyRow "No address: data, or a script of its own" (paymentUnaddressed payment) | paymentUnaddressed payment > 0]
            <> [paidRow TotalRow (case side of PaymentSent -> "Total paid"; PaymentReceived -> "Total received") total]
        )
    paidRow kindOfRow name satoshis =
      Row kindOfRow ([plainCell name, plainCell (satoshisText (toInteger satoshis))] <> [plainCell value | Just value <- [valueOf satoshis]])

    -- What the table leaves to say of the amounts: the fee of a payment
    -- sent and what left the payer in all, or what the transaction pays
    -- besides what is listed
    amounts = case side of
      PaymentSent
        | partial ->
            [ Fact "Network fee" [amount (paymentFee payment) <> ", paid by the payer for the whole transaction"],
              Fact "Other payments" [amount (paymentLeftOut payment) <> ", which this confirmation leaves out"]
            ]
        | otherwise ->
            [ Fact "Network fee" [amount (paymentFee payment) <> ", paid by the payer on top of the amount"],
              Fact "Total debited" [amount (total + paymentFee payment)]
            ]
      PaymentReceived ->
        [Fact "Other payments" [amount (paymentLeftOut payment) <> " to other addresses of the recipient, which this receipt leaves out"] | partial]

    -- The transaction and the blocks, which let a reader check the
    -- payment against the chain
    chain =
      [ Fact "Transaction ID" [txHashToHex (paymentTxid payment)],
        Fact "Block" (blockLines (paymentBlock payment)),
        Fact "Confirmations" [shown (paymentConfirmations payment) <> " at the last synchronisation" <> maybe "" reached (paymentSynced payment)]
      ]
    -- The block the last sync reached, which the confirmations are
    -- counted up to. It is on the line of the confirmations, without
    -- its hash: the block a reader checks is that of the payment.
    reached block = ", which reached block " <> shown (blockNamedHeight block) <> maybe "" (\time -> ", dated " <> longTime time) (blockNamedTime block)

    named = not (Text.null payer && Text.null recipient)
    -- The notes are few and short, so that a confirmation of one
    -- address fits on one page: it is a page to send, and a second one
    -- with two notes on it would look unfinished. So they do not repeat
    -- what a fact states, such as the payments a confirmation leaves
    -- out.
    notes =
      [ "This "
          <> document
          <> " was prepared by the "
          <> preparer
          <> ", from the data of a public blockchain. No third party has audited or confirmed it. The payment was "
          <> heldBy
          <> " a self-custody wallet, whose keys the "
          <> preparer
          <> " keeps: no bank, exchange or payment service "
          <> (case side of PaymentSent -> "carried it out."; PaymentReceived -> "holds the assets.")
          <> " Two "
          <> document
          <> "s with the same reference state the same transaction, names, purpose, amounts and price.",
        "The payment is a transaction on the "
          <> assetName net
          <> " blockchain, which is public. Anyone can look the transaction up by its ID, and check the addresses it pays, the amounts and its block, which is named by its height and its hash. The blockchain does not say who holds an address."
          <> (if named then " The names on this " <> document <> " are as the " <> preparer <> " gave them." else ""),
        case side of
          PaymentSent ->
            "The network fee went to the miners of the block. The payer paid it on top of the amount paid, so the addresses listed received that amount in full."
              <> (if paymentChange payment > 0 then " What the transaction returns to the payer as change is no payment, and is not listed." else "")
          PaymentReceived ->
            "The receipt lists what the transaction pays to the addresses of the recipient. What it pays to other addresses, such as the change of the payer, is not listed.",
        "The payment is dated as its block is, on the clock of the computer that prepared the "
          <> document
          <> ". The confirmations count that block and the blocks built on it up to the last synchronisation: the more there are, the harder the payment is to undo."
      ]
        -- Every amount is written with its unit, so the notes do not
        -- explain the unit as those of the statements do
        <> [ "Values are in "
               <> valuationCurrency prices
               <> ": an amount times the price of the asset, rounded to the nearest hundredth. The "
               <> preparer
               <> " gave the price."
               <> (if Text.null (valuationSource prices) then "" else " Its source: " <> valuationSource prices <> ".")
             | Just prices <- [valuation]
           ]
