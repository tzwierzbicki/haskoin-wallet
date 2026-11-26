{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about transactions: a transaction in its compact view, in
-- the tables of txs and of pendingtxs, and in the verbose view that show
-- gives and that a command shows before it signs or sends.
module Haskoin.Wallet.Cli.PrettyPrinter.Transaction
  ( listedTxDoc,
    whereFields,
    txAnswerDoc,
    localTxTime,
    headlineDoc,
    compactTxDoc,
    compactHeadline,
    Payee (..),
    mainPayee,
    txColumns,
    txRowDoc,
    pendingTableColumns,
    pendingRowDoc,
    verboseTxDoc,
    heldTxsDoc,
    signedDoc,
    sentDoc,
  )
where

import Data.List (intercalate, intersperse, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, maybeToList)
import Data.Ord (Down (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.PrettyPrinter.AddressWords (bookAddressWord, contactNote, contactNoteBelow, heldPathText, heldPathWord, ownAddressNote, ownAddressNoteBelow, trailingPathWord, withNotesBelow, wrappingLabelWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (amountCell, block, cappedConfirmations, cellsLine, changeSign, columns, field, fullConfirmations, labelWord, minuteWidth, minuteWord, neutralAmount, secondText, shownWidth, styled, tagMarks, text, valueText, withStyle)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (commandWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (PathNaming (..), SeenFrom (..), activePaths, pathsSeenFrom, portfolioWords, seenPortfolioDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (Column (..), Side (..), amountColumn, column, confsCell, shortIdWidth, tableLine, tagsCell, tagsColumn, timeCell, widestCell)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Address (isIntPath)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (PoolPath (..), kindName)
import Haskoin.Wallet.ShortId (shortHashText)
import Haskoin.Wallet.Tx.Addresses (HeldAddress (..), TxAddress (..), paidTo, spentFrom)
import Haskoin.Wallet.Tx.Size (PaidRate, paidRateText)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, annotate, fillSep, hcat, hsep, indent, pretty, punctuate, vcat, (<+>))

{- Transactions -}

-- A transaction shows in one of three views. The compact and the verbose
-- views give what changed and where it stands on their first line. The
-- compact view takes two lines. The first starts with the short id, the
-- first 8 digits of the hash, then gives the time, the amount and where
-- the transaction stands, the confirmations counted up to 999+. The second
-- gives the balance after it, in txs, then what it is for, in the words of
-- the table (@purposeCell@), which names an address of the wallet by its
-- path. A listing that keeps the transactions at tagged addresses adds a
-- line for each other address that has such a tag (@otherKeptAddresses@).
-- The verbose view starts with the time, the type, the amount and
-- the full count of the confirmations, then gives every detail as fields:
-- every address in full, and the hash in full with its short id. The
-- table gives a row of columns under a header: the short id, the time,
-- the type, the amount, the balance, the confirmations, and what the
-- transaction is for.

-- | A transaction as a listing shows it, in the view asked for, with its
-- time in the time zone of the computer. A listing of every portfolio
-- names the portfolio of each transaction, with its wallet or not as the
-- views name the wallets ('portfolioWords'). The balance after
-- it is the one txs gives: the other listings have none. The paths are
-- written as given (the type t'PathNaming'): txs lists the active portfolio
-- ('activePaths'), and a listing of every portfolio has transactions of
-- other ones ('Haskoin.Wallet.Cli.PrettyPrinter.Place.listedPaths').
listedTxDoc :: PathNaming -> View -> Network -> AmountUnit -> Maybe PortfolioRef -> Maybe Integer -> TxInfo -> IO (Doc Style)
listedTxDoc paths view net unit portfolioM balanceM info = do
  time <- localTxTime info
  return $ case view of
    Compact -> compactTxDoc paths net unit portfolioM balanceM time info
    Verbose -> verboseTxDoc paths net unit (whereFields net (listedWhere <$> portfolioM) info) balanceM time info
    -- The table needs the width of its column of tags: sectionsPrinter
    -- lays it out ('txRowDoc', 'pendingRowDoc')
    Table -> compactTxDoc paths net unit portfolioM balanceM time info
  where
    listedWhere ref = text (portfolioWords paths.namingWallets ref) `withStyle` Account

-- | The fields that say where a transaction is, first in its verbose
-- view: its portfolio, as the view names it, when it names it, its
-- network, and the types of the portfolio's addresses it involves, when
-- it involves any. A portfolio holds coins of both types on Bitcoin, and
-- the types tell which of them a transaction touched.
whereFields :: Network -> Maybe (Doc Style) -> TxInfo -> [Doc Style]
whereFields net portfolioM info =
  [field 9 "Portfolio" <> named | Just named <- [portfolioM]]
    <> [field 9 "Network" <> text net.name]
    <> [field 9 "Types" <> text (intercalate ", " (cs . kindName <$> kinds)) | let kinds = summaryKinds (txInfoSummary info), not (null kinds)]

-- | The one transaction a command answers with, with every detail, naming
-- its portfolio when it is one of the wallet, and its network. The user
-- sees whose coins it moves, and which coins: private keys, coins and the
-- legacy addresses of Bitcoin Cash read the same on Bitcoin, and a sweep
-- outside the wallet is on the network of its file. The portfolio says
-- when it is not the active one ('seenPortfolioDoc'), and every path then
-- names its portfolio ('pathsSeenFrom'). Each portfolio is named with its
-- wallet or not, as given.
txAnswerDoc :: WalletNaming -> Network -> Maybe SeenFrom -> UnitChoice -> TxInfo -> IO (Doc Style)
txAnswerDoc naming net seenM choice info = do
  time <- localTxTime info
  let paths = maybe (activePaths naming) (pathsSeenFrom naming) seenM
  return $ verboseTxDoc paths net (unitOn net choice) (whereFields net (seenPortfolioDoc naming <$> seenM) info) Nothing time info

-- | That time in the time zone of the computer, as the clock read it then.
-- A transaction of the summer shows summer time, even in winter.
localTxTime :: TxInfo -> IO (Maybe ZonedTime)
localTxTime = traverse utcToLocalZonedTime . txTime

-- | The first line of the verbose view of a transaction: its time, its
-- type, what it adds to the account or takes from it, fee included, and
-- where it stands. Its columns line up from one transaction to the next.
-- The function given writes the confirmations: 'fullConfirmations' in the
-- verbose view, 'cappedConfirmations' where a count that grows without end
-- would push the rest of a line.
--
-- An external transaction adds nothing to a portfolio and takes nothing
-- from it. Its amount is all that it spends, fee included, as the amount
-- of a payment is: without a sign or a colour, since it is neither in
-- nor out ('spentOfOthers').
headlineDoc :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
headlineDoc countText unit time (TxInfo summary status _ _ _ _ _) =
  hcat
    [ headlineTime status time,
      "  ",
      text (typeWord (summaryType summary)),
      "  ",
      amount,
      "  ",
      statusWords countText (summaryFeeRate summary) status
    ]
  where
    amount = case summaryType summary of
      TxExternal -> amountCell Nothing "" unit (toInteger (spentOfOthers summary))
      _ -> signedAmountCell unit (summaryAmount summary)

-- | The time a first line starts with ('minuteWord'). A transaction in
-- preparation without one is not held by the wallet, which read it from a
-- file: "from a file" says so, where "time unknown" read as a fault. A
-- file holds no time.
headlineTime :: TxStatus -> Maybe ZonedTime -> Doc Style
headlineTime (Pending _) Nothing = styled Muted (block minuteWidth "from a file")
headlineTime _ time = minuteWord time

-- | The length of the four words of 'typeWord'
typeWidth :: Natural
typeWidth = 8

-- | What a transaction adds to the account or takes from it, with its
-- sign, green when it comes in and red when it goes out, then its unit:
-- on the first line of the compact view, and in the column of a table
signedAmountCell :: AmountUnit -> Integer -> Doc Style
signedAmountCell unit amnt = amountCell style sign unit amnt
  where
    (style, sign) = changeSign amnt

-- | Where a transaction stands, in a few words. One that waits shows its
-- fee rate, which tells how soon a block may take it: unconfirmed, or in
-- preparation. One in conflict can no longer be sent, and a dead one was
-- replaced or dropped from the network: their fee rates no longer matter.
-- The function given writes the confirmations of one in a block.
statusWords :: (Natural -> String) -> PaidRate -> TxStatus -> Doc Style
statusWords countText paid = \case
  OnChain status
    | chainDead status -> styled Failure "dead"
    | chainConfirmations status == 0 -> styled Waiting "unconfirmed" <> rate
    | otherwise -> text (countText (chainConfirmations status))
  Pending status
    | pendingConflicted status -> styled Failure "conflicted"
    | pendingOnline status -> styled Good "sent" <> rate
    | pendingSigned status -> styled Waiting "signed, not sent" <> rate
    | otherwise -> styled Waiting "not signed" <> rate
  where
    rate = ", " <> paidRateDoc paid

-- | A transaction in two lines, for a listing that the eye runs down.
-- The first line gives the short id, the time, what the transaction adds
-- to the account or takes from it, with its sign and its colour, and
-- where it stands ('compactHeadline'). The short id comes first, as in
-- the first column of the table: the eye finds it there to copy it, and
-- each transaction starts where its id does.
--
-- The second line gives the details, a comma between them: the
-- portfolio and the network, in a listing of every portfolio, with the
-- wallet when the views name the wallets ('portfolioWords'); the
-- balance after the transaction, in
-- txs; the transactions a replacement that bumpfee prepared replaces, in
-- yellow (@replacesNote@); the transactions that replaced a dead one
-- (@replacedByNote@); and last what the transaction is for, in the
-- words of the last column of the table (@purposeCell@). The label of a payment says what it is for
-- better than the address it pays, and takes its place. What it is for
-- has no width to fix in advance, so it ends the line: a long address or
-- label pushes nothing after it. The verbose view gives every address in
-- full, and the type of the transaction, which the sign of the amount and
-- the words "at" and "to" tell here. A transaction with no detail, which
-- pays no address and has no label, takes one line. A path is written as
-- the paths of the view are (the type t'PathNaming').
--
-- A listing that keeps the transactions at tagged addresses
-- (--address-tag) adds lines below the second one. Each gives one
-- address that has a tag asked for and that the second line does not
-- name, with that tag (@otherKeptAddresses@). Such an address is often a
-- contact of the address book, written in full with its label. On the
-- second line it came after the balance and an address already, and the
-- line ran far past 80 columns. A line of its own also sets it apart
-- from what the transaction is for: it says why the transaction is in
-- the listing.
compactTxDoc :: PathNaming -> Network -> AmountUnit -> Maybe PortfolioRef -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
compactTxDoc paths net unit portfolioM balanceM time info =
  vcat $
    compactHeadline unit time info
      : [indent 2 (hcat (intersperse ", " details)) | not (null details)]
        <> (indent 2 <$> otherKeptAddresses paths net info)
  where
    details =
      [(text (portfolioWords paths.namingWallets ref) `withStyle` Account) <+> text net.name | Just ref <- [portfolioM]]
        <> ["balance" <+> neutralAmount unit balance | Just balance <- [balanceM]]
        <> maybeToList (replacesNote (txInfoStatus info))
        <> maybeToList (replacedByNote (txInfoStatus info))
        <> purposePart paths net info
        -- The tags of the transaction end the line, each between
        -- brackets
        <> [hsep (tagMarks tags) | let tags = txOwnTags (txInfoTags info), not (null tags)]

-- | What a replacement says of the transactions it replaces, by their
-- short ids: "replaces a1075db5". Nothing for a transaction that replaces
-- none. It is yellow, as what waits for something: the network keeps only
-- one of the transactions, and until it mines one of them, a payment
-- stands in two places. The user may not expect it, since the replaced
-- transaction is still in the history, so it stands out in every view
-- that shows a pending transaction.
replacesNote :: TxStatus -> Maybe (Doc Style)
replacesNote = \case
  Pending pending
    | not (null (pendingReplaces pending)) ->
        Just (("replaces" <+> shortIdList (pendingReplaces pending)) `withStyle` Waiting)
  _ -> Nothing

-- | What a dead transaction says of the transactions that replaced it, by
-- their short ids: "replaced by 2b69e4f4". Nothing for a live
-- transaction, and nothing for a dead one that names none
-- ('chainReplacedBy'): the network dropped it, or the portfolio cannot
-- tell. It answers what the user asks of a dead payment: whether it went
-- out in another transaction, and in which.
--
-- It has no colour. The word "dead" beside it is red already, and the
-- note waits for nothing, unlike the "replaces" of a pending
-- replacement.
replacedByNote :: TxStatus -> Maybe (Doc Style)
replacedByNote = \case
  OnChain status
    | not (null (chainReplacedBy status)) -> Just ("replaced by" <+> shortIdList (chainReplacedBy status))
  _ -> Nothing

-- | The first line of the compact view of a transaction: its short id,
-- faint for one on the chain (@hashWord@), its time to the minute, what
-- it adds to the account or takes from it, fee included, and where it
-- stands, the confirmations counted up to 999+. Its columns line up from
-- one transaction to the next. It leaves out the type: the sign and the
-- colour of the amount tell what comes in and what goes out, so the line
-- keeps to 80 columns.
compactHeadline :: AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
compactHeadline unit time (TxInfo summary status _ _ _ _ _) =
  columns
    [ hashWord status,
      minuteWord time,
      signedAmountCell unit (summaryAmount summary),
      statusWords cappedConfirmations (summaryFeeRate summary) status
    ]

-- | Short ids one after the other, as English lists them: "a1075db5",
-- "a1075db5 and b2d4f6a8", "a1075db5, b2d4f6a8 and 3c5e7a90"
shortIdList :: [TxHash] -> Doc Style
shortIdList hashes = case reverse (text . shortHashText <$> hashes) of
  [] -> mempty
  [only] -> only
  lastOne : others -> hcat (intersperse ", " (reverse others)) <+> "and" <+> lastOne

-- | The address a transaction pays the most, with the word before it and
-- how many other addresses it pays
data Payee = Payee
  { -- | "at" for a payment the wallet receives, which it is paid at, and
    -- "to" for the others
    payeePreposition :: !Text,
    payeeRow :: !TxAddress,
    payeeOthers :: !Int
  }

-- | The address a transaction pays the most, of those it sends to: the
-- address of another wallet, or of another portfolio, for a payment, one
-- of the portfolio for a payment received or one between its addresses.
-- None for a transaction that pays no address.
--
-- Between the addresses of the portfolio, a receiving address comes
-- before a change address, whatever each is paid. The user named the
-- receiving address, and the change of such a payment is often the larger
-- of the two. Naming the change first told the user that the payment went
-- to a change address.
--
-- In a payment received, an address of the portfolio comes before any
-- other, whatever each is paid. One in preparation lists the others too
-- ('paidTo'), and the portfolio is paid "at" its own address only.
mainPayee :: TxInfo -> Maybe Payee
mainPayee info =
  case sortOn rank (fst (paidTo info)) of
    [] -> Nothing
    first : others -> Just (Payee preposition first (length others))
  where
    kind = summaryType (txInfoSummary info)
    preposition = if kind == TxCredit then "at" else "to"
    rank row = (kind == TxCredit && not (ofViewed row), kind == TxInternal && onChangeChain row, Down (txAddressValue row))
    onChangeChain row = maybe False (isIntPath . pathInAccount . (.heldPath)) (txAddressHeld row)
    ofViewed row = maybe False (isNothing . (.heldIn)) (txAddressHeld row)

-- | Whom a transaction pays: "at bip44\/0\/3 \"Rent\"", "to bip84\/0\/5 of
-- portfolio1", "to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 2 more". An
-- address of the wallet is named by its path, as the paths of the view
-- are written ('heldPathWord'), then by what the function given says of
-- it: @purposeCell@ gives the label of the transaction there, or else
-- that of the address, or else the mark of a change address. The address
-- of another wallet is in full. One of the address book is underlined,
-- then what the second function says of its contact: "to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\"".
payeeWordsNoted :: PathNaming -> Network -> (HeldAddress -> [Doc Style]) -> (Contact -> [Doc Style]) -> Payee -> Doc Style
payeeWordsNoted paths net noteOf contactNoteOf (Payee preposition row others) =
  hsep $ [text (cs preposition), target] <> note <> more
  where
    target = case (txAddressHeld row, txAddressContact row) of
      (Just held, _) -> heldPathWord paths held
      (Nothing, Just _) -> bookAddressWord net (txAddress row)
      (Nothing, Nothing) -> text (addrToString net (txAddress row))
    -- A path is short: its label stays on its line. So does the label of
    -- a contact, which ends the line.
    note = case (txAddressHeld row, txAddressContact row) of
      (Just held, _) -> noteOf held
      (Nothing, Just contact) -> contactNoteOf contact
      (Nothing, Nothing) -> []
    more = if others > 0 then ["and", text (show others), "more"] else []

-- | The short id to name a transaction by, the first 8 digits of its hash
-- ("Haskoin.Wallet.ShortId"). For one on the chain, that of its txid,
-- faint, since the user rarely types it. For one in preparation, that of
-- its nosigHash until it is signed, then that of its txid, as the
-- commands take it.
hashWord :: TxStatus -> Doc Style
hashWord (OnChain status) = styled Muted (shortHashText (chainTxid status))
hashWord (Pending status) = styled PendingHash (shortHashText (pendingShownHash status))

{- The table of txs -}

-- The last column says what a transaction is for, as a bank statement
-- does. An address of the wallet is named by its path, which is short,
-- then the label of the transaction, or else that of the address. The
-- address of another wallet gives way to the label of the transaction,
-- which says more to the user. Nothing in the cell is cut.
--
-- The first column gives the short id of the transaction, the first 8
-- digits of its txid, which the commands take ("Haskoin.Wallet.ShortId"),
-- as the first line of the compact view starts with it. The 64 digits of
-- the txid would make a row too wide for most screens. The verbose view
-- gives them.

-- | The columns of the table of txs, that of the tags as wide as given
txColumns :: AmountUnit -> Natural -> [Column]
txColumns unit tagsWide =
  [ column "Txid" shortIdWidth LeftSide,
    column "Time" minuteWidth LeftSide,
    column "Type" typeWidth LeftSide,
    amountColumn "Amount" unit,
    amountColumn "Balance" unit,
    -- Its cells take four characters at most, "999+" and "dead": the
    -- heading sets its width
    column "Confs" 4 RightSide,
    tagsColumn tagsWide,
    column "Description" 0 LeftSide
  ]

-- | The confirmations of a transaction, as a number alone in its column of
-- the table, up to "999+". An unconfirmed transaction has 0, in the
-- colour of what waits. It gives no fee rate, which would not fit. The
-- compact view gives it. A dead transaction says it is dead: it will
-- never confirm, and a count would hide that. txs lists no transaction in
-- preparation, but one would say how far it got, in the words of the
-- compact view, as short as they go.
tableConfirmations :: TxStatus -> Doc Style
tableConfirmations = \case
  OnChain status
    | chainDead status -> styled Failure "dead"
    | otherwise -> confsCell (chainConfirmations status)
  Pending status -> pendingStatusWord status

-- | How far a transaction in preparation got, in the words of the compact
-- view, as short as they go ('pendingStatusText'), in the colour of where
-- it stands: red in conflict, green once sent, yellow before
pendingStatusWord :: PendingStatus -> Doc Style
pendingStatusWord status = styled style (pendingStatusText status)
  where
    style
      | pendingConflicted status = Failure
      | pendingOnline status = Good
      | otherwise = Waiting

-- | A transaction on one row of the table of txs, under the columns
-- given ('txColumns'): its short id, its time,
-- its type, its amount, the balance after it and its confirmations, each
-- in its column, then what it is for (@purposeCell@). The amount has its sign and its
-- colour, as on the first line of the compact view. A dead transaction leaves its balance empty, since
-- the balance does not count it. Its description says first which
-- transactions replaced it, if any (@replacedByNote@), as the compact
-- view does. The table lists the active portfolio,
-- so no row names its portfolio, and its paths are written as given
-- ('activePaths'): alone, but for an address of another portfolio.
txRowDoc :: PathNaming -> Network -> AmountUnit -> [Column] -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
txRowDoc paths net unit headed balanceM time info@(TxInfo summary status _ _ _ _ _) =
  tableLine (zip headed cells)
  where
    cells =
      [ hashWord status,
        timeCell time,
        text (typeWord (summaryType summary)),
        signedAmountCell unit (summaryAmount summary),
        maybe mempty (amountCell Nothing "" unit) balanceM,
        tableConfirmations status,
        tagsCell (txOwnTags (txInfoTags info)),
        descriptionCell
      ]
    -- The words of the compact view, all in one cell, a comma between
    -- them
    descriptionCell = hcat . intersperse ", " $ maybeToList (replacedByNote status) <> purposeWords paths net info

-- | What a transaction is for, as the last cell of the table of txs and
-- the end of the compact view say it. A payment received, or one between the addresses of the wallet, gives the path of
-- the address it pays the most, then the label of the transaction, or
-- else the label of that address, or else that it is a change address:
-- "at bip44\/0\/3 \"Salary\"". A payment to another portfolio of the
-- database reads the same way, its path naming that portfolio: "to
-- bip84\/0\/5 of portfolio1 \"Savings\"". The path says the payment went
-- to the user, which the label alone does not. Any other payment gives its
-- label alone, or else the address of the other wallet it pays the most:
-- "to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU". Each tells how many other
-- addresses it pays. A label and an address are whole, however long.
--
-- In a listing that keeps the transactions at tagged addresses
-- (--address-tag), the address named by its path is followed by those of
-- its tags the listing keeps ('keptTagsAt'). @otherKeptAddresses@ gives
-- the other addresses of the transaction that have such a tag.
--
-- An address of the address book is named as an address of the wallet
-- is: always, with a label or without, since that it is a known address
-- is what the user wants to see. It is underlined, and followed by the
-- label of the transaction, or else by that of its contact
-- ('contactNote').
purposeCell :: PathNaming -> Network -> TxInfo -> Doc Style
purposeCell paths net info = case mainPayee info of
  Just payee
    | namesPayee payee -> payeeWordsNoted paths net (heldNote (txAddress (payeeRow payee))) bookNote payee
  _ -> if Text.null label then mempty else labelWord label
  where
    label = txInfoLabel info
    namesPayee payee =
      isJust (txAddressHeld (payeeRow payee))
        || isJust (txAddressContact (payeeRow payee))
        || Text.null label
    -- The label of the transaction says what it is for better than the
    -- label of the contact it pays. The tags of the contact that the
    -- listing keeps follow, as those of an address of the wallet do.
    bookNote contact =
      (if Text.null label then contactNote contact else [labelWord label])
        <> tagMarks (keptContactTags info contact)
    -- The label of the transaction says what it is for better than the
    -- label of its address
    heldNote addr held =
      ( if Text.null label
          then ownAddressNote held.heldLabel (isIntPath (pathInAccount held.heldPath))
          else [labelWord label]
      )
        <> tagMarks (keptTagsAt info addr)

-- | What @purposeCell@ says of a transaction, as one of the parts a
-- short view puts a comma between. None for a transaction that pays no
-- address and has no label, so that no comma stands alone.
purposePart :: PathNaming -> Network -> TxInfo -> [Doc Style]
purposePart paths net info = [purpose | not (null (renderPlain purpose))]
  where
    purpose = purposeCell paths net info

-- | The addresses that put a transaction in a listing that keeps the
-- transactions at tagged addresses (--address-tag), but for the one
-- @purposeCell@ names, whose tags are beside it there. So the reader
-- sees, for each transaction of such a listing, which address put it
-- there. Outside such a listing there are none.
--
-- First come the addresses of the portfolio that have a tag asked for,
-- in the order of their paths. Each is named by its path, with those
-- tags: "bip84\/1\/2 [kyc]".
--
-- Then comes each contact of the address book that has such a tag, by
-- its address, underlined, with its label and those tags:
-- "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" [rent]". A payment
-- received names the address it was paid at, so the contact that paid it
-- is seen only here. The label is given, unlike that of an address of
-- the portfolio: a path says which address it is, and the address of
-- somebody else says nothing without its name.
--
-- The compact view gives each of them a line of its own
-- ('compactTxDoc'). The tables keep them in their last cell
-- (@purposeWords@).
otherKeptAddresses :: PathNaming -> Network -> TxInfo -> [Doc Style]
otherKeptAddresses paths net info =
  [ heldPathWord paths (HeldAddress path "" Nothing) <+> hsep (tagMarks kept)
    | (addr, path) <- ownAddresses,
      Just addr /= named,
      let kept = keptTagsAt info addr,
      not (null kept)
  ]
    <> [ hsep (bookAddressWord net addr : contactNote contact <> tagMarks kept)
         | (addr, contact) <- Map.toList (txInfoContacts info),
           Just addr /= namedContact,
           -- An address another portfolio holds is of the wallet
           not (Map.member addr (txInfoElsewhere info)),
           let kept = keptContactTags info contact,
           not (null kept)
       ]
  where
    -- The contact @purposeCell@ names by its address
    namedContact = case mainPayee info of
      Just payee
        | isNothing (txAddressHeld (payeeRow payee)), isJust (txAddressContact (payeeRow payee)) -> Just (txAddress (payeeRow payee))
      _ -> Nothing
    summary = txInfoSummary info
    -- The addresses of the portfolio in the transaction, each once, in
    -- the order of their paths
    ownAddresses =
      sortOn snd . Map.toList $
        Map.map myOutputsPath (summaryMyOutputs summary) <> Map.map myInputsPath (summaryMyInputs summary)
    -- The address of the portfolio @purposeCell@ names by its path
    named = case mainPayee info of
      Just payee
        | Just held <- txAddressHeld (payeeRow payee), isNothing held.heldIn -> Just (txAddress (payeeRow payee))
      _ -> Nothing

-- | What the last cell of a table says a transaction is for, as the
-- parts it puts a comma between: what @purposeCell@ says, then each
-- other address that has a tag the listing was asked to keep
-- (@otherKeptAddresses@). A row of a table is one line, so these
-- addresses stay in the cell, where the compact view gives each a line
-- ('compactTxDoc'). The cell is the last one, and a wide row scrolls
-- sideways.
purposeWords :: PathNaming -> Network -> TxInfo -> [Doc Style]
purposeWords paths net info = purposePart paths net info <> otherKeptAddresses paths net info

{- The table of pending transactions -}

-- The table of pending transactions gives what the compact view gives:
-- the short id of the hash the other commands take first, where the
-- transaction is, the time it was created, its type, its amount, how far
-- it got, its fee rate, and what it is for, as the table of txs says it.
-- A replacement says first, in yellow, which transactions it replaces, as
-- the compact view does. Where it is takes a column for the wallet when
-- the database has several, then one for the portfolio and one for the
-- network. The listing is read whole ('listingWhole'), so these columns
-- are as wide as the longest name they show, as in the table of
-- portfolios. The hash in full would make a row too wide. The verbose
-- view gives it.

-- | The columns of the table of pending transactions: the short id first,
-- then the wallet when the listing names the wallets, the portfolio and
-- the network, each as wide as the widest the places given hold. The
-- wallet has a column of its own, as in the table of portfolios, so that
-- the eye runs down the wallets apart from the portfolios. Each amount
-- has the unit of its network after it ('amountColumn').
pendingTableColumns :: UnitChoice -> WalletNaming -> Natural -> [Place] -> [Column]
pendingTableColumns choice naming tagsWide places =
  [column "Hash" shortIdWidth LeftSide]
    <> [column "Wallet" (widestCell (cs . walletName . refWallet . placePortfolio) places) LeftSide | naming == WithWallets]
    <> [ column "Portfolio" (widestCell (cs . portfolioName . refPortfolio . placePortfolio) places) LeftSide,
         column "Network" (widestCell (\place -> (placeNetwork place).name) places) LeftSide
       ]
    <> [ column "Time" minuteWidth LeftSide,
         column "Type" typeWidth LeftSide,
         amountColumn "Amount" (chosenUnit choice),
         column "Status" pendingStatusWidth LeftSide,
         -- The fee rate has a few digits: the heading sets the width
         column "sat/vB" 1 RightSide,
         tagsColumn tagsWide,
         column "Description" 0 LeftSide
       ]

-- | The width of the column of the statuses of pending transactions: that
-- of "not signed" and "conflicted", the longest
pendingStatusWidth :: Natural
pendingStatusWidth = 10

-- | A pending transaction on one row of its table, under the columns given:
-- its short id, its wallet when the listing names the wallets, its
-- portfolio and its network, the time it was created, its type, its
-- amount, how far it got, its fee rate, and what it is for
-- (@purposeCell@), after the transactions it replaces, if any
-- (@replacesNote@). One in conflict can no longer be sent: its fee rate no
-- longer matters, and its cell is empty. The paths are written as given
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Place.listedPaths'): those of a
-- transaction of another portfolio than the active one name their
-- portfolio.
pendingRowDoc :: UnitChoice -> WalletNaming -> PathNaming -> [Column] -> Place -> Maybe ZonedTime -> TxInfo -> Doc Style
pendingRowDoc choice naming paths headed place time info@(TxInfo summary status _ _ _ _ _) =
  tableLine . zip headed $
    [hashWord status]
      <> [pretty (walletName (refWallet ref)) | naming == WithWallets]
      <> [ pretty (portfolioName (refPortfolio ref)) `withStyle` Account,
           text net.name
         ]
      <> [ timeCell time,
           text (typeWord (summaryType summary)),
           signedAmountCell (unitOn net choice) (summaryAmount summary),
           statusCell,
           rateCell,
           tagsCell (txOwnTags (txInfoTags info)),
           descriptionCell
         ]
  where
    net = placeNetwork place
    ref = placePortfolio place
    -- pendingtxs lists the transactions in preparation only
    (statusCell, rateCell) = case status of
      Pending pending
        | pendingConflicted pending -> (pendingStatusWord pending, mempty)
        | otherwise -> (pendingStatusWord pending, text (paidRateText (summaryFeeRate summary)))
      OnChain _ -> (tableConfirmations status, mempty)
    -- The words of the compact view, all in one cell, a comma between
    -- them
    descriptionCell = hcat . intersperse ", " $ maybeToList (replacesNote status) <> purposeWords paths net info

{- The verbose view -}

-- | A transaction with every detail: its first line, then its fields. The
-- lines given come first: its account in a listing of every account, or its
-- network in an answer. Its label, if any, follows them. The balance after
-- it, if the listing gives one, follows its fee. Every address it spends
-- from or pays is on a line of its own, in full, with its value, and those
-- of the wallet with their paths, in columns. Below an address of the
-- wallet, its label, and for one it spends from, the labels its coins show,
-- if any (@addressRow@). A change address is marked as one, except under
-- Change, which says it. A path is written as the paths of the view are
-- (the type t'PathNaming'), and the path of an address of another portfolio
-- names that portfolio.
verboseTxDoc :: PathNaming -> Network -> AmountUnit -> [Doc Style] -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
verboseTxDoc paths net unit named balanceM time info@(TxInfo summary status label _ _ _ _) =
  vcat $
    headlineDoc fullConfirmations unit time info
      : ( indent 2
            <$> named
              <> [field 9 "Label" <> labelWord label | not (Text.null label)]
              <> [field 9 "Tags" <> tagsCell tags | let tags = txOwnTags (txInfoTags info), not (null tags)]
              <> statusFields time summary status
              <> spendsField unit info
              <> [feeField unit summary]
              <> [field 9 "Balance" <> neutralAmount unit balance | Just balance <- [balanceM]]
              <> lockTimeField summary
              <> section "From" Nothing True from
              <> section "To" (Just Incoming) True to
              <> section "Change" (Just Incoming) False change
        )
  where
    from = spentFrom info
    (to, change) = paidTo info
    widths = addressColumns paths net unit (from <> to <> change)
    -- What the portfolio receives is green. What it spends, and what
    -- others receive, another portfolio among them, is neither in nor out
    -- of it.
    section name ownStyle marked rows =
      [field 9 name <> align (vcat (addressRow paths net unit widths ownStyle marked <$> rows)) | not (null rows)]

-- | The fields that say where a transaction stands: the hash to name it
-- by, in full and by its short id, its time to the second with its time
-- zone, and what its first line does not explain. The summary tells what
-- became of a dead transaction (@deadStatus@).
statusFields :: Maybe ZonedTime -> TxSummary -> TxStatus -> [Doc Style]
statusFields time summary (OnChain status) =
  hashFields Nothing "Txid" (chainTxid status)
    <> [field 9 "Time" <> text (secondText t) | Just t <- [time]]
    <> [field 9 "Status" <> deadStatus summary status | chainDead status]
-- A transaction in preparation shows one hash, the one to name it by: its
-- nosigHash until it is signed, then its id, which the network will know.
statusFields time _ (Pending status) =
  hashFields (Just PendingHash) (if pendingSigned status then "Txid" else "NoSigHash") (pendingShownHash status)
    -- Yellow, as in the compact view (@replacesNote@)
    <> [ field 9 "Replaces" <> align (vcat [text (cs $ txHashToHex replaced) `withStyle` Waiting | replaced <- pendingReplaces status])
       | not (null (pendingReplaces status))
       ]
    <> [field 9 "Created" <> text (secondText t) | Just t <- [time]]
    <> [ field 9 "Conflict"
           <> styled Failure "A coin it spends was spent by another transaction, or is gone."
           <+> "It can no longer be sent. Delete it with"
           <+> commandWord "deletetx"
           <> "."
       | pendingConflicted status
       ]

-- | What became of a dead transaction, as its Status field says it.
--
-- One that live transactions replaced names them, each in full on a line
-- of its own under the words "replaced by" ('chainReplacedBy'). The
-- verbose view gives every id in full, and one line for the words and an
-- id would pass 80 columns.
--
-- One that names none was dropped from the network, when the portfolio
-- made it ('ownTransaction'): only a transaction that spends one of its
-- coins could replace it, and the portfolio would hold that transaction.
-- Any other, a payment received for instance, was replaced or dropped,
-- and the portfolio cannot tell which. Its payer can replace it with a
-- transaction that pays the portfolio nothing, which the portfolio never
-- sees.
deadStatus :: TxSummary -> ChainStatus -> Doc Style
deadStatus summary status
  | not (null replacements) = align (vcat ("replaced by" : [text (cs (txHashToHex replacement)) | replacement <- replacements]))
  | ownTransaction summary = "dropped from the network"
  | otherwise = "replaced, or dropped from the network"
  where
    replacements = chainReplacedBy status

-- | A hash under its name, in full, then its short id on a field of its
-- own, as the verbose views give both: the full hash for a block explorer
-- or another wallet, the short id for a command. Both are in the style
-- given, if any. On one line, the two would reach 86 columns.
hashFields :: Maybe Style -> Text -> TxHash -> [Doc Style]
hashFields styleM name hash =
  [ field 9 name <> shown (cs (txHashToHex hash)),
    field 9 "Short id" <> shown (shortHashText hash)
  ]
  where
    shown = maybe text styled styleM

-- | What a transaction of the portfolio that is in preparation spends of
-- coins that are not the portfolio's ('spentOfOthers'), if anything: all
-- that a sweep into the wallet spends. The first line gives what the
-- portfolio gains or loses, which says nothing of those coins, and they
-- may be the user's too, with keys kept elsewhere. The user who signs
-- must see the total, to check it against the addresses paid.
--
-- An external transaction has no such field: its first line gives that
-- total already ('headlineDoc'). Nor has one on the chain, where the coins
-- of others are those of the payer ('txInPreparation').
spendsField :: AmountUnit -> TxInfo -> [Doc Style]
spendsField unit info =
  [ field 9 "Spends" <> neutralAmount unit (toInteger spent) <+> "of coins that are not this portfolio's"
  | txInPreparation info,
    summaryType summary /= TxExternal,
    spent > 0
  ]
  where
    summary = txInfoSummary info
    spent = spentOfOthers summary

-- | The fee and its rate. Below them, the change the fee took in, too
-- small for an output: that change comes neither in nor out.
feeField :: AmountUnit -> TxSummary -> Doc Style
feeField unit summary =
  field 9 "Fee"
    <> align
      ( vcat $
          [neutralAmount unit (fromIntegral (summaryFee summary)) <> "," <+> paidRateDoc (summaryFeeRate summary)]
            <> [ "includes" <+> neutralAmount unit (fromIntegral change) <+> "of change too small for an output"
               | Just change <- [summaryChangeInFee summary],
                 change > 0
               ]
      )

-- | The lock time of a transaction that has one: the block it waits for,
-- which hw sets near the best block (see "Haskoin.Wallet.Tx.LockTime"). A
-- lock time of 500,000,000 or more is a Unix time instead, which hw never
-- writes.
lockTimeField :: TxSummary -> [Doc Style]
lockTimeField summary =
  [field 9 "Lock time" <> text (kind <> show lockTime) | lockTime /= 0]
  where
    lockTime = summaryLockTime summary
    kind = if lockTime < 500000000 then "block " else "Unix time "

-- | The widths of the columns of the addresses of a transaction, each that
-- of its widest cell
data AddressColumns = AddressColumns
  { addressColumn :: !Int,
    valueColumn :: !Int,
    pathColumn :: !Int
  }

-- | The widths of the columns for the addresses given, all those of one
-- transaction. The path of an address is measured as the view writes it,
-- with its portfolio when it names one ('heldPathText').
addressColumns :: PathNaming -> Network -> AmountUnit -> [TxAddress] -> AddressColumns
addressColumns paths net unit rows =
  AddressColumns
    { addressColumn = widest [addrToString net (txAddress row) | row <- rows],
      valueColumn = widest [valueText unit v | Just v <- txAddressValue <$> rows],
      pathColumn = widest [heldPathText paths held | Just held <- txAddressHeld <$> rows]
    }
  where
    widest = maximum . (0 :) . map shownWidth

-- | An address of a transaction: the address, its value right-aligned,
-- then for an address of the wallet, its path, as the paths of the view
-- are written ('heldPathText'). Below them, on a line that
-- starts under the address, an address of the wallet has its label, or
-- that it is a change address when marked, then the labels its coins
-- show, after the word "from". The line below keeps the columns of
-- addresses lined up, however long the labels. An address of the wallet
-- and its label are in its colour, of whichever portfolio it is. Its
-- value is in the style given when it is of the portfolio of the view.
-- The value of an address of another portfolio has no style, as that of
-- an address of another wallet: it is neither in nor out of the portfolio
-- the amount of the view is counted for. An address of the address book
-- is underlined, with the label and the tags of its contact on the line
-- below ('contactNoteBelow').
addressRow :: PathNaming -> Network -> AmountUnit -> AddressColumns -> Maybe Style -> Bool -> TxAddress -> Doc Style
addressRow paths net unit widths ownStyle marked row =
  withNotesBelow (cellsLine firstLine) notes
  where
    firstLine =
      [(addressColumn widths, address, addressDoc)]
        <> [(valueColumn widths, padding value <> value, valueDoc value) | Just v <- [txAddressValue row], let value = valueText unit v]
        <> [(pathColumn widths, path, trailingPathWord path) | Just held <- [txAddressHeld row], let path = heldPathText paths held]
    notes = case (txAddressHeld row, txAddressContact row) of
      (Just held, _) ->
        ownAddressNoteBelow held.heldLabel (marked && isIntPath (pathInAccount held.heldPath))
          -- The tags are one note: a space between them, as on every
          -- other line that shows tags
          <> [hsep (tagMarks tags) | let tags = txAddressShownTags row, not (null tags)]
          <> [styled Muted "from" <+> fillSep (punctuate "," (wrappingLabelWord <$> spentLabels)) | let spentLabels = txAddressCoinLabels row, not (null spentLabels)]
      -- An address of the address book: its label, then its tags
      (Nothing, Just contact) -> contactNoteBelow contact
      (Nothing, Nothing) -> []
    address = addrToString net (txAddress row)
    addressDoc
      | isJust (txAddressHeld row) = styled OwnAddress address
      | isJust (txAddressContact row) = styled BookAddress address
      | otherwise = text address
    style = case txAddressHeld row of
      Just held | isNothing held.heldIn -> ownStyle
      _ -> Nothing
    -- Right-aligned, so that the points line up. The text of the cell
    -- holds the spaces before the value, so that 'cellsLine' adds none
    -- after it, and the paths line up too.
    padding value = replicate (valueColumn widths - length value) ' '
    valueDoc value = pretty (padding value) <> maybe id annotate style (pretty value)

-- | The fee rate a transaction pays, in satoshis per virtual byte, with
-- the decimal of a rate that is not whole ('paidRateText')
paidRateDoc :: PaidRate -> Doc Style
paidRateDoc paid = text (paidRateText paid <> " sat/vB")

{- Transactions held, signed and sent -}

-- | A transaction as each portfolio that holds it sees it, in full, a
-- blank line between the portfolios: what label and show answer. Each
-- view that is not of the active portfolio given says so. Each portfolio
-- is named with its wallet or not, as given, the same in every view: a
-- payment between two wallets shows in a view of each.
heldTxsDoc :: WalletNaming -> UnitChoice -> Maybe PortfolioRef -> [(Place, TxInfo)] -> IO (Doc Style)
heldTxsDoc naming choice activeM held =
  vcat . intersperse mempty <$> mapM viewOf held
  where
    viewOf (place, txInfo) = txAnswerDoc naming (placeNetwork place) (Just (SeenFrom (placePortfolio place) activeM)) choice txInfo

-- | A transaction just signed, as signtx and signsweep answer on the
-- screen: its id, and the file it was written to, if any. The transaction
-- itself was shown before the secret was asked for.
signedDoc :: Maybe FilePath -> TxInfo -> Doc Style
signedDoc fileM info =
  vcat $
    [doneWithTxid "Signed" hash | Just hash <- [txInfoHash info]]
      <> ["Written to" <+> styled UserText file | Just file <- [fileM]]

-- | What send and sendall answer with on the screen: that the transaction
-- was sent, and its txid. The command showed the transaction in full
-- before it asked for the mnemonic, and nothing of it changed since, so
-- showing it again would only push the txid off the screen.
sentDoc :: TxInfo -> Doc Style
sentDoc info = vcat [doneWithTxid "Sent" hash | Just hash <- [txInfoHash info]]

-- | What a command did to a transaction, in a word, then its txid:
-- "Signed, txid 3f2a...", as signtx, send and sendall answer once they
-- showed the transaction in full
doneWithTxid :: String -> TxHash -> Doc Style
doneWithTxid done hash = styled Good done <> "," <+> "txid" <+> styled PendingHash (cs (txHashToHex hash))
