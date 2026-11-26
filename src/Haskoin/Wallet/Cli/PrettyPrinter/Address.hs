{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about addresses: an address and a balance, each in its
-- compact view, its verbose view and its table, what receive says of a new
-- address, and what show derives at a path.
module Haskoin.Wallet.Cli.PrettyPrinter.Address
  ( listedAddressDoc,
    compactAddressDoc,
    verboseAddressDoc,
    addressAnswerDoc,
    withCodeDoc,
    derivedAddrDoc,
    listedBalanceDoc,
    coinBalanceDoc,
    verboseBalanceDoc,
    addressTableColumns,
    addressRowDoc,
    balanceTableColumns,
    balanceRowDoc,
  )
where

import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.Command (AddressCode (..))
import Haskoin.Wallet.Cli.PrettyPrinter.AddressWords (changeWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Balance (balanceFigures, heldAmountCell, notSyncedWords, totalFigure, unspendableFigure, waitingAmount, waitingAmountCell)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (amountPrinter, amountWidth, block, columns, commaList, counted, countPrinter, field, labelWord, secondText, sentence, styled, tagMarks, text, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (commandWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (PathNaming (..), SeenFrom (..), accountRefField, ownPathsAlone, ownSuffix, pathsSeenFrom)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (Column (..), Side (..), addressWidth, amountHeading, column, tableLine, tagsCell, tagsColumn)
import Haskoin.Wallet.Cli.QrCode (paymentUri, qrRows)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address (AddressFunds (..), CoinBalance (..), addrPath, addrPoolPath, coinBalanceNotSpendableYet, coinBalanceUnspendable, pathText)
import Haskoin.Wallet.Database.PendingTx (Retag (..), intercalateAnd)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), poolPathText)
import Haskoin.Wallet.ShortId (shortHashText)
import Haskoin.Wallet.Tags (ShownTag (..), tagText)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, annotate, hsep, indent, pretty, vcat, (<+>))
import System.IO

{- Addresses -}

-- | An address of the network as addresses lists it, in the view asked
-- for, the time it was created in the time zone of the computer. Its path
-- is padded to the given width, that of the longest path of its section
-- (@longestPathWidth@ of "Haskoin.Wallet.Cli.PrettyPrinter.Listing").
listedAddressDoc :: View -> Network -> AmountUnit -> Natural -> DBAddress -> AddressFunds -> [ShownTag] -> IO (Doc Style)
listedAddressDoc view net unit pad addr funds tags = do
  created <- utcToLocalZonedTime (dBAddressCreated addr)
  return $ case view of
    Compact -> compactAddressDoc net unit pad addr funds tags
    -- A listing is of the active portfolio
    Verbose -> verboseAddressDoc ownPathsAlone net unit pad created addr funds tags
    -- The table needs the account for the width of its addresses:
    -- rowsPrinter lays it out ('addressRowDoc')
    Table -> compactAddressDoc net unit pad addr funds tags

-- | The first line of an address of the network: its type and its
-- derivation path (@rowPathText@), which tell which address of the
-- portfolio it is, receiving or change, padded to the width given, then
-- the address and its label. The path names its portfolio when the paths
-- of the view do (the type t'PathNaming'): in the answer about an address of
-- another portfolio than the active one. In a listing of addresses of
-- both chains, a change address says so in place of a label, which it
-- never has. The tags of the address end the line, each between
-- brackets ('tagMarks'), in the compact view and in the verbose one
-- alike: they are few and short, and say which groups the address is in
-- as the label says what it is for.
addressLine :: PathNaming -> Network -> Bool -> Natural -> DBAddress -> [ShownTag] -> Doc Style
addressLine paths net mixed pad addr tags =
  columns $
    [text (block pad (cs (rowPathText net addr) <> ownSuffix paths)), pretty (dBAddressAddress addr)]
      <> [labelWord label | let label = dBAddressLabel addr, not (Text.null label)]
      <> [changeWord | mixed, dBAddressInternal addr]
      <> [hsep (tagMarks tags) | not (null tags)]

-- | The path of an address of the wallet on the network, as every view
-- writes it: the type of its account in front, as in bip84\/0\/3
-- ('poolPathText'). A portfolio has an address at \/0\/3 in each of its
-- accounts on a network, so the path alone would name two. The type
-- costs five columns, and saves the user from reading the address to
-- tell them apart. The path is alone for an address that does not read on
-- the network, which hw never stores.
rowPathText :: Network -> DBAddress -> Text
rowPathText net addr = maybe (pathText (addrPath addr)) poolPathText (addrPoolPath net addr)

-- | An address in two lines: its first line, then what it holds, what
-- it received and how many transactions it has, or that it is unused.
-- addresses lists one kind of address at a time, so none is marked.
--
-- An address of a pool that never synced is neither: hw asked no server
-- about it, so its second line says that it is not synced
-- ('notSyncedWords'), and not that it is unused, which is a statement
-- about the chain (the type t'AddressFunds').
--
-- What it holds is the total of its coins, then what it cannot spend of
-- them now, when there is any (the type t'CoinTotals'). The line started
-- with the spendable amount alone: an address whose coin was locked, or
-- waited for a confirmation, read \"spendable 0\" as an address that a
-- payment had emptied. The spendable amount is not written out, as the
-- line of a portfolio writes it
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Balance.shortBalanceFigures'): it
-- is the total on nearly every address, and the three amounts with what
-- the address received would make a line of 117 columns. It is the total
-- without the unspendable amount, and balances gives the three.
--
-- The count is of every transaction of the address, those that spent from
-- it as well as those that paid it: the store gives no other. So it
-- stands apart, after a comma. It followed the amount received as "in 2
-- transactions", which said of an address paid once, then emptied, that
-- two payments had made up its amount.
compactAddressDoc :: Network -> AmountUnit -> Natural -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
compactAddressDoc = compactAddressDocOf ownPathsAlone

-- | 'compactAddressDoc', with the path of the address written as the
-- paths of the view are: an answer may be about another portfolio than
-- the active one, and its paths then name theirs
compactAddressDocOf :: PathNaming -> Network -> AmountUnit -> Natural -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
compactAddressDocOf paths net unit pad addr funds tags =
  vcat [addressLine paths net False pad addr tags, indent 2 activity]
  where
    txs = fromIntegral (dBAddressBalanceTxs addr)
    activity = case funds of
      NotSynced -> notSyncedWords
      Holds totals
        | txs == 0 -> styled Muted "unused"
        | otherwise ->
            commaList $
              [totalFigure unit totals.coinsTotal]
                <> [unspendableFigure unit rest | let rest = coinsUnspendable totals, rest > 0]
                <> [ "received" <+> amountPrinter unit (dBAddressBalanceReceived addr),
                     text (counted txs "transaction" "transactions")
                   ]

-- | An address with every detail: its first line, its path written as the
-- paths of the view are, then its fields. What it holds comes in the
-- two splits of the verbose view of a portfolio
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Portfolio.verbosePortfolioDoc'),
-- in the same order: the total of its coins with what it can spend of
-- them and the rest, then its confirmed and unconfirmed balances.
--
-- An address of a pool that never synced has one field in place of
-- these eight, which says that it is not synced ('notSyncedWords'):
-- eight zeros would say that nobody paid it, and hw never asked (the
-- type t'AddressFunds').
verboseAddressDoc :: PathNaming -> Network -> AmountUnit -> Natural -> ZonedTime -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
verboseAddressDoc paths net unit pad created addr funds tags =
  vcat (addressLine paths net False pad addr tags : (indent 2 <$> (held <> [field 12 "Created" <> text (secondText created)])))
  where
    held = case funds of
      NotSynced -> [field 12 "Balance" <> notSyncedWords]
      Holds totals ->
        [ field 12 "Transactions" <> countPrinter (fromIntegral (dBAddressBalanceTxs addr)),
          field 12 "Received" <> amountPrinter unit (dBAddressBalanceReceived addr),
          field 12 "Total" <> amountPrinter unit totals.coinsTotal,
          field 12 "Spendable" <> amountPrinter unit totals.coinsSpendable,
          field 12 "Unspendable" <> waitingAmount unit (coinsUnspendable totals),
          field 12 "Confirmed" <> amountPrinter unit (dBAddressBalanceConfirmed addr),
          field 12 "Unconfirmed" <> waitingAmount unit (dBAddressBalanceUnconfirmed addr),
          field 12 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr))
        ]

-- | An address as an answer shows it, with where it is, and with its QR code
-- when the command gives the address out and someone can scan the screen.
-- The active portfolio given, if any, is named when the address is of
-- another one ('Haskoin.Wallet.Cli.PrettyPrinter.Place.seenPortfolioDoc'),
-- and the path of the address then names its portfolio (the type
-- t'PathNaming'): label and signmsg would read the path alone in the
-- active portfolio, at another address. Each portfolio is named with its
-- wallet or not, as given.
--
-- With --with-change, tag and untag end the answer with the change
-- addresses that followed the address, after an empty line
-- (@changeFollowedDoc@).
addressAnswerDoc :: WalletNaming -> AddressCode -> UnitChoice -> Maybe PortfolioRef -> AccountRef -> DBAddress -> AddressFunds -> [ShownTag] -> Maybe ChangeFollowed -> IO (Doc Style)
addressAnswerDoc naming code choice activeM ref addr funds tags followedM = do
  created <- utcToLocalZonedTime (dBAddressCreated addr)
  let net = accountRefNetwork ref
      unit = unitOn net choice
      paths = pathsSeenFrom naming (SeenFrom (placePortfolio (accountRefPlace ref)) activeM)
      -- One address: its path needs no padding
      details = verboseAddressDoc paths net unit 0 created addr funds tags
  terminal <- hIsTerminalDevice stdout
  let rows
        | code == WithCode && terminal = fromMaybe [] (qrRows (paymentUri net (dBAddressAddress addr)))
        | otherwise = []
      answered = withCodeDoc (vcat [details, indent 2 (accountRefField naming 12 activeM ref)]) rows
  return (vcat (answered : concat [[mempty, changeFollowedDoc paths net unit followed] | Just followed <- [followedM]]))

-- | The change addresses that followed the address tag or untag was
-- given with --with-change: a line that says what was done to them and
-- why, then each as addresses lists it in its compact view, with its
-- tags as they are now and what it holds. The command changed
-- addresses the user did not name, so the answer names every one of
-- them. What each holds shows where the coins of the group are now:
-- the address tagged is most often empty, and the end of its chain holds
-- them. When none followed, one line says so: the user asked for them.
--
-- The change addresses where the tags stopped come after them, apart
-- (@changeStopsDoc@).
changeFollowedDoc :: PathNaming -> Network -> AmountUnit -> ChangeFollowed -> Doc Style
changeFollowedDoc paths net unit followed =
  vcat . intersperse mempty $
    vcat (said : [compactAddressDocOf paths net unit pad addr funds tags | (addr, funds, tags) <- followed.followedRows])
      : [changeStopsDoc paths net unit followed.followedStops | not (null followed.followedStops)]
  where
    pad = pathPad paths net [addr | (addr, _, _) <- followed.followedRows]
    count = counted (fromIntegral (length followed.followedRows)) "change address" "change addresses"
    said = case (followed.followedRetag, null followed.followedRows) of
      (AddTags, False) -> "Tagged with it, down the chain of its payments:" <+> text count
      (AddTags, True) -> "No change address down the chain of its payments was tagged with it"
      (RemoveTags, False) -> "Untagged with it, down the chain of its payments:" <+> text count
      (RemoveTags, True) -> "No change address down the chain of its payments was untagged with it"

-- | The width that lines up the paths of the addresses, as in a listing
pathPad :: PathNaming -> Network -> [DBAddress] -> Natural
pathPad paths net addrs = maximum (0 : [fromIntegral (Text.length (rowPathText net addr) + length (ownSuffix paths)) | addr <- addrs])

-- | The change addresses where the tags of tag --with-change stopped:
-- a line that counts them, then each as @changeFollowedDoc@ lists an
-- address, with why it took no tag below it. The reason names the
-- payment, by the short id every command takes, and the address of the
-- coin from outside the group, by its path: tagging that address is what
-- lets the tags go further, and hw does not decide it for the user.
changeStopsDoc :: PathNaming -> Network -> AmountUnit -> [ChangeStop] -> Doc Style
changeStopsDoc paths net unit stops =
  vcat (text ("Not tagged, where the chain stops: " <> count) : concatMap stopDoc stops)
  where
    pad = pathPad paths net [addr | ChangeStop {changeStopAt = (addr, _, _)} <- stops]
    count = counted (fromIntegral (length stops)) "change address" "change addresses"
    stopDoc stop =
      let (addr, funds, tags) = stop.changeStopAt
       in [compactAddressDocOf paths net unit pad addr funds tags, indent 2 (sentence (wordsOf (why stop)))]
    why stop =
      ("The payment " <> shortHashText stop.changeStopPayment <> " also spends a coin ")
        <> case stop.changeStopSource of
          Just (source, _, _) -> "at " <> cs (rowPathText net source) <> ownSuffix paths <> ", without " <> lacked stop
          Nothing -> "that is not of the portfolio, and has no tag"
    lacked stop = case cs . tagText . shownTag <$> stop.changeStopTags of
      [tag] -> "the tag " <> tag
      tags -> "the tags " <> intercalateAnd tags

-- | Text, then a QR code drawn in the rows given. The code goes under the
-- text and never beside it: a terminal wraps its long lines again when it
-- is resized, which would mix the text into the code.
withCodeDoc :: Doc Style -> [Text] -> Doc Style
withCodeDoc details rows = vcat (details : (annotate QrCode . pretty <$> rows))

-- | The address at a path of the account that show derives, when the
-- wallet does not hold it: its type and its path, and the address cut, as
-- the first line of addresses, then its account, then what the cut form
-- is for. The wallet holds no row for it, so it has no label and no
-- balance. The user compares its ends with the address the online
-- computer showed, and the note says why the middle is missing, which a
-- user would otherwise take for a fault ("Haskoin.Wallet.CutAddress").
derivedAddrDoc :: WalletNaming -> AccountRef -> SoftPath -> Text -> Doc Style
derivedAddrDoc naming ref path cut =
  vcat
    [ columns [text (cs (poolPathText typed)), pretty cut],
      -- show derives a path in the active portfolio, so there is no other
      -- one to name
      indent 2 (accountRefField naming 9 Nothing ref),
      mempty,
      cutAddressNote typed
    ]
  where
    typed = PoolPath (accountRefKind ref) path

-- | How to check an address with its cut form, and why its middle is
-- hidden. The user reads it in the middle of a payment, about to give the
-- address out, so it says what to do in each case and alarms only when
-- the ends differ. Why the address must not be paid from this screen, that
-- it may lie past the gap, is in the help and the README instead. The
-- word receive is in the colour of a command, as in the list of commands
-- ('commandWord'), so that it reads as the name of one.
cutAddressNote :: PoolPath -> Doc Style
cutAddressNote path =
  sentence $
    wordsOf "Compare both ends with the address that"
      <> [commandWord "receive"]
      <> wordsOf ("gave out at " <> cs (poolPathText path) <> " on the online computer.")
      <> wordsOf "If they match, that address is of this account, and you can give it out."
      <> wordsOf "If they differ, the online computer may have been tampered with, and you should not give the address out."
      <> wordsOf "Nobody can make another address with the same ends."
      <> wordsOf "This screen is for checking only, so the middle of the address is hidden."
      <> wordsOf "Give out the address as"
      <> [commandWord "receive"]
      <> wordsOf "showed it."

{- Balances -}

-- | An address as balances lists it, in the view asked for. The table
-- needs the account for the widths of its columns: rowsPrinter lays it
-- out ('balanceRowDoc').
listedBalanceDoc :: View -> Network -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
listedBalanceDoc view net unit pad addr balance tags = case view of
  Verbose -> verboseBalanceDoc net unit pad addr balance tags
  _ -> coinBalanceDoc net unit pad addr balance tags

-- | An address as balances shows it: its first line, with a change
-- address marked as one among the receiving addresses, then the value of
-- its coins, what it can spend now, and the rest, which waits
coinBalanceDoc :: Network -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
coinBalanceDoc net unit pad addr balance tags =
  vcat
    [ addressLine ownPathsAlone net True pad addr tags,
      indent 2 (commaList (balanceFigures unit (coinBalanceTotal balance) (coinBalanceSpendable balance)))
    ]

-- | An address as balances --verbose shows it: its first line, then its
-- fields. The value of its coins and how many there are, then what it can
-- spend now, and what it cannot, split as coins tells the state of each
-- coin: locked by a pending transaction, locked by the user, locked by
-- a tag, or not spendable yet. The compact view gives the sum of the
-- four; this one tells which to wait for, and which to unlock.
--
-- The coins that hold tokens of Bitcoin Cash have a line too, only at an
-- address that has some: no other network has tokens, and few addresses
-- of Bitcoin Cash do, so the line would be a zero nearly everywhere.
verboseBalanceDoc :: Network -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
verboseBalanceDoc net unit pad addr balance tags =
  vcat $
    addressLine ownPathsAlone net True pad addr tags
      : ( indent 2
            <$> [ field 17 "Total" <> amountPrinter unit (coinBalanceTotal balance),
                  field 17 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr)),
                  field 17 "Spendable" <> amountPrinter unit (coinBalanceSpendable balance),
                  field 17 "Locked by tx" <> waitingAmount unit (coinBalanceLockedByTx balance),
                  field 17 "Locked by you" <> waitingAmount unit (coinBalanceLockedByUser balance),
                  field 17 "Locked by tag" <> waitingAmount unit (coinBalanceLockedByTag balance)
                ]
              <> [field 17 "Locked by tokens" <> waitingAmount unit (coinBalanceLockedByTokens balance) | coinBalanceLockedByTokens balance > 0]
              <> [field 17 "Not spendable yet" <> waitingAmount unit (coinBalanceNotSpendableYet balance)]
        )

{- The table of addresses -}

-- The table of addresses gives what the compact view gives: the path, the
-- address, what it can spend, what it received, how many transactions
-- it has, then its label. addresses lists the receiving addresses
-- or the change addresses, never both at once. A change address never
-- has a label, so the table of change addresses has no column for one.

-- | The columns of the table of the addresses of the account, on its
-- receiving chain or on its change chain. The path takes the width given,
-- that of the longest path of the account on the chain
-- (@longestPathWidth@ of "Haskoin.Wallet.Cli.PrettyPrinter.Listing"), the
-- address that of the widest address the account can have
-- ('addressWidth'), and the tags the second width given.
addressTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Natural -> Bool -> [Column]
addressTableColumns unit net kinds pad tagsWide internal =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column (amountHeading "Total" unit) amountWidth RightSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Received" unit) amountWidth RightSide,
    column "Txs" txCountWidth RightSide,
    tagsColumn tagsWide
  ]
    <> [column "Label" 0 LeftSide | not internal]

-- | The width of the column of the numbers of transactions: that of
-- 99 999 transactions. An address used that often is rare: hw gives out a
-- new address for each payment.
txCountWidth :: Natural
txCountWidth = 5

-- | An address on one row of its table, under the columns given: its
-- path, the address, what its coins are worth, what it can spend of them
-- now and what it received, in green unless zero, how many transactions
-- it has, then its label. The total beside the spendable amount tells an
-- address whose coin is locked, or waits, from one that holds nothing.
-- An address no transaction paid leaves its amounts empty, which sets it
-- apart from one that was paid and spent everything.
--
-- An address of a pool that never synced leaves its number of
-- transactions empty too: a zero there would say that nobody paid it,
-- and hw never asked (the type t'AddressFunds'). The column is too
-- narrow for the words of the other views.
addressRowDoc :: AmountUnit -> Network -> [Column] -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
addressRowDoc unit net headed addr funds tags =
  tableLine (zip headed cells)
  where
    cells =
      [text (cs (rowPathText net addr)), pretty (dBAddressAddress addr)]
        <> figures
        <> [tagsCell tags]
        <> [labelCell | not (dBAddressInternal addr)]
    -- The three amounts, then the number of transactions
    figures = case funds of
      NotSynced -> replicate 4 mempty
      Holds totals
        | dBAddressBalanceTxs addr > 0 -> (heldAmountCell unit <$> [totals.coinsTotal, totals.coinsSpendable, dBAddressBalanceReceived addr]) <> [txCount]
        | otherwise -> replicate 3 mempty <> [txCount]
    txCount = text (show (dBAddressBalanceTxs addr))
    labelCell
      | Text.null (dBAddressLabel addr) = mempty
      | otherwise = labelWord (dBAddressLabel addr)

{- The table of balances -}

-- The table of balances gives what the compact view gives: the path, the
-- address, its three amounts, and its label, or that it is a change
-- address. balances lists the addresses of both chains together.

-- | The columns of the table of balances. The path takes the width given,
-- that of the longest path of the account on its two chains, and the
-- address that of the widest address the account can have.
balanceTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Natural -> [Column]
balanceTableColumns unit net kinds pad tagsWide =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column (amountHeading "Total" unit) amountWidth RightSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Unspendable" unit) amountWidth RightSide,
    tagsColumn tagsWide,
    column "Label" 0 LeftSide
  ]

-- | An address on one row of the table of balances, under the columns
-- given: its path, the address, the value of its coins and what it can
-- spend, in green unless zero, the rest in yellow unless zero, then its
-- label, or that it is a change address
balanceRowDoc :: AmountUnit -> Network -> [Column] -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
balanceRowDoc unit net headed addr balance tags =
  tableLine $
    zip
      headed
      [ text (cs (rowPathText net addr)),
        pretty (dBAddressAddress addr),
        heldAmountCell unit (coinBalanceTotal balance),
        heldAmountCell unit (coinBalanceSpendable balance),
        waitingAmountCell unit (coinBalanceUnspendable balance),
        tagsCell tags,
        labelCell
      ]
  where
    labelCell
      | not (Text.null (dBAddressLabel addr)) = labelWord (dBAddressLabel addr)
      | dBAddressInternal addr = changeWord
      | otherwise = mempty
