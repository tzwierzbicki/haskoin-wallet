{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The history of the wallet as a journal of double-entry accounting, in
-- the plain text format of the ledger command line tool. exportjournal
-- writes it to a file the user names. hw also keeps it in a file next to
-- the database, up to date after each command, when the configuration
-- asks for it ("Haskoin.Wallet.Commands").
--
-- hw knows what happened on the chain: what each portfolio received and
-- paid, the fee of each payment, and which payments went from one
-- portfolio to another. It knows nothing of what the money was for, of
-- what a coin cost, or of its price on a given day. An accounting tool
-- knows those, and does not know the chain. The journal hands the first
-- to the second. The user includes the file in a journal of their own,
-- and keeps their prices, their categories and their reports there.
--
-- The link goes one way. hw writes the file and never reads one. The
-- file holds no price and no cost: hw would have to ask a server for
-- them, which is one more service to trust, and the rules for the cost
-- of a coin differ from one country to the next.
--
-- = One entry for each transaction
--
-- An entry is a transaction of the chain, with a posting for each
-- account it moves money in or out of. Its postings add up to zero, as
-- double-entry accounting asks.
--
-- A portfolio is an account, as in @Assets:hw:wallet0:portfolio0@. The
-- name is made of the indexes of the wallet and of the portfolio, which
-- never change. A description would read better and can change at any
-- time. The user gives the account a name of their own with the alias
-- directive of ledger.
--
-- The network is the commodity of the amounts, not a part of the account:
-- a portfolio holds BTC and BCH side by side, as it does in hw. The
-- commodity is the name of the network in capitals. So the coins of a
-- test network never add up with those of Bitcoin.
--
-- The journal is of the whole database. A listing of hw shows a
-- transaction from the side of one portfolio. A payment from portfolio0
-- to portfolio1 is outgoing in the first and incoming in the second. Two
-- journals, one for each portfolio, would count that money as spent and
-- as earned. One journal sees both sides, and writes one entry that moves
-- the money between the two accounts ('txEntry').
--
-- What comes from others is posted to @Income:Unknown@, and what goes to
-- others to @Expenses:Unknown@. hw cannot tell a salary from a refund.
-- The fee of a transaction the wallet made goes to
-- @Expenses:Fees:Mining@. A payment the wallet made has a posting for
-- each address of others it pays, with the address below it, and the
-- label of its contact when the address book has it.
--
-- = Which transactions
--
-- A confirmed transaction is written as cleared, with a star. An
-- unconfirmed one is written as pending, with an exclamation mark, so
-- that a report can leave it out. A dead transaction, replaced or
-- dropped, is left out: its money never moved. The transactions in
-- preparation are left out too, since they are not on the chain.
--
-- The file is written whole each time. A transaction that confirms, one
-- that is replaced, and a reorganisation of the chain then need nothing
-- special: the next file says what the chain says now. Adding the new
-- entries to the end of a file was the other choice. It would leave in
-- the file what the chain took back.
--
-- = The order and the dates
--
-- The entries of a network follow each other in the order of the chain,
-- the oldest first, as a journal is read. An entry has the date of its
-- transaction on the clock of the computer, since a tax year ends at
-- midnight there. The format has no time zones, so the time in UTC is in
-- a note of the entry.
--
-- The dates may run a little out of order, because a block may be dated
-- before the block under it ("Haskoin.Wallet.Database.TxInfo".'Standing').
--
-- = The balances at the end
--
-- The last entry states what each portfolio holds on each network, as
-- txs shows it. It moves nothing. ledger adds up the postings of the
-- file, and refuses the file when a sum is not the balance stated. So a
-- file cut short, or one edited by hand, does not pass for the history of
-- the wallet.
--
-- The balance is stated once, at the end, and not after each entry. Some
-- tools check a balance in the order of the dates, and the dates of the
-- chain are not always in order. A balance stated between two entries
-- dated the other way around would then fail on a correct file.
--
-- = Text from outside
--
-- A label can come from a file, written by another wallet or by the
-- online computer. In a journal, a line break in a label would start a
-- new line, which ledger would read as a posting or as an entry of its
-- own. So every text from outside is written on one line, its spaces as
-- single spaces, and its control characters as escapes ('journalSafe').
-- Two spaces in a row would end the name of the payee.
module Haskoin.Wallet.Cli.Journal
  ( journalText,
    localJournal,
    journalTimes,
    journalSafe,
    satoshisText,
    commodityOf,
    portfolioAccount,
    View,
    transactionName,
  )
where

import Data.Char (isAlpha, isSpace)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day, LocalTime (..), TimeZone, UTCTime, getTimeZone, utc, utcToLocalTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Sheet (ZoneAt)
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..), portfolioName, walletName)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.ShortId (shortHashText)
import Haskoin.Wallet.Tags (ShownTag (..), tagText)
import Haskoin.Wallet.Util (visibleText)

{- The parts of a journal -}

-- | Whether an entry is settled: a confirmed transaction is cleared, and
-- an unconfirmed one is pending. ledger marks the first with a star and
-- the second with an exclamation mark, and its reports keep either.
data EntryState = Cleared | StillPending
  deriving (Eq, Show)

-- | An entry of the journal: a transaction of the chain, or the balances
-- at the end
data Entry = Entry
  { entryDate :: !Day,
    entryState :: !EntryState,
    -- | The short id of the transaction, which finds it in hw. ledger
    -- calls this field the code of an entry. None for the balances.
    entryCode :: !(Maybe Text),
    entryPayee :: !Text,
    -- | The notes of the entry, each a name and its value: its txid, its
    -- time and its block
    entryNotes :: ![(Text, Text)],
    -- | The tags of the transaction, in the order of the alphabet
    entryTags :: ![Text],
    entryPostings :: ![Posting]
  }
  deriving (Eq, Show)

-- | A posting of an entry: what an account gains, or loses below zero
data Posting = Posting
  { postingAccount :: !Text,
    -- | The commodity of its amount: that of the network ('commodityOf')
    postingCommodity :: !Text,
    -- | The amount in satoshis
    postingAmount :: !Integer,
    -- | The balance the account must have after the posting, in satoshis,
    -- which ledger checks. Only the postings of the last entry state one.
    postingBalance :: !(Maybe Integer),
    -- | The notes of the posting, each a name and its value: the address
    -- paid, and the label of its contact
    postingNotes :: ![(Text, Text)]
  }
  deriving (Eq, Show)

-- | A posting of an amount of a network to an account, without a balance
-- to check and without notes
posting :: Network -> Text -> Integer -> Posting
posting net account amount = Posting account (commodityOf net) amount Nothing []

{- Names -}

-- | The account of a portfolio: @Assets:hw:wallet0:portfolio0@. It names
-- the wallet even in a database of one wallet. The views of hw leave the
-- wallet out there, and a journal cannot: the name of an account must
-- stay the same once a second wallet is made.
portfolioAccount :: PortfolioRef -> Text
portfolioAccount ref = "Assets:hw:" <> walletName (refWallet ref) <> ":" <> portfolioName (refPortfolio ref)

-- | The account of what the wallet pays to others, whose purpose hw does
-- not know. The name is the one ledger gives a posting it cannot place.
unknownExpenses :: Text
unknownExpenses = "Expenses:Unknown"

-- | The account of what the wallet receives from others, whose source hw
-- does not know
unknownIncome :: Text
unknownIncome = "Income:Unknown"

-- | The account of the fees of the transactions the wallet made. The fee
-- goes to the miners on every network, so the name fits Bitcoin Cash as
-- well as Bitcoin. The commodity tells the two apart.
miningFees :: Text
miningFees = "Expenses:Fees:Mining"

-- | The commodity of the amounts of a network: its name in capitals, as
-- in BTC, BCH and BTCTEST. ledger reads a commodity made of letters
-- alone. A name with a digit, such as that of btctest4, is written in
-- double quotes, as ledger asks.
commodityOf :: Network -> Text
commodityOf net
  | Text.all isAlpha name = name
  | otherwise = "\"" <> name <> "\""
  where
    name = Text.toUpper (cs net.name)

-- | An amount of satoshis as a number of coins with its eight decimals,
-- as in 0.00050000 and -1.20000000. The journal writes every amount in
-- full, so that ledger shows every one to the satoshi.
satoshisText :: Integer -> Text
satoshisText amount = sign <> cs (show coins) <> "." <> Text.justifyRight 8 '0' (cs (show satoshis))
  where
    sign = if amount < 0 then "-" else ""
    (coins, satoshis) = abs amount `divMod` 100000000

-- | A text from outside the wallet, such as a label, made safe to write
-- on a line of the journal. Every run of spaces, tabs and line breaks
-- becomes one space, and the spaces at both ends go. Any other control
-- character is written as an escape ('visibleText'), since ledger prints
-- the text on a terminal.
--
-- A line break would end the line, and the rest of the label would be
-- read as a line of the journal: a label could add a posting of its own.
-- Two spaces or a tab end the name of the payee, and what follows a
-- semicolon after them is read as a note.
journalSafe :: Text -> Text
journalSafe = visibleText . Text.unwords . Text.words . Text.map spaceForBreak
  where
    -- Text.words splits at the spaces of Data.Char alone. The line and
    -- paragraph separators of Unicode are not among them.
    spaceForBreak c
      | isSpace c || c `elem` ['\x2028', '\x2029', '\x85'] = ' '
      | otherwise = c

{- The entry of a transaction -}

-- | A transaction as one portfolio sees it: the portfolio, and what the
-- transaction means to it
type View = (PortfolioRef, TxInfo)

-- | The entry of a transaction of a network, given every portfolio of
-- the database that sees it, each with what it means to that portfolio,
-- and the status of the transaction on the chain. The function gives the
-- zone the date is read in.
--
-- Each portfolio gets a posting of what it gains or loses, fee included.
-- The other postings say where that money came from or went, so that
-- the postings add up to zero.
--
-- When the wallet made the transaction, every input is a coin of one of
-- its portfolios ('walletTransaction'). The fee is then the wallet's, and
-- gets a posting. Each address of others the transaction pays gets a
-- posting too. What is left is what the transaction pays to no address,
-- as an output with a script of its own does, and it is posted without
-- an address.
--
-- When another wallet made the transaction, its fee is the payer's, and
-- gets no posting. One posting balances what the portfolios gained. A
-- transaction that the wallet made with another one has inputs of both.
-- hw cannot tell who paid its fee, so it is treated the same way.
--
-- The postings come in the order money is read: what receives first,
-- what pays last.
txEntry :: ZoneAt -> Network -> ChainStatus -> NonEmpty View -> Entry
txEntry zoneAt net status views =
  Entry
    { entryDate = dayIn (zoneAt time) time,
      entryState = if chainConfirmations status > 0 then Cleared else StillPending,
      entryCode = Just (cs (shortHashText (chainTxid status))),
      entryPayee = fromMaybe fallbackPayee (listToMaybe (labelsOfTx <> labelsOfContacts <> labelsOfOwnAddresses)),
      entryNotes =
        [("txid", txHashToHex (chainTxid status)), ("time", utcText time)]
          <> [("block", cs (show height)) | Store.BlockRef height _ <- [chainBlock status]],
      entryTags = Set.toAscList (Set.fromList [tagText (shownTag shown) | (_, info) <- viewList, shown <- txOwnTags (txInfoTags info)]),
      entryPostings =
        [gain | gain <- portfolioPostings, postingAmount gain >= 0]
          <> paidToOthers
          <> [posting net unknownExpenses rest | rest > 0]
          <> [posting net miningFees feePaid | feePaid > 0]
          <> [loss | loss <- portfolioPostings, postingAmount loss < 0]
          <> [posting net unknownIncome rest | rest < 0]
    }
  where
    viewList = NonEmpty.toList views
    summaries = txInfoSummary . snd <$> viewList
    -- What the transaction means to the first portfolio that sees it.
    -- Every summary of a transaction has the same fee and the same
    -- outputs. They differ in which outputs they count as their own.
    firstSummary = txInfoSummary (snd (NonEmpty.head views))
    -- A transaction without a time has the date of 1970, which stands
    -- out. The rows of the database all have a time.
    time = fromMaybe (posixSecondsToUTCTime 0) (chainTime status)
    madeByWallet = walletTransaction summaries
    portfolioPostings = [posting net (portfolioAccount ref) (summaryAmount (txInfoSummary info)) | (ref, info) <- viewList]
    feePaid = if madeByWallet then toInteger (summaryFee firstSummary) else 0
    -- The addresses of the wallet the transaction pays, over every
    -- portfolio: to one portfolio, an address of another is of others
    ownAddresses = Set.unions (Map.keysSet . summaryMyOutputs <$> summaries)
    contacts = Map.unions (txInfoContacts . snd <$> viewList)
    -- What the transaction pays to addresses of no portfolio, the largest
    -- first, when the wallet made it
    outside =
      sortOn
        (Down . snd)
        [ (address, toInteger value)
          | madeByWallet,
            (address, value) <- Map.assocs (summaryOtherOutputs firstSummary),
            address `Set.notMember` ownAddresses
        ]
    paidToOthers =
      [ (posting net unknownExpenses value)
          { postingNotes =
              [("address", text) | Just text <- [addrToText net address]]
                <> [("contact", label) | Just label <- [contactLabelAt address]]
          }
        | (address, value) <- outside
      ]
    contactLabelAt address = nonEmpty . journalSafe . contactLabel =<< Map.lookup address contacts
    -- What the postings above leave out of balance: more than zero went
    -- to others, less than zero came from others
    rest = negate (sum (postingAmount <$> portfolioPostings)) - feePaid - sum (snd <$> outside)
    paysOthers = rest > 0 || not (null outside)
    -- The name of the entry: the label of the transaction, or else that
    -- of the contact it pays the most, or else, for a transaction that
    -- pays no one else, that of the address of the wallet it pays the
    -- most. The last tells an invoice from the label of the address it
    -- was paid at. It is left out of a payment to others, whose change
    -- may come back to a labelled address.
    labelsOfTx = mapMaybe (nonEmpty . journalSafe . txInfoLabel . snd) viewList
    labelsOfContacts = mapMaybe (contactLabelAt . fst) outside
    labelsOfOwnAddresses =
      [ label
        | not paysOthers,
          (_, written) <- sortOn (Down . fst) [(myOutputsValue paid, myOutputsLabel paid) | summary <- summaries, paid <- Map.elems (summaryMyOutputs summary)],
          Just label <- [nonEmpty (journalSafe written)]
      ]
    -- What an entry is called when nothing has a label. hw calls a
    -- transaction internal when it stays inside one portfolio, so one
    -- that moves money from a portfolio to another gets a name of its
    -- own.
    fallbackPayee
      | paysOthers = "Payment sent"
      | rest < 0 = "Payment received"
      | length viewList > 1 = "Transfer between portfolios"
      | otherwise = "Internal transfer"

-- | The name of a transaction of a network, as its entry has it: its
-- label, or else the label of the contact it pays the most, or else
-- that of the address of the wallet it pays the most, or else what it
-- did, as in Payment received ('txEntry'). The views are those of every
-- portfolio that sees the transaction, and the label of the first one
-- comes first.
--
-- The account statement of the command report describes its lines with
-- it ("Haskoin.Wallet.Report.AccountStatement"), so that the journal and
-- the statement call a transaction the same thing.
--
-- The zone gives the entry its date, which plays no part in its name,
-- so any zone does.
transactionName :: Network -> ChainStatus -> NonEmpty View -> Text
transactionName net status = entryPayee . txEntry (const utc) net status

-- | A text, or nothing for the empty text
nonEmpty :: Text -> Maybe Text
nonEmpty text = if Text.null text then Nothing else Just text

-- | The day a clock in the zone given reads at a time
dayIn :: TimeZone -> UTCTime -> Day
dayIn zone = localDay . utcToLocalTime zone

-- | The elements given, each key once: the first element of each key,
-- in the order they come
firstOfEach :: (Ord key) => (element -> key) -> [element] -> [element]
firstOfEach keyOf = go Set.empty
  where
    go _ [] = []
    go seen (element : others)
      | keyOf element `Set.member` seen = go seen others
      | otherwise = element : go (Set.insert (keyOf element) seen) others

-- | A time to the second in UTC, as a note of an entry gives it:
-- 2023-11-14 22:13:20 UTC
utcText :: UTCTime -> Text
utcText = cs . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC"

{- The journal of the wallet -}

-- | The transactions the journal holds, among those of the listings of
-- txs of every portfolio: the live ones, each with its status on the
-- chain, by network, in the order the networks first come. Every
-- transaction of a network comes once, with every portfolio that sees it,
-- in the order of the chain, the oldest first.
--
-- The rows of a portfolio come as txs lists them, the newest first. They
-- are turned around, then sorted by block. The sort keeps the order of
-- the transactions of one block, and of the unconfirmed ones, which is
-- the order txs gave them: a transaction comes after the ones it spends
-- from.
liveTransactions :: [(Place, ListedTx)] -> [(Network, [(ChainStatus, NonEmpty View)])]
liveTransactions rows =
  [ (net, sortOn (blockOrder . fst) (withViews (reverse ofNetwork)))
    | net <- firstOfEach (.name) [placeNetwork place | (place, _, _) <- live],
      let ofNetwork = [(placePortfolio place, info, status) | (place, info, status) <- live, (placeNetwork place).name == net.name]
  ]
  where
    live = [(place, info, status) | (place, ListedTx info _) <- rows, OnChain status <- [txInfoStatus info], not (chainDead status)]
    -- Each transaction once, where it first comes, with every portfolio
    -- that sees it, in the order the rows come. The views are gathered
    -- by transaction in one pass, so that a long history takes no longer
    -- for each of its transactions.
    withViews ofNetwork =
      [ (status, views)
        | (_, _, status) <- firstOfEach rowTxid ofNetwork,
          Just views <- [NonEmpty.nonEmpty (Map.findWithDefault [] (chainTxid status) viewsByTxid)]
      ]
      where
        viewsByTxid = Map.fromListWith (flip (<>)) [(chainTxid status, [(ref, info)]) | (ref, info, status) <- ofNetwork]
    rowTxid (_, _, status) = chainTxid status
    -- The confirmed transactions by the height of their block, then the
    -- unconfirmed ones
    blockOrder status = case chainBlock status of
      Store.BlockRef height _ -> (0 :: Int, height)
      Store.MemRef _ -> (1, 0)

-- | The entry that states the balances: what each portfolio holds on
-- each network, as txs shows it beside its newest live transaction. Each
-- posting moves nothing and states the balance its account must have.
-- Nothing when no portfolio has a live transaction.
--
-- The entry has the latest of the dates given, those of the entries of
-- the transactions. A tool that checks the balances in the order of the
-- dates then checks these after every entry. The day the file is written
-- was the other choice. The file would then change every day, with no
-- change in the history ('journalText').
balancesEntry :: [Day] -> [(Place, ListedTx)] -> Maybe Entry
balancesEntry dates rows
  | null stated || null dates = Nothing
  | otherwise = Just (Entry (maximum dates) Cleared Nothing "Balances of hw" [] [] stated)
  where
    -- The rows of a place come the newest first, so the first balance of
    -- a place is the one it has now
    stated =
      [ (posting net (portfolioAccount ref) 0) {postingBalance = Just balance}
        | (Place ref net, balance) <- firstOfEach (\(Place ref net, _) -> (ref, net.name)) [(place, balance) | (place, ListedTx _ (Just balance)) <- rows]
      ]

-- | The journal of the wallet as text, given the zone each date is read
-- in, and the rows of txs of every portfolio, each with its portfolio and
-- its network, as txs lists them.
--
-- The file starts with a comment that says what wrote it. Then come the
-- declarations of what the file uses: its commodities, its accounts and
-- the names of its notes and tags. ledger asks for them when it is run
-- with its strict checks, and a file that declares its own names passes
-- them without help. Then come the entries of each network, and last the
-- balances ('balancesEntry').
--
-- The text depends on the history alone, and on the zone of the clock.
-- It names no time of writing. So the same history gives the same file,
-- byte for byte. The journal hw keeps next to the database is written
-- again after many commands, and is left alone when nothing changed
-- ("Haskoin.Wallet.Commands"). A user who keeps the file under version
-- control then sees the changes of the history, and nothing else.
journalText :: ZoneAt -> [(Place, ListedTx)] -> Text
journalText zoneAt rows =
  Text.unlines . concat $
    [ [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
        "; hw writes this file whole each time. Do not edit it: your changes would be lost."
      ]
    ]
      <> [[""] <> concat [["commodity " <> commodity, "    format 1000.00000000 " <> commodity] | commodity <- commodities] | not (null commodities)]
      <> [[""] <> ["account " <> account | account <- accounts] | not (null accounts)]
      <> [[""] <> ["tag " <> name | name <- noteNames] | not (null noteNames)]
      <> [[""] <> entryLines widths entry | entry <- entries <> maybeToList balances]
  where
    entries = [txEntry zoneAt net status views | (net, transactions) <- liveTransactions rows, (status, views) <- transactions]
    balances = balancesEntry (entryDate <$> entries) rows
    postings = concatMap entryPostings (entries <> maybeToList balances)
    commodities = firstOfEach id (postingCommodity <$> postings)
    accounts = Set.toAscList (Set.fromList (postingAccount <$> postings))
    -- The names of the notes first, as the entries give them, then the
    -- tags of the transactions
    noteNames =
      firstOfEach id $
        concat [fst <$> entryNotes entry | entry <- entries]
          <> concat [fst <$> postingNotes posted | posted <- postings]
          <> Set.toAscList (Set.fromList (concatMap entryTags entries))
    widths =
      Widths
        { accountWidth = maximum (0 : (Text.length . postingAccount <$> postings)),
          amountWidth = maximum (0 : (Text.length . satoshisText . postingAmount <$> postings))
        }

-- | The widths of the columns of the postings of a file: the accounts
-- start in one column and the amounts end in one column, through the
-- whole file
data Widths = Widths
  { accountWidth :: !Int,
    amountWidth :: !Int
  }

-- | The lines of an entry: its date, its state, its code and its payee,
-- then its notes and its tags, then its postings, each with its notes
-- below it. ledger gives the notes right below the first line to the
-- entry, and those below a posting to that posting.
entryLines :: Widths -> Entry -> [Text]
entryLines widths entry =
  [ Text.unwords $
      [cs (formatTime defaultTimeLocale "%Y-%m-%d" (entryDate entry)), stateMark (entryState entry)]
        <> ["(" <> code <> ")" | Just code <- [entryCode entry]]
        <> [entryPayee entry]
  ]
    <> [noteLine note | note <- entryNotes entry]
    <> ["    ; :" <> Text.intercalate ":" (entryTags entry) <> ":" | not (null (entryTags entry))]
    <> concat [postingLine widths posted : (noteLine <$> postingNotes posted) | posted <- entryPostings entry]

-- | The mark of the state of an entry, as ledger reads it
stateMark :: EntryState -> Text
stateMark = \case
  Cleared -> "*"
  StillPending -> "!"

-- | The line of a note: a name, a colon and a value, in a comment
noteLine :: (Text, Text) -> Text
noteLine (name, value) = "    ; " <> name <> ": " <> value

-- | The line of a posting: its account, then its amount and its
-- commodity, then the balance it states, if any. Two spaces or more end
-- the name of the account, as ledger asks. The amounts of a file end in
-- one column.
postingLine :: Widths -> Posting -> Text
postingLine widths posted =
  "    "
    <> Text.justifyLeft (accountWidth widths) ' ' (postingAccount posted)
    <> "  "
    <> Text.justifyRight (amountWidth widths) ' ' (satoshisText (postingAmount posted))
    <> " "
    <> postingCommodity posted
    <> maybe "" (\balance -> " = " <> satoshisText balance <> " " <> postingCommodity posted) (postingBalance posted)

{- On this computer -}

-- | Every time the journal reads a date from, in UTC: those of the live
-- transactions of the rows
journalTimes :: [(Place, ListedTx)] -> [UTCTime]
journalTimes rows = [time | (_, transactions) <- liveTransactions rows, (status, _) <- transactions, Just time <- [chainTime status]]

-- | The journal of the wallet as text, its dates on the clock of this
-- computer ('journalText'). The zone of each date is the one the computer
-- had on that day: a transaction of the summer is dated in summer time.
localJournal :: [(Place, ListedTx)] -> IO Text
localJournal rows = do
  zones <- Map.fromList <$> mapM (\time -> (time,) <$> getTimeZone time) (nubOrd (journalTimes rows))
  return $ journalText (\time -> Map.findWithDefault utc time zones) rows
