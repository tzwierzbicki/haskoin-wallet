{-# LANGUAGE OverloadedStrings #-}

-- | What the specs of the statements of report share: the story of
-- their transactions, the days and the times they are made at, and how
-- they read a report.
module Haskoin.Wallet.Report.TestUtils
  ( received,
    spent,
    spentUnconfirmed,
    datedAt,
    savings,
    spending,
    paidInPart,
    day14,
    day13,
    syncedInJanuary,
    preparedAt,
    hashOfBlock2,
    factsNamed,
    tablesOf,
    notesOf,
  )
where

import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), UTCTime (..), ZonedTime (..), fromGregorian)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Report.Layout
import Haskoin.Wallet.Report.Statement
-- The rows of a table are those of a report here, not those of a
-- table of the database
import Haskoin.Wallet.TestUtils hiding (tableRows)

-- The story is that of the shared summaries. portfolio0 of wallet0
-- receives 50000 at its first receiving address,
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, in block 800000. It then spends
-- that coin in block 800001: 40000 to an address of others, 9000 of
-- change to its first change address,
-- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, and 1000 of fee.
--
-- Both blocks are dated at seenTime, 14 Nov 2023 22:13:20 UTC, unless a
-- test says otherwise. A clock of Central Europe in winter reads that as
-- 23:13:20 on the same day.

-- | The payment received, confirmed in block 800000
received :: TxInfo
received = txOnChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False ""

-- | The payment made, confirmed in block 800001
spent :: TxInfo
spent = txOnChain spendSummary 2 (Store.BlockRef 800001 1) 2 False ""

-- | The payment made, still in the mempool
spentUnconfirmed :: TxInfo
spentUnconfirmed = txOnChain spendSummary 2 (Store.MemRef 1700000000) 0 False ""

-- | A transaction of the chain with its block dated at the time given
datedAt :: UTCTime -> TxInfo -> TxInfo
datedAt time info = case txInfoStatus info of
  OnChain status -> info {txInfoStatus = OnChain status {chainTime = Just time}}
  Pending _ -> info

-- | portfolio0 of wallet0, described as "Savings", with the transactions
-- given on btc
savings :: [TxInfo] -> PortfolioHistory
savings transactions = PortfolioHistory mainPortfolioRef "" "Savings" [(btc, [ListedTx info Nothing | info <- transactions])]

-- | portfolio1 of wallet0, described as "Spending", which the payment
-- made pays: it sees the 40000 of the transaction 2 come to one of its
-- addresses ('spendSeenByPortfolio1')
spending :: PortfolioHistory
spending = PortfolioHistory (PortfolioRef 0 1) "" "Spending" [(btc, [ListedTx (txOnChain spendSeenByPortfolio1 2 (Store.BlockRef 800001 1) 2 False "") Nothing])]

-- | The two portfolios when portfolio0 pays portfolio1 and somebody else
-- at once ('batchSummary'). portfolio0 receives 50000 twice in block
-- 800000, then pays 60000 to portfolio1 and 30000 to another wallet in
-- block 800001, with 1000 of fee: it loses 91000 and holds 9000.
-- portfolio1 gains the 60000 ('batchSeenByPortfolio1').
paidInPart :: [PortfolioHistory]
paidInPart =
  [ savings [txOnChain batchSummary 5 (Store.BlockRef 800001 1) 2 False "", txOnChain paymentSummary 3 (Store.BlockRef 800000 2) 3 False "", received],
    PortfolioHistory (PortfolioRef 0 1) "" "Spending" [(btc, [ListedTx (txOnChain batchSeenByPortfolio1 5 (Store.BlockRef 800001 1) 2 False "") Nothing])]
  ]

-- | 14 Nov 2023, the day of seenTime on a clock of Central Europe
day14, day13 :: Day
day14 = fromGregorian 2023 11 14
day13 = fromGregorian 2023 11 13

-- | The block 800002, dated 2 Jan 2024 at 12:00 UTC, 13:00 in Central
-- Europe: a sync that reached it knows the whole of 2023
syncedInJanuary :: SyncedTo
syncedInJanuary = SyncedTo 800002 (fakeBlockHash 2) (Just (UTCTime (fromGregorian 2024 1 2) (12 * 3600)))

-- | 15 Nov 2026 at 14:32:07 on a clock of Central Europe in winter
preparedAt :: ZonedTime
preparedAt = ZonedTime (LocalTime (fromGregorian 2026 11 15) (TimeOfDay 14 32 7)) cet

-- | The hash of the block 2 of the tests as hw writes it: 62 zeros, then
-- 02
hashOfBlock2 :: Text
hashOfBlock2 = "0000000000000000000000000000000000000000000000000000000000000002"

-- | The facts of a report under a name
factsNamed :: Text -> Report -> [[Text]]
factsNamed name report = [factLines fact | FactsBlock facts <- reportBlocks report, fact <- facts, factName fact == name]

-- | The rows of each table of a report, each row as the texts of its
-- cells
tablesOf :: Report -> [[[Text]]]
tablesOf report = [[cellText <$> rowCells row | row <- tableRows table] | TableBlock table <- reportBlocks report]

-- | The notes of a report
notesOf :: Report -> [Text]
notesOf report = concat [notes | NotesBlock notes <- reportBlocks report]
