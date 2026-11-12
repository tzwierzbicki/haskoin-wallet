{-# LANGUAGE OverloadedStrings #-}

-- | The sheets --csv and exportsheet write: the columns of each listing,
-- and the cells of its rows, with their kinds. Both files hold the same
-- sheet of a listing. Every sheet starts with the account of a row, its
-- wallet, portfolio, network and type, and two sheets give the columns
-- they share in the same order.
module Haskoin.Wallet.Cli.SheetSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time (LocalTime (..), TimeOfDay (..), TimeZone (..), UTCTime (..), fromGregorian)
import Data.Word (Word8)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Response (AccountCounts (..), Group (..), Response (..))
import Haskoin.Wallet.Cli.Sheet (Cell (..), PortfolioSheets (..), Sheet (..), listingSheet, sheetTimes, stacked, walletSheets)
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (AccountKind (..), legacyPath)
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec

-- Every time below is seenTime, 14 Nov 2023 22:13:20 UTC, unless a test
-- says otherwise. The sheets are made for a clock of Central Europe in
-- winter, an hour ahead of UTC, which reads 23:13:20 then
-- ('seenTimeCells').
-- The txid of fakeTxHash n is 62 zeros and n in hexadecimal. The rows are
-- those of portfolio0 of wallet0 on btc, mainPlace, whose bip44 account
-- has 12 receiving and 3 change addresses.

-- | The sheet of an answer on a clock of Central Europe in winter
cetSheet :: Response -> Maybe Sheet
cetSheet = listingSheet (const cet)

spec :: Spec
spec = do
  describe "The sheet of a listing" $ do
    -- The answer of each of the six listings with no rows, by its name
    let emptyListings =
          [ ("portfolios" :: String, ResponsePortfolios []),
            ("addresses", ResponseAddresses []),
            ("balances", ResponseBalances []),
            ("coins", ResponseCoins []),
            ("txs", ResponseTxs []),
            ("pendingtxs", ResponsePendingTxs Nothing [])
          ]

    it "is none for an answer that lists nothing, such as an error" $
      cetSheet (ResponseError "There are no portfolios in the wallet") `shouldBe` Nothing

    it "starts with the wallet, the portfolio, the network and the type, whatever the listing" $
      [(name, take 4 . sheetHeadings <$> cetSheet answer) | (name, answer) <- emptyListings]
        `shouldBe` [(name, Just ["Wallet", "Portfolio", "Network", "Type"]) | name <- ["portfolios", "addresses", "balances", "coins", "txs", "pendingtxs"]]

    -- The headings two sheets share, as each sheet orders them
    it "gives the columns two listings share in the same order in both" $ do
      let headingsOf answer = maybe [] sheetHeadings (cetSheet answer)
          sharedIn a b = filter (`elem` headingsOf b) (headingsOf a)
      [(nameA, nameB) | (nameA, a) <- emptyListings, (nameB, b) <- emptyListings, sharedIn a b /= sharedIn b a]
        `shouldBe` []

    it "ends with the date the wallet created the element, for the listings whose elements have one" $ do
      let lastThree answer = reverse . take 3 . reverse . sheetHeadings <$> cetSheet answer
      [lastThree answer | answer <- [ResponsePortfolios [], ResponseAddresses [], ResponseBalances [], ResponsePendingTxs Nothing []]]
        `shouldBe` replicate 4 (Just ["Created (UTC)", "Created (local)", "Created (offset)"])

  timeColumnsSpec
  portfolioSheetSpec
  addressSheetSpec
  balanceSheetSpec
  coinSheetSpec
  txSheetSpec
  pendingSheetSpec
  walletSheetSpec

-- | The three columns of a time, shown on the time of a coin
timeColumnsSpec :: Spec
timeColumnsSpec =
  describe "A time of a sheet" $ do
    let timesOf zoneAt coins = cellsUnder ["Time (UTC)", "Time (local)", "Time (offset)"] <$> listingSheet zoneAt (ResponseCoins [mainGroup coins])

    -- 22:13:20 UTC + 1 hour = 23:13:20
    it "comes in UTC, then as the clock of the computer read it, then with how far that clock was from UTC" $
      timesOf (const cet) [spendableCoinRow]
        `shouldBe` Just [[TimeCell seenTime, LocalTimeCell (LocalTime (fromGregorian 2023 11 14) (TimeOfDay 23 13 20)), TextCell "UTC+01:00"]]

    -- A clock of Central Europe is two hours ahead of UTC on 14 Jul 2023,
    -- and one hour ahead on 14 Nov 2023: 12:00:00 + 2 hours = 14:00:00
    it "has the offset of its own day, summer time for a time of the summer" $ do
      let july = UTCTime (fromGregorian 2023 7 14) (12 * 3600)
          zoneAt time = if time == july then TimeZone 120 True "CEST" else cet
      timesOf zoneAt [spendableCoinRow {jsonCoinTime = Just july}, spendableCoinRow]
        `shouldBe` Just
          [ [TimeCell july, LocalTimeCell (LocalTime (fromGregorian 2023 7 14) (TimeOfDay 14 0 0)), TextCell "UTC+02:00"],
            [TimeCell seenTime, LocalTimeCell (LocalTime (fromGregorian 2023 11 14) (TimeOfDay 23 13 20)), TextCell "UTC+01:00"]
          ]

    -- Newfoundland is 3 hours and 30 minutes behind UTC in winter:
    -- 22:13:20 - 3:30:00 = 18:43:20
    it "has its offset to the minute, with a minus sign for a clock behind UTC" $
      timesOf (const (TimeZone (-210) False "NST")) [spendableCoinRow]
        `shouldBe` Just [[TimeCell seenTime, LocalTimeCell (LocalTime (fromGregorian 2023 11 14) (TimeOfDay 18 43 20)), TextCell "UTC-03:30"]]

    it "leaves its three cells empty when hw does not know it" $
      timesOf (const cet) [spendableCoinRow {jsonCoinTime = Nothing}]
        `shouldBe` Just [[EmptyCell, EmptyCell, EmptyCell]]

    it "is found among the times of the sheet once, by its cell in UTC" $
      sheetTimes <$> cetSheet (ResponseCoins [mainGroup [spendableCoinRow]]) `shouldBe` Just [seenTime]

portfolioSheetSpec :: Spec
portfolioSheetSpec =
  describe "The sheet of portfolios" $ do
    it "names its columns: where the portfolio is, whether it is the active one, what describes it, its amounts and counts, then when it was created, without the keys or the fingerprint of the seed" $
      sheetHeadings <$> cetSheet (ResponsePortfolios [])
        `shouldBe` Just ["Wallet", "Portfolio", "Network", "Type", "Active", "Description", "Wallet description", "Account paths", "Spendable (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Coins", "Addresses", "Created (UTC)", "Created (local)", "Created (offset)"]

    -- portfolio0 is the active portfolio, described as savings, in a
    -- wallet without a description. The view names no account. It holds
    -- 5000000 confirmed and 100000 unconfirmed on btc, in 3 coins, can
    -- spend 4900000 now, and has 17 addresses there.
    it "gives a portfolio a row on its network: where it is, yes for the active one, its description, its amounts in satoshis, its coins and addresses, and when it was created" $
      sheetRows <$> cetSheet (ResponsePortfolios [mainPortfolioView])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc", EmptyCell, TextCell "yes", TextCell "savings", EmptyCell, EmptyCell, SatoshiCell 4900000, SatoshiCell 5000000, SatoshiCell 100000, CountCell 3, CountCell 17] <> seenTimeCells]

    -- The accounts of portfolio 0 on btc are m/44'/0'/0' and m/84'/0'/0'
    it "names the types of the accounts of the portfolio on the network, and the path of each" $ do
      let withAccounts = mainPortfolioView {viewNetworks = [NetworkView btc 5000000 100000 4900000 3 17 [legacyKeys, segwitKeys]]}
      (cellsUnder ["Type", "Account paths"] <$> cetSheet (ResponsePortfolios [withAccounts]))
        `shouldBe` Just [[TextCell "bip44, bip84", TextCell "/44'/0'/0', /84'/0'/0'"]]

    it "gives the description of the wallet of a portfolio in its own column" $
      (cellsUnder ["Description", "Wallet description"] <$> cetSheet (ResponsePortfolios [mainPortfolioView {viewWalletDescription = "Family"}]))
        `shouldBe` Just [[TextCell "savings", TextCell "Family"]]

    it "says no in the column Active of a portfolio that is not the active one" $
      (cellsUnder ["Wallet", "Portfolio", "Active"] <$> cetSheet (ResponsePortfolios [mainPortfolioView {viewActive = False}]))
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "no"]]

    -- A whole portfolio has an account on eight networks
    it "gives a portfolio a row for each network it has an account on, in the order of its networks" $ do
      let onBch = NetworkView bch 0 0 0 0 0 []
          both = mainPortfolioView {viewNetworks = viewNetworks mainPortfolioView <> [onBch]}
      (cellsUnder ["Wallet", "Portfolio", "Network", "Spendable (sats)", "Coins"] <$> cetSheet (ResponsePortfolios [both]))
        `shouldBe` Just
          [ [TextCell "wallet0", TextCell "portfolio0", TextCell "btc", SatoshiCell 4900000, CountCell 3],
            [TextCell "wallet0", TextCell "portfolio0", TextCell "bch", SatoshiCell 0, CountCell 0]
          ]

    it "gives the rows of each portfolio after those of the one before, each named by its wallet and its index" $ do
      let other = mainPortfolioView {viewRef = PortfolioRef 2 1, viewActive = False, viewDescription = ""}
      (cellsUnder ["Wallet", "Portfolio", "Network", "Active", "Description"] <$> cetSheet (ResponsePortfolios [mainPortfolioView, other]))
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "yes", TextCell "savings"], [TextCell "wallet2", TextCell "portfolio1", TextCell "btc", TextCell "no", EmptyCell]]

addressSheetSpec :: Spec
addressSheetSpec =
  describe "The sheet of addresses" $ do
    it "names its columns: the account of the address, the address, its path, chain and label, its amounts and counts, then when it was created" $
      sheetHeadings <$> cetSheet (ResponseAddresses [])
        `shouldBe` Just ["Wallet", "Portfolio", "Network", "Type", "Address", "Path", "Chain", "Address label", "Spendable (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Received (sats)", "Coins", "Transactions", "Created (UTC)", "Created (local)", "Created (offset)"]

    -- /0/0 received 50000 in one transaction, and holds it in one coin,
    -- confirmed, which it can spend
    it "gives an address a row: its account, the address, its path, its chain, its label, its amounts in satoshis, its coins and transactions, and when it was created" $
      sheetRows <$> cetSheet (ResponseAddresses [mainGroup [(paidAddressRow, 50000)]])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "bip44", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "receiving", TextCell "Invoice 42", SatoshiCell 50000, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 50000, CountCell 1, CountCell 1] <> seenTimeCells]

    it "leaves the label of an address without one empty" $
      (cellsUnder ["Address", "Path", "Address label"] <$> cetSheet (ResponseAddresses [mainGroup [(unusedAddressRow, 0)]]))
        `shouldBe` Just [[TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", TextCell "/0/1", EmptyCell]]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is /1/0 of m/44'/0'/0'
    it "says change in the column Chain of a change address" $ do
      let changeRow = unusedAddressRow {dBAddressInternal = True, dBAddressIndex = 0, dBAddressAddress = "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"}
      (cellsUnder ["Address", "Path", "Chain"] <$> cetSheet (ResponseAddresses [mainGroup [(changeRow, 0)]]))
        `shouldBe` Just [[TextCell "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", TextCell "/1/0", TextCell "change"]]

    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is /0/0 of m/84'/0'/0':
    -- both accounts of the portfolio have an address at /0/0
    it "gives a native SegWit address the type bip84, which tells it from the bip44 address at the same path" $ do
      let segwitRow = unusedAddressRow {dBAddressIndex = 0, dBAddressAddress = "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"}
          groups = [mainGroup [(paidAddressRow, 50000)], Group mainPlace [AccountCounts Bip84 2 0] [(segwitRow, 0)]]
      (cellsUnder ["Network", "Type", "Address", "Path"] <$> cetSheet (ResponseAddresses groups))
        `shouldBe` Just
          [ [TextCell "btc", TextCell "bip44", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0"],
            [TextCell "btc", TextCell "bip84", TextCell "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", TextCell "/0/0"]
          ]

    it "gives the addresses of each network after those of the network before, each with its network" $ do
      let bchRow = unusedAddressRow {dBAddressIndex = 0, dBAddressAddress = "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"}
          groups = [mainGroup [(paidAddressRow, 50000)], Group (Place mainPortfolioRef bch) [AccountCounts Bip44 1 0] [(bchRow, 0)]]
      (cellsUnder ["Network", "Type", "Address", "Path"] <$> cetSheet (ResponseAddresses groups))
        `shouldBe` Just
          [ [TextCell "btc", TextCell "bip44", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0"],
            [TextCell "bch", TextCell "bip44", TextCell "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", TextCell "/0/0"]
          ]

    it "names the wallet and the portfolio of the addresses of another portfolio" $
      (cellsUnder ["Wallet", "Portfolio", "Network"] <$> cetSheet (ResponseAddresses [Group (Place (PortfolioRef 2 1) btc) [AccountCounts Bip44 1 0] [(paidAddressRow, 50000)]]))
        `shouldBe` Just [[TextCell "wallet2", TextCell "portfolio1", TextCell "btc"]]

balanceSheetSpec :: Spec
balanceSheetSpec =
  describe "The sheet of balances" $ do
    let total = CoinBalance 80000 50000 30000 0

    it "names its columns: those that name an address, the value of its coins and the three parts it cannot spend, then the columns the sheet of addresses ends with" $
      sheetHeadings <$> cetSheet (ResponseBalances [])
        `shouldBe` Just ["Wallet", "Portfolio", "Network", "Type", "Address", "Path", "Chain", "Address label", "Total (sats)", "Spendable (sats)", "Locked by tx (sats)", "Locked by you (sats)", "Not spendable yet (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Received (sats)", "Coins", "Transactions", "Created (UTC)", "Created (local)", "Created (offset)"]

    -- 80000 in all, 50000 spendable, 30000 locked by a pending
    -- transaction: 80000 - 50000 - 30000 = 0 not spendable yet. The store
    -- counts 50000 confirmed and received at /0/0, in one coin and one
    -- transaction.
    it "gives an address a row: its account, the address, its path, chain and label, the balance of its coins in satoshis, what the store counts there, and when it was created" $
      sheetRows <$> cetSheet (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 30000 0)], total)])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "bip44", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "receiving", TextCell "Invoice 42", SatoshiCell 80000, SatoshiCell 50000, SatoshiCell 30000, SatoshiCell 0, SatoshiCell 0, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 50000, CountCell 1, CountCell 1] <> seenTimeCells]

    -- 80000 - 50000 spendable - 10000 locked by the user = 20000
    it "gives as not spendable yet what the address cannot spend that nothing locks" $
      (cellsUnder ["Total (sats)", "Spendable (sats)", "Locked by tx (sats)", "Locked by you (sats)", "Not spendable yet (sats)"] <$> cetSheet (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 0 10000)], total)]))
        `shouldBe` Just [[SatoshiCell 80000, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 10000, SatoshiCell 20000]]

coinSheetSpec :: Spec
coinSheetSpec =
  describe "The sheet of coins" $ do
    let under headings coin = cellsUnder headings <$> cetSheet (ResponseCoins [mainGroup [coin]])

    it "names its columns: the account of the coin, the coin and its transaction, each in full and by its short id, its time, value and state, then its address and both labels" $
      sheetHeadings <$> cetSheet (ResponseCoins [])
        `shouldBe` Just ["Wallet", "Portfolio", "Network", "Type", "Coin", "Short coin", "Txid", "Short txid", "Output index", "Time (UTC)", "Time (local)", "Time (offset)", "Value (sats)", "State", "Confirmations", "Block", "Address", "Path", "Chain", "Address label", "Transaction label"]

    -- spendableCoinRow is output 0 of transaction 1, whose txid starts
    -- with 00000000
    it "gives a coin a row: its account, the coin and its transaction in full and by their short ids, the index of its output, the time of its transaction, its value in satoshis, its state, its confirmations and block, then its address, path and chain" $
      sheetRows <$> cetSheet (ResponseCoins [mainGroup [spendableCoinRow]])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "bip44", TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TextCell "00000000:0", TextCell "0000000000000000000000000000000000000000000000000000000000000001", TextCell "00000000", CountCell 0] <> seenTimeCells <> [SatoshiCell 50000, TextCell "spendable", CountCell 3, CountCell 800000, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "receiving", EmptyCell, EmptyCell]]

    it "gives the short id of a coin as the short id of its txid, a colon and its index, and the txid and the index each in a column" $
      under ["Coin", "Short coin", "Txid", "Short txid", "Output index"] spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1}
        `shouldBe` Just [[TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1", TextCell "a1075db5:1", TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5", CountCell 1]]

    it "says a coin a pending transaction spends is locked by a pending transaction" $
      under ["State"] spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}
        `shouldBe` Just [[TextCell "locked by a pending transaction"]]

    it "says an unconfirmed coin is not spendable yet, with 0 confirmations and no block" $
      under ["State", "Confirmations", "Block"] spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False}
        `shouldBe` Just [[TextCell "not spendable yet", CountCell 0, EmptyCell]]

    it "gives the label of the address and that of the transaction, each in its column" $
      under ["Address label", "Transaction label"] spendableCoinRow {jsonCoinAddressLabel = "Invoice 42", jsonCoinTxLabel = "Salary"}
        `shouldBe` Just [[TextCell "Invoice 42", TextCell "Salary"]]

    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is /0/0 of m/84'/0'/0'
    it "gives a coin of the bip84 account the type bip84" $
      under ["Type", "Address", "Path"] spendableCoinRow {jsonCoinAddress = segwitExternalAddr 0, jsonCoinPath = Just (segwitPath (Deriv :/ 0 :/ 0))}
        `shouldBe` Just [[TextCell "bip84", TextCell "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", TextCell "/0/0"]]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is /1/0 of m/44'/0'/0'
    it "says change in the column Chain of a coin at a change address" $
      under ["Address", "Path", "Chain"] spendableCoinRow {jsonCoinAddress = internalAddr 0, jsonCoinPath = Just (legacyPath (Deriv :/ 1 :/ 0)), jsonCoinChange = True}
        `shouldBe` Just [[TextCell "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", TextCell "/1/0", TextCell "change"]]

-- | A transaction on the chain: its summary, the last byte of its txid,
-- its block, its confirmations, whether it is dead, and its label
onChain :: TxSummary -> Word8 -> Store.BlockRef -> Natural -> Bool -> Text -> TxInfo
onChain summary txid block confirmations dead label =
  TxInfo summary (OnChain (ChainStatus (fakeTxHash txid) block (Just seenTime) confirmations dead [])) label Map.empty Map.empty

txSheetSpec :: Spec
txSheetSpec =
  describe "The sheet of txs" $ do
    let sheetOfTxs txs = cetSheet (ResponseTxs [mainGroup txs])
        under headings info = cellsUnder headings <$> sheetOfTxs [ListedTx info (Just 9000)]
        spent = onChain spendSummary 2 (Store.BlockRef 800001 1) 2 False ""
        fromColumns = ["From 1 address", "From 1 path", "From 1 label", "From 1 value (sats)", "From 2 address", "From 2 path", "From 2 label", "From 2 value (sats)"]
        toColumns = ["To address", "To path", "To label", "To value (sats)"]
        changeColumns = ["Change address", "Change path", "Change value (sats)"]

    it "names its columns: the accounts of the transaction, its txid, its time, what it does to the portfolio, where it stands, its label, then two addresses it spends from, one it pays, its change and the count of the others" $
      sheetHeadings <$> cetSheet (ResponseTxs [])
        `shouldBe` Just
          [ "Wallet", "Portfolio", "Network", "Type",
            "Txid", "Short txid",
            "Time (UTC)", "Time (local)", "Time (offset)",
            "Direction", "Amount (sats)", "Fee (sats)", "Fee rate (sat/vB)", "Size (vB)", "Balance (sats)",
            "Status", "Confirmations", "Block", "Replaced by",
            "Transaction label",
            "From 1 address", "From 1 path", "From 1 label", "From 1 value (sats)",
            "From 2 address", "From 2 path", "From 2 label", "From 2 value (sats)",
            "To address", "To path", "To label", "To value (sats)",
            "Change address", "Change path", "Change value (sats)",
            "Other addresses",
            "Lock time"
          ]

    -- The payment received pays 50000 at /0/0 of the bip44 account. Its
    -- payer spent from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and paid 1000 of
    -- fee, at 4 sat/vB for 226 vB. The txids of fakeTxHash all start with
    -- 00000000.
    it "gives a payment received a row: its account, txid, time, direction, amount, fee and balance, where it stands, its label, the address of its payer without a value, and the address it was paid at with its path and value" $
      sheetRows <$> sheetOfTxs [ListedTx (onChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False "Salary") (Just 50000)]
        `shouldBe` Just
          [ [TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "bip44"]
              <> [TextCell "0000000000000000000000000000000000000000000000000000000000000001", TextCell "00000000"]
              <> seenTimeCells
              <> [TextCell "Incoming", SatoshiCell 50000, SatoshiCell 1000, CountCell 4, CountCell 226, SatoshiCell 50000]
              -- Confirmed in block 800000, replaced by nothing
              <> [TextCell "confirmed", CountCell 3, CountCell 800000, EmptyCell]
              <> [TextCell "Salary"]
              -- From 1 is of another wallet, and there is no From 2
              <> [TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, EmptyCell]
              <> [EmptyCell, EmptyCell, EmptyCell, EmptyCell]
              -- To, without a label, and no change
              <> [TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, SatoshiCell 50000]
              <> [EmptyCell, EmptyCell, EmptyCell]
              -- No other address, no lock time
              <> [CountCell 0, EmptyCell]
          ]

    -- The coin of 50000 at /0/0 pays 40000 to
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 9000 of change to /1/0,
    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn. 40000 paid + 1000 of fee = 41000
    -- leave the portfolio, which keeps 9000.
    it "gives a payment a row: the address it spends from with its path and value, the address of the other wallet it pays with no path, and its change" $
      sheetRows <$> sheetOfTxs [ListedTx spent (Just 9000)]
        `shouldBe` Just
          [ [TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "bip44"]
              <> [TextCell "0000000000000000000000000000000000000000000000000000000000000002", TextCell "00000000"]
              <> seenTimeCells
              <> [TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, CountCell 226, SatoshiCell 9000]
              <> [TextCell "confirmed", CountCell 2, CountCell 800001, EmptyCell]
              <> [EmptyCell]
              -- From 1, and no From 2
              <> [TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, SatoshiCell 50000]
              <> [EmptyCell, EmptyCell, EmptyCell, EmptyCell]
              -- To, then the change
              <> [TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, SatoshiCell 40000]
              <> [TextCell "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", TextCell "/1/0", SatoshiCell 9000]
              <> [CountCell 0, EmptyCell]
          ]

    -- The coin of 50000 at /0/0 of the bip44 account pays 40000 to
    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc, /0/0 of the bip84 account,
    -- and 9000 of change to /1/0 of the bip44 one: 50000 - 40000 - 9000 =
    -- 1000 of fee, all the portfolio loses
    it "names both types for a transaction between the two accounts of the portfolio" $ do
      let toSegwit =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(segwitExternalAddr 0, MyOutputs 40000 (segwitPath (Deriv :/ 0 :/ 0)) ""), (internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
      under ["Type", "Direction"] (onChain toSegwit 2 (Store.BlockRef 800001 1) 2 False "")
        `shouldBe` Just [[TextCell "bip44, bip84", TextCell "Internal"]]

    it "gives the short id of the txid, its first 8 digits, after the txid" $
      under ["Txid", "Short txid"] (TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty Map.empty)
        `shouldBe` Just [[TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5"]]

    it "gives the label of the address a payment was received at in the column of its label" $
      under toColumns (onChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False "")
        `shouldBe` Just [[TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "Rent", SatoshiCell 50000]]

    it "gives the label of the address a payment spends from in the column of its label" $
      under ["From 1 address", "From 1 path", "From 1 label", "From 1 value (sats)"] (onChain spendFromRent 2 (Store.BlockRef 800001 1) 2 False "")
        `shouldBe` Just [[TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "Rent", SatoshiCell 50000]]

    it "says an unconfirmed transaction is unconfirmed, with 0 confirmations and no block" $
      under ["Status", "Confirmations", "Block"] (onChain spendSummary 2 (Store.MemRef 1700000000) 0 False "")
        `shouldBe` Just [[TextCell "unconfirmed", CountCell 0, EmptyCell]]

    -- The balance of the portfolio leaves a dead transaction out
    it "says a dead transaction is dead, and leaves its balance empty" $
      (cellsUnder ["Balance (sats)", "Status"] <$> sheetOfTxs [ListedTx (onChain spendSummary 2 (Store.MemRef 1700000000) 0 True "") Nothing])
        `shouldBe` Just [[EmptyCell, TextCell "dead"]]

    it "names the transaction that replaced a dead one, by its txid in full" $ do
      let bumped = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [b2d4f6a8Txid])) "" Map.empty Map.empty
      under ["Status", "Replaced by"] bumped
        `shouldBe` Just [[TextCell "dead", TextCell "b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001"]]

    it "names every transaction that replaced a dead one in one cell, apart by a comma and a space" $ do
      let bumped = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.MemRef 1700000000) (Just seenTime) 0 True [a1075db5First, b2d4f6a8Txid])) "" Map.empty Map.empty
      under ["Replaced by"] bumped
        `shouldBe` Just [[TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001, b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001"]]

    it "leaves the cell of what replaced a dead transaction empty when nothing did" $
      under ["Status", "Replaced by"] (onChain spendSummary 2 (Store.MemRef 1700000000) 0 True "")
        `shouldBe` Just [[TextCell "dead", EmptyCell]]

    -- /0/0 is 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj and /0/1 is
    -- 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg. The larger coin is at /0/1 in the
    -- first transaction and at /0/0 in the second.
    it "names the address a transaction spends the most from first, then the next one, whichever address each is" $ do
      let spending atFirst atSecond =
            onChain
              spendSummary
                { summaryMyInputs =
                    Map.fromList
                      [ (externalAddr 0, MyInputs atFirst (legacyPath (Deriv :/ 0 :/ 0)) "" [OutPoint (fakeTxHash 1) 0]),
                        (externalAddr 1, MyInputs atSecond (legacyPath (Deriv :/ 0 :/ 1)) "" [OutPoint (fakeTxHash 3) 0])
                      ]
                }
              2
              (Store.BlockRef 800001 1)
              2
              False
              ""
      (under fromColumns (spending 20000 30000), under fromColumns (spending 30000 20000))
        `shouldBe` ( Just [[TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", TextCell "/0/1", EmptyCell, SatoshiCell 30000, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, SatoshiCell 20000]],
                     Just [[TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, SatoshiCell 30000, TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", TextCell "/0/1", EmptyCell, SatoshiCell 20000]]
                   )

    -- Coins of 50000, 30000 and 20000 at /0/0, /0/1 and /0/2 pay 60000 to
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 30000 to
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ, with 9000 of change to /1/0. The
    -- sheet names two of the three it spends from and one of the two it
    -- pays: 1 + 1 = 2 others.
    it "names the address a payment pays the most, and counts the addresses that have no column: those it spends from past the second, and those it pays past the first" $ do
      let batch =
            spendSummary
              { summaryAmount = -91000,
                summaryMyInputs =
                  Map.fromList
                    [ (externalAddr 0, MyInputs 50000 (legacyPath (Deriv :/ 0 :/ 0)) "" [OutPoint (fakeTxHash 1) 0]),
                      (externalAddr 1, MyInputs 30000 (legacyPath (Deriv :/ 0 :/ 1)) "" [OutPoint (fakeTxHash 3) 0]),
                      (externalAddr 2, MyInputs 20000 (legacyPath (Deriv :/ 0 :/ 2)) "" [OutPoint (fakeTxHash 4) 0])
                    ],
                summaryOtherOutputs = Map.fromList [(otherAddr 0, 60000), (otherAddr 1, 30000)]
              }
      under (toColumns <> ["Other addresses"]) (onChain batch 2 (Store.BlockRef 800001 1) 2 False "")
        `shouldBe` Just [[TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, SatoshiCell 60000, CountCell 2]]

    -- /0/0 pays 40000 to /0/1 and 9000 to /1/0, and the portfolio loses
    -- the fee of 1000. Neither output can be told to be the change, as in
    -- the verbose view: /0/1 is paid the most, and /1/0 is the other
    -- address.
    it "gives a transaction between the addresses of the portfolio the address it pays the most and no change, and counts the other address it pays" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 1)) ""), (internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
      under (toColumns <> changeColumns <> ["Other addresses"]) (onChain internal 2 (Store.BlockRef 800001 1) 2 False "")
        `shouldBe` Just [[TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", TextCell "/0/1", EmptyCell, SatoshiCell 40000, EmptyCell, EmptyCell, EmptyCell, CountCell 1]]

    -- The payment of 40000 goes to an address of portfolio1, labelled
    -- there. The sheet has no column for the portfolio of a path.
    it "gives an address of another portfolio of the wallet in full, with its value, without its path or its label there" $ do
      let toOther = TxInfo spendSummary (OnChain (ChainStatus (fakeTxHash 2) (Store.BlockRef 800001 1) (Just seenTime) 2 False [])) "" Map.empty (Map.fromList [(otherAddr 0, HeldElsewhere (PortfolioRef 0 1) (legacyPath (Deriv :/ 0 :/ 4)) "Holidays")])
      under toColumns toOther
        `shouldBe` Just [[TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, SatoshiCell 40000]]

    it "gives the lock time of a transaction that has one, the block it waits for" $
      under ["Lock time"] (onChain spendSummary {summaryLockTime = 800000} 2 (Store.BlockRef 800001 1) 2 False "")
        `shouldBe` Just [[CountCell 800000]]

    it "gives the transactions of each network after those of the network before, each with its network" $ do
      let onBch = Group (Place mainPortfolioRef bch) [AccountCounts Bip44 1 0] [ListedTx (onChain paymentSummary 3 (Store.BlockRef 900000 1) 1 False "") (Just 50000)]
      (cellsUnder ["Wallet", "Portfolio", "Network"] <$> cetSheet (ResponseTxs [mainGroup [ListedTx (onChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False "") (Just 50000)], onBch]))
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc"], [TextCell "wallet0", TextCell "portfolio0", TextCell "bch"]]

pendingSheetSpec :: Spec
pendingSheetSpec =
  describe "The sheet of pending transactions" $ do
    let prepared signedM online conflicted = TxInfo spendSummary (Pending (PendingStatus a1075db5First signedM online conflicted (Just seenTime) [])) "" Map.empty Map.empty
        sheetOfPending infos = cetSheet (ResponsePendingTxs Nothing [(mainPlace, info) | info <- infos])
        under headings infos = cellsUnder headings <$> sheetOfPending infos

    it "names its columns: those of txs in the same order, with both hashes first, without what the chain alone tells, and when the wallet started holding it last" $
      sheetHeadings <$> cetSheet (ResponsePendingTxs Nothing [])
        `shouldBe` Just
          [ "Wallet", "Portfolio", "Network", "Type",
            "Nosighash", "Short nosighash", "Txid", "Short txid",
            "Direction", "Amount (sats)", "Fee (sats)", "Fee rate (sat/vB)", "Size (vB)", "Change in fee (sats)",
            "Status", "Replaces",
            "Transaction label",
            "From 1 address", "From 1 path", "From 1 label", "From 1 value (sats)",
            "From 2 address", "From 2 path", "From 2 label", "From 2 value (sats)",
            "To address", "To path", "To label", "To value (sats)",
            "Change address", "Change path", "Change value (sats)",
            "Other addresses",
            "Lock time",
            "Created (UTC)", "Created (local)", "Created (offset)"
          ]

    -- The coin of 50000 at /0/0 pays 40000 to
    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 9000 of change to /1/0: 40000
    -- paid + 1000 of fee = 41000, at 4 sat/vB for 226 vB
    it "gives a transaction not signed a row: its account, its nosigHash in full and by its short id, no txid, its direction, amount and fee, its status, its addresses, and when it was created" $
      sheetRows <$> sheetOfPending [prepared Nothing False False]
        `shouldBe` Just
          [ [TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TextCell "bip44"]
              <> [TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5", EmptyCell, EmptyCell]
              -- The rate it was built for is unknown, so the change in its fee is too
              <> [TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, CountCell 226, EmptyCell]
              -- It replaces nothing, and has no label
              <> [TextCell "not signed", EmptyCell]
              <> [EmptyCell]
              -- From 1, and no From 2
              <> [TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, SatoshiCell 50000]
              <> [EmptyCell, EmptyCell, EmptyCell, EmptyCell]
              -- To, then the change
              <> [TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, SatoshiCell 40000]
              <> [TextCell "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", TextCell "/1/0", SatoshiCell 9000]
              -- No other address, no lock time
              <> [CountCell 0, EmptyCell]
              <> seenTimeCells
          ]

    it "gives a signed transaction its txid in full and by its short id next to its nosigHash" $
      under ["Status", "Nosighash", "Short nosighash", "Txid", "Short txid"] [prepared (Just b2d4f6a8Txid) False False]
        `shouldBe` Just [[TextCell "signed", TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5", TextCell "b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001", TextCell "b2d4f6a8"]]

    it "says a transaction sent is sent, and one in conflict conflicted" $
      under ["Status"] [prepared (Just b2d4f6a8Txid) True False, prepared (Just b2d4f6a8Txid) False True]
        `shouldBe` Just [[TextCell "sent"], [TextCell "conflicted"]]

    it "names the transaction a replacement of bumpfee replaces, by its txid in full" $ do
      let replacement = TxInfo spendSummary (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [fakeTxHash 2])) "" Map.empty Map.empty
      under ["Replaces"] [replacement]
        `shouldBe` Just [[TextCell "0000000000000000000000000000000000000000000000000000000000000002"]]

    -- The fee of 1000 holds 300 of change too small for an output
    it "gives the change the fee of a transaction took in, when hw knows the rate it was built for" $ do
      let withChange = TxInfo spendSummary {summaryChangeInFee = Just 300} (Pending (PendingStatus a1075db5First Nothing False False (Just seenTime) [])) "" Map.empty Map.empty
      under ["Fee (sats)", "Change in fee (sats)"] [withChange]
        `shouldBe` Just [[SatoshiCell 1000, SatoshiCell 300]]

    it "names the wallet and the portfolio of a transaction of another portfolio" $
      (cellsUnder ["Wallet", "Portfolio", "Network"] <$> cetSheet (ResponsePendingTxs Nothing [(Place (PortfolioRef 2 1) bch, prepared Nothing False False)]))
        `shouldBe` Just [[TextCell "wallet2", TextCell "portfolio1", TextCell "bch"]]

walletSheetSpec :: Spec
walletSheetSpec = do
  let ofPortfolio name path = Sheet ["Portfolio", "Path"] [[TextCell name, TextCell path]]
      sheetsOf sheet = PortfolioSheets sheet sheet sheet sheet

  describe "The sheets of the spreadsheet of the wallet" $ do
    it "are named after the six listings, in the order of the help" $
      map fst (walletSheets (Sheet [] []) [] (Sheet [] []))
        `shouldBe` ["Portfolios", "Addresses", "Balances", "Coins", "Transactions", "Pending transactions"]

    it "give the rows of each portfolio one portfolio after the other, under the headings of its listing, with no column added" $
      lookup "Addresses" (walletSheets (Sheet [] []) [sheetsOf (ofPortfolio "portfolio0" "/0/0"), sheetsOf (ofPortfolio "portfolio1" "/0/7")] (Sheet [] []))
        `shouldBe` Just (Sheet ["Portfolio", "Path"] [[TextCell "portfolio0", TextCell "/0/0"], [TextCell "portfolio1", TextCell "/0/7"]])

    it "keep the sheets of the portfolios and the pending transactions as the listings give them" $ do
      let portfolios = Sheet ["Wallet", "Portfolio"] [[TextCell "wallet0", TextCell "portfolio0"]]
          pendingSheet = Sheet ["Wallet", "Portfolio", "Status"] [[TextCell "wallet0", TextCell "portfolio0", TextCell "signed"]]
          sheets = walletSheets portfolios [sheetsOf (ofPortfolio "portfolio0" "/0/0")] pendingSheet
      (lookup "Portfolios" sheets, lookup "Pending transactions" sheets) `shouldBe` (Just portfolios, Just pendingSheet)

  describe "Sheets stacked" $ do
    it "are the rows of each one after the other, under the headings of the first" $
      stacked [ofPortfolio "portfolio0" "/0/1", Sheet ["Portfolio", "Path"] [], ofPortfolio "portfolio1" "/1/0"]
        `shouldBe` Sheet ["Portfolio", "Path"] [[TextCell "portfolio0", TextCell "/0/1"], [TextCell "portfolio1", TextCell "/1/0"]]

    it "are a sheet of no column when there are none" $
      stacked [] `shouldBe` Sheet [] []
