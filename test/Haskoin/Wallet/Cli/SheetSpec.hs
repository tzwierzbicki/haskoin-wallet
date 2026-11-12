{-# LANGUAGE OverloadedStrings #-}

-- | The sheets --csv and exportsheet write: the columns of each listing,
-- and the cells of its rows, with their kinds. A listing over a portfolio
-- covers several networks, and for the addresses and the coins both types:
-- a sheet has no sections, so the network and the type are columns.
module Haskoin.Wallet.Cli.SheetSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Response (AccountCounts (..), Group (..), Response (..), WalletNaming (..))
import Haskoin.Wallet.Cli.Sheet (Cell (..), PortfolioSheets (..), Sheet (..), listingSheet, stacked, walletSheets, withPortfolioColumns)
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Account (NetworkView (..), PortfolioView (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (AccountKind (..), legacyPath)
import Haskoin.Wallet.TestUtils
import Test.Hspec

-- Every time below is seenTime, 14 Nov 2023 22:13:20 UTC. The txid of
-- fakeTxHash n is 62 zeros and n in hexadecimal. The rows are those of
-- portfolio0 of wallet0 on btc, mainPlace, whose bip44 account has 12
-- receiving and 3 change addresses.

spec :: Spec
spec = do
  describe "The sheet of a listing" $
    it "is none for an answer that lists nothing, such as an error" $
      listingSheet (ResponseError "There are no portfolios in the wallet") `shouldBe` Nothing

  portfolioSheetSpec
  addressSheetSpec
  balanceSheetSpec
  coinSheetSpec
  txSheetSpec
  pendingSheetSpec
  walletSheetSpec

portfolioSheetSpec :: Spec
portfolioSheetSpec =
  describe "The sheet of portfolios" $ do
    it "names its columns: the wallet, the portfolio, whether it is the active one and its description, then the network, without the keys or the fingerprint of the seed" $
      sheetHeadings <$> listingSheet (ResponsePortfolios [])
        `shouldBe` Just ["Wallet", "Portfolio", "Active", "Description", "Network", "Spendable (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Coins", "Addresses", "Created (UTC)"]

    -- portfolio0 is the active portfolio. It holds 5000000 confirmed and
    -- 100000 unconfirmed on btc, in 3 coins, can spend 4900000 now, and has
    -- 17 addresses there.
    it "gives a portfolio a row on its network: its wallet, its name, yes for the active one, its description, the network, its amounts in satoshis, its coins and addresses, and when it was created" $
      sheetRows <$> listingSheet (ResponsePortfolios [mainPortfolioView])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "yes", TextCell "savings", TextCell "btc", SatoshiCell 4900000, SatoshiCell 5000000, SatoshiCell 100000, CountCell 3, CountCell 17, TimeCell seenTime]]

    it "says no in the column Active of a portfolio that is not the active one" $
      map (take 3) . sheetRows <$> listingSheet (ResponsePortfolios [mainPortfolioView {viewActive = False}])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "no"]]

    -- A whole portfolio has an account on eight networks
    it "gives a portfolio a row for each network it has an account on, in the order of its networks" $ do
      let onBch = NetworkView bch 0 0 0 0 0 []
          both = mainPortfolioView {viewNetworks = viewNetworks mainPortfolioView <> [onBch]}
      sheetRows <$> listingSheet (ResponsePortfolios [both])
        `shouldBe` Just
          [ [TextCell "wallet0", TextCell "portfolio0", TextCell "yes", TextCell "savings", TextCell "btc", SatoshiCell 4900000, SatoshiCell 5000000, SatoshiCell 100000, CountCell 3, CountCell 17, TimeCell seenTime],
            [TextCell "wallet0", TextCell "portfolio0", TextCell "yes", TextCell "savings", TextCell "bch", SatoshiCell 0, SatoshiCell 0, SatoshiCell 0, CountCell 0, CountCell 0, TimeCell seenTime]
          ]

    it "gives the rows of each portfolio after those of the one before, each named by its wallet and its index" $ do
      let other = mainPortfolioView {viewRef = PortfolioRef 2 1, viewActive = False, viewDescription = ""}
      map (take 5) . sheetRows <$> listingSheet (ResponsePortfolios [mainPortfolioView, other])
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "yes", TextCell "savings", TextCell "btc"], [TextCell "wallet2", TextCell "portfolio1", TextCell "no", EmptyCell, TextCell "btc"]]

addressSheetSpec :: Spec
addressSheetSpec =
  describe "The sheet of addresses" $ do
    it "names its columns, the network first and the type of the account second" $
      sheetHeadings <$> listingSheet (ResponseAddresses [])
        `shouldBe` Just ["Network", "Type", "Path", "Address", "Label", "Spendable (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Received (sats)", "Coins", "Transactions", "Created (UTC)"]

    -- /0/0 received 50000 in one transaction, and holds it in one coin
    it "gives an address a row: its network, its type, its path, the address, its label, its amounts in satoshis, its coins and transactions, and when it was created" $
      sheetRows <$> listingSheet (ResponseAddresses [mainGroup [(paidAddressRow, 50000)]])
        `shouldBe` Just [[TextCell "btc", TextCell "bip44", TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "Invoice 42", SatoshiCell 50000, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 50000, CountCell 1, CountCell 1, TimeCell seenTime]]

    it "leaves the label of an address without one empty" $
      sheetRows <$> listingSheet (ResponseAddresses [mainGroup [(unusedAddressRow, 0)]])
        `shouldBe` Just [[TextCell "btc", TextCell "bip44", TextCell "/0/1", TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", EmptyCell, SatoshiCell 0, SatoshiCell 0, SatoshiCell 0, SatoshiCell 0, CountCell 0, CountCell 0, TimeCell seenTime]]

    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is /0/0 of m/84'/0'/0':
    -- both accounts of the portfolio have an address at /0/0
    it "gives a native SegWit address the type bip84, which tells it from the bip44 address at the same path" $ do
      let segwitRow = unusedAddressRow {dBAddressIndex = 0, dBAddressAddress = "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"}
          groups = [mainGroup [(paidAddressRow, 50000)], Group mainPlace [AccountCounts Bip84 2 0] [(segwitRow, 0)]]
      map (take 4) . sheetRows <$> listingSheet (ResponseAddresses groups)
        `shouldBe` Just
          [ [TextCell "btc", TextCell "bip44", TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
            [TextCell "btc", TextCell "bip84", TextCell "/0/0", TextCell "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"]
          ]

    it "gives the addresses of each network after those of the network before, each with its network" $ do
      let bchRow = unusedAddressRow {dBAddressIndex = 0, dBAddressAddress = "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"}
          groups = [mainGroup [(paidAddressRow, 50000)], Group (Place mainPortfolioRef bch) [AccountCounts Bip44 1 0] [(bchRow, 0)]]
      map (take 4) . sheetRows <$> listingSheet (ResponseAddresses groups)
        `shouldBe` Just
          [ [TextCell "btc", TextCell "bip44", TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"],
            [TextCell "bch", TextCell "bip44", TextCell "/0/0", TextCell "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"]
          ]

balanceSheetSpec :: Spec
balanceSheetSpec =
  describe "The sheet of balances" $ do
    let total = CoinBalance 80000 50000 30000 0

    it "names its columns, the network and the type first, the value the address cannot spend in three parts" $
      sheetHeadings <$> listingSheet (ResponseBalances [])
        `shouldBe` Just ["Network", "Type", "Path", "Address", "Label", "Total (sats)", "Spendable (sats)", "Locked by tx (sats)", "Locked by you (sats)", "Not spendable yet (sats)", "Coins"]

    -- 80000 in all, 50000 spendable, 30000 locked by a pending
    -- transaction: 80000 - 50000 - 30000 = 0 not spendable yet
    it "gives an address a row: its network, its type, its path, the address, its label, its amounts in satoshis, and its coins" $
      sheetRows <$> listingSheet (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 30000 0)], total)])
        `shouldBe` Just [[TextCell "btc", TextCell "bip44", TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "Invoice 42", SatoshiCell 80000, SatoshiCell 50000, SatoshiCell 30000, SatoshiCell 0, SatoshiCell 0, CountCell 1]]

    -- 80000 - 50000 spendable - 10000 locked by the user = 20000
    it "gives as not spendable yet what the address cannot spend that nothing locks" $
      sheetRows <$> listingSheet (ResponseBalances [(mainGroup [(paidAddressRow, CoinBalance 80000 50000 0 10000)], total)])
        `shouldBe` Just [[TextCell "btc", TextCell "bip44", TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "Invoice 42", SatoshiCell 80000, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 10000, SatoshiCell 20000, CountCell 1]]

coinSheetSpec :: Spec
coinSheetSpec =
  describe "The sheet of coins" $ do
    let rowsOf coin = sheetRows <$> listingSheet (ResponseCoins [mainGroup [coin]])

    it "names its columns, the network first, and the type of the account of the address" $
      sheetHeadings <$> listingSheet (ResponseCoins [])
        `shouldBe` Just ["Network", "Coin", "Short coin", "Time (UTC)", "Value (sats)", "State", "Confirmations", "Block", "Address", "Type", "Path", "Address label", "Transaction label"]

    -- spendableCoinRow is output 0 of transaction 1, whose txid starts
    -- with 00000000
    it "gives a coin a row: its network, the coin in full and by its short id, the time of its transaction, its value in satoshis, its state, its confirmations and block, then its address, type and path" $
      rowsOf spendableCoinRow
        `shouldBe` Just [[TextCell "btc", TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TextCell "00000000:0", TimeCell seenTime, SatoshiCell 50000, TextCell "spendable", CountCell 3, CountCell 800000, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "bip44", TextCell "/0/0", EmptyCell, EmptyCell]]

    it "gives the short id of a coin as the short id of its txid, a colon and its index" $
      (map (take 2 . drop 1) <$> rowsOf spendableCoinRow {jsonCoinOutpoint = OutPoint a1075db5First 1})
        `shouldBe` Just [[TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001:1", TextCell "a1075db5:1"]]

    it "says a coin a pending transaction spends is locked by a pending transaction" $
      (map (take 1 . drop 5) <$> rowsOf spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False})
        `shouldBe` Just [[TextCell "locked by a pending transaction"]]

    it "leaves the block of an unconfirmed coin empty" $
      rowsOf spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False}
        `shouldBe` Just [[TextCell "btc", TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TextCell "00000000:0", TimeCell seenTime, SatoshiCell 50000, TextCell "not spendable yet", CountCell 0, EmptyCell, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "bip44", TextCell "/0/0", EmptyCell, EmptyCell]]

    it "gives the label of the address and that of the transaction, each in its column" $
      (map (drop 11) <$> rowsOf spendableCoinRow {jsonCoinAddressLabel = "Invoice 42", jsonCoinTxLabel = "Salary"})
        `shouldBe` Just [[TextCell "Invoice 42", TextCell "Salary"]]

    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc is /0/0 of m/84'/0'/0'
    it "gives a coin of the bip84 account the type bip84" $
      (map (take 3 . drop 8) <$> rowsOf spendableCoinRow {jsonCoinAddress = segwitExternalAddr 0, jsonCoinPath = Just (segwitPath (Deriv :/ 0 :/ 0))})
        `shouldBe` Just [[TextCell "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", TextCell "bip84", TextCell "/0/0"]]

txSheetSpec :: Spec
txSheetSpec =
  describe "The sheet of txs" $ do
    let onChain summary txid block confirmations dead label =
          TxInfo summary (OnChain (ChainStatus (fakeTxHash txid) block (Just seenTime) confirmations dead)) label Map.empty Map.empty
        rowsOf txs = sheetRows <$> listingSheet (ResponseTxs [mainGroup txs])

    it "names its columns, the network first, and the types of the addresses of the portfolio a transaction involves" $
      sheetHeadings <$> listingSheet (ResponseTxs [])
        `shouldBe` Just ["Network", "Time (UTC)", "Type", "Address types", "Amount (sats)", "Fee (sats)", "Fee rate (sat/vB)", "Balance (sats)", "Status", "Confirmations", "Block", "Txid", "Short txid", "Label", "Address", "Path", "Address label", "Other addresses"]

    -- The payment received pays 50000 at /0/0 of the bip44 account, and
    -- its payer paid 1000 of fee, at 4 sat/vB. The txids of fakeTxHash all
    -- start with 00000000.
    it "gives a payment received a row: its network, time, type, the types it involves, amount, fee and balance, where it stands, its txid in full and by its short id, and its label, then the address it was paid at" $
      rowsOf [ListedTx (onChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False "Salary") (Just 50000)]
        `shouldBe` Just [[TextCell "btc", TimeCell seenTime, TextCell "Incoming", TextCell "bip44", SatoshiCell 50000, SatoshiCell 1000, CountCell 4, SatoshiCell 50000, TextCell "confirmed", CountCell 3, CountCell 800000, TextCell "0000000000000000000000000000000000000000000000000000000000000001", TextCell "00000000", TextCell "Salary", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, CountCell 0]]

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
      (map (take 4) <$> rowsOf [ListedTx (onChain toSegwit 2 (Store.BlockRef 800001 1) 2 False "") (Just 49000)])
        `shouldBe` Just [[TextCell "btc", TimeCell seenTime, TextCell "Internal", TextCell "bip44, bip84"]]

    it "gives the short id of the txid, its first 8 digits, after the txid" $
      (map (take 2 . drop 11) <$> rowsOf [ListedTx (TxInfo spendSummary (OnChain (ChainStatus a1075db5First (Store.BlockRef 800001 1) (Just seenTime) 2 False)) "" Map.empty Map.empty) (Just 9000)])
        `shouldBe` Just [[TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5"]]

    -- 40000 paid + 1000 of fee = 41000 leave the portfolio, which keeps 9000
    it "gives a payment the address of the other wallet it pays, with no path" $
      rowsOf [ListedTx (onChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") (Just 9000)]
        `shouldBe` Just [[TextCell "btc", TimeCell seenTime, TextCell "Outgoing", TextCell "bip44", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, SatoshiCell 9000, TextCell "confirmed", CountCell 2, CountCell 800001, TextCell "0000000000000000000000000000000000000000000000000000000000000002", TextCell "00000000", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    it "gives the label of the address a payment was received at in its own column" $
      (map (drop 13) <$> rowsOf [ListedTx (onChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False "") (Just 50000)])
        `shouldBe` Just [[EmptyCell, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "Rent", CountCell 0]]

    it "says an unconfirmed transaction is unconfirmed, with 0 confirmations and no block" $
      (map (take 3 . drop 8) <$> rowsOf [ListedTx (onChain spendSummary 2 (Store.MemRef 1700000000) 0 False "") (Just 9000)])
        `shouldBe` Just [[TextCell "unconfirmed", CountCell 0, EmptyCell]]

    -- The balance of the portfolio leaves a dead transaction out
    it "says a dead transaction is dead, and leaves its balance empty" $
      (map (take 2 . drop 7) <$> rowsOf [ListedTx (onChain spendSummary 2 (Store.MemRef 1700000000) 0 True "") Nothing])
        `shouldBe` Just [[EmptyCell, TextCell "dead"]]

    -- /0/0 pays 40000 to /0/1 and 9000 to /1/0: /0/1 is paid the most,
    -- and the portfolio loses the fee of 1000
    it "gives the address a transaction pays the most, and counts the other addresses it pays" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (legacyPath (Deriv :/ 0 :/ 1)) ""), (internalAddr 0, MyOutputs 9000 (legacyPath (Deriv :/ 1 :/ 0)) "")]
              }
      (map (drop 14) <$> rowsOf [ListedTx (onChain internal 2 (Store.BlockRef 800001 1) 2 False "") (Just 49000)])
        `shouldBe` Just [[TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", TextCell "/0/1", EmptyCell, CountCell 1]]

    it "gives the transactions of each network after those of the network before, each with its network" $ do
      let onBch = Group (Place mainPortfolioRef bch) [AccountCounts Bip44 1 0] [ListedTx (onChain paymentSummary 3 (Store.BlockRef 900000 1) 1 False "") (Just 50000)]
      (map (take 1) . sheetRows <$> listingSheet (ResponseTxs [mainGroup [ListedTx (onChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False "") (Just 50000)], onBch]))
        `shouldBe` Just [[TextCell "btc"], [TextCell "bch"]]

pendingSheetSpec :: Spec
pendingSheetSpec =
  describe "The sheet of pending transactions" $ do
    let prepared signedM online conflicted = TxInfo spendSummary (Pending (PendingStatus a1075db5First signedM online conflicted (Just seenTime) [])) "" Map.empty Map.empty
        rowsOf infos = sheetRows <$> listingSheet (ResponsePendingTxs WithoutWallets Nothing [(mainPlace, info) | info <- infos])

    it "names its columns, the wallet, the portfolio and the network first" $
      sheetHeadings <$> listingSheet (ResponsePendingTxs WithoutWallets Nothing [])
        `shouldBe` Just ["Wallet", "Portfolio", "Network", "Created (UTC)", "Type", "Address types", "Amount (sats)", "Fee (sats)", "Fee rate (sat/vB)", "Status", "Nosighash", "Short nosighash", "Txid", "Short txid", "Label", "Address", "Path", "Address label", "Other addresses"]

    -- 40000 paid + 1000 of fee = 41000, at 4 sat/vB
    it "gives a transaction not signed a row: where it is, when it was created, its type, the types it involves, amount and fee, its status and nosigHash in full and by its short id, with no txid, then what it is for" $
      rowsOf [prepared Nothing False False]
        `shouldBe` Just [[TextCell "wallet0", TextCell "portfolio0", TextCell "btc", TimeCell seenTime, TextCell "Outgoing", TextCell "bip44", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, TextCell "not signed", TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5", EmptyCell, EmptyCell, EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    it "gives a signed transaction its txid in full and by its short id next to its nosigHash" $
      (map (take 5 . drop 9) <$> rowsOf [prepared (Just b2d4f6a8Txid) False False])
        `shouldBe` Just [[TextCell "signed", TextCell "a1075db5c0ffee00000000000000000000000000000000000000000000000001", TextCell "a1075db5", TextCell "b2d4f6a8c0ffee00000000000000000000000000000000000000000000000001", TextCell "b2d4f6a8"]]

    it "says a transaction sent is sent, and one in conflict conflicted" $
      (map (take 1 . drop 9) <$> rowsOf [prepared (Just b2d4f6a8Txid) True False, prepared (Just b2d4f6a8Txid) False True])
        `shouldBe` Just [[TextCell "sent"], [TextCell "conflicted"]]

    it "names the wallet and the portfolio of a transaction of another portfolio" $
      (map (take 3) . sheetRows <$> listingSheet (ResponsePendingTxs WithoutWallets Nothing [(Place (PortfolioRef 2 1) bch, prepared Nothing False False)]))
        `shouldBe` Just [[TextCell "wallet2", TextCell "portfolio1", TextCell "bch"]]

walletSheetSpec :: Spec
walletSheetSpec = do
  let paths path = Sheet ["Path"] [[TextCell path]]
      sheetsOf sheet = PortfolioSheets sheet sheet sheet sheet
      other = PortfolioRef 2 1

  describe "The sheets of the spreadsheet of the wallet" $ do
    it "are named after the six listings, in the order of the help" $
      map fst (walletSheets (Sheet [] []) [] (Sheet [] []))
        `shouldBe` ["Portfolios", "Addresses", "Balances", "Coins", "Transactions", "Pending transactions"]

    it "give the rows of each portfolio under two first columns that name its wallet and the portfolio, one portfolio after the other" $
      lookup "Addresses" (walletSheets (Sheet [] []) [(mainPortfolioRef, sheetsOf (paths "/0/0")), (other, sheetsOf (paths "/0/7"))] (Sheet [] []))
        `shouldBe` Just (Sheet ["Wallet", "Portfolio", "Path"] [[TextCell "wallet0", TextCell "portfolio0", TextCell "/0/0"], [TextCell "wallet2", TextCell "portfolio1", TextCell "/0/7"]])

    it "keep the sheets of the portfolios and the pending transactions as the listings give them, since they name their portfolios" $ do
      let portfolios = Sheet ["Wallet", "Portfolio"] [[TextCell "wallet0", TextCell "portfolio0"]]
          pendingSheet = Sheet ["Wallet", "Portfolio", "Status"] [[TextCell "wallet0", TextCell "portfolio0", TextCell "signed"]]
          sheets = walletSheets portfolios [(mainPortfolioRef, sheetsOf (paths "/0/0"))] pendingSheet
      (lookup "Portfolios" sheets, lookup "Pending transactions" sheets) `shouldBe` (Just portfolios, Just pendingSheet)

  describe "A sheet with the columns of its portfolio" $
    it "starts every row with the names of the wallet and of the portfolio, under the headings Wallet and Portfolio" $
      withPortfolioColumns other (Sheet ["Path", "Coins"] [[TextCell "/0/0", CountCell 1], [TextCell "/0/1", CountCell 0]])
        `shouldBe` Sheet ["Wallet", "Portfolio", "Path", "Coins"] [[TextCell "wallet2", TextCell "portfolio1", TextCell "/0/0", CountCell 1], [TextCell "wallet2", TextCell "portfolio1", TextCell "/0/1", CountCell 0]]

  describe "Sheets stacked" $ do
    it "are the rows of each one after the other, under the headings of the first" $
      stacked [paths "/0/1", Sheet ["Path"] [], paths "/1/0"]
        `shouldBe` Sheet ["Path"] [[TextCell "/0/1"], [TextCell "/1/0"]]

    it "are a sheet of no column when there are none" $
      stacked [] `shouldBe` Sheet [] []
