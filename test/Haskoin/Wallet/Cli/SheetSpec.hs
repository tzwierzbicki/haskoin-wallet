{-# LANGUAGE OverloadedStrings #-}

-- | The sheets --csv writes: the columns of each listing, and the cells of
-- its rows, with their kinds
module Haskoin.Wallet.Cli.SheetSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Cli.Sheet (AccountSheets (..), Cell (..), Sheet (..), listingSheet, stacked, walletSheets, withAccountColumn)
import Haskoin.Wallet.Database (DBAccount (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.TestUtils
import Test.Hspec

-- Every time below is seenTime, 14 Nov 2023 22:13:20 UTC. The txid of
-- fakeTxHash n is 62 zeros and n in hexadecimal.

spec :: Spec
spec = do
  describe "The sheet of a listing" $
    it "is none for an answer that lists nothing, such as an error" $
      listingSheet (ResponseError "There are no accounts in the wallet") `shouldBe` Nothing

  accountSheetSpec
  addressSheetSpec
  balanceSheetSpec
  coinSheetSpec
  txSheetSpec
  pendingSheetSpec
  walletSheetSpec

accountSheetSpec :: Spec
accountSheetSpec =
  describe "The sheet of accounts" $ do
    it "names its columns, without the fingerprint of the seed" $
      sheetHeadings <$> listingSheet (ResponseAccounts [])
        `shouldBe` Just ["Account", "Network", "Path", "Spendable (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Coins", "Receiving addresses", "Change addresses", "Created (UTC)"]

    -- main holds 5000000 confirmed and 100000 unconfirmed in 3 coins, and
    -- has 12 receiving and 3 change addresses
    it "gives an account a row: its name, network and path, its amounts in satoshis, its coins and addresses, and when it was created" $
      sheetRows <$> listingSheet (ResponseAccounts [(mainAccountRow, 4900000)])
        `shouldBe` Just [[TextCell "main", TextCell "btc", TextCell "/44'/0'/0'", SatoshiCell 4900000, SatoshiCell 5000000, SatoshiCell 100000, CountCell 3, CountCell 12, CountCell 3, TimeCell seenTime]]

addressSheetSpec :: Spec
addressSheetSpec =
  describe "The sheet of addresses" $ do
    it "names its columns" $
      sheetHeadings <$> listingSheet (ResponseAddresses mainAccountRow [])
        `shouldBe` Just ["Path", "Address", "Label", "Spendable (sats)", "Confirmed (sats)", "Unconfirmed (sats)", "Received (sats)", "Coins", "Transactions", "Created (UTC)"]

    -- /0/0 received 50000 in one transaction, and holds it in one coin
    it "gives an address a row: its path, the address, its label, its amounts in satoshis, its coins and transactions, and when it was created" $
      sheetRows <$> listingSheet (ResponseAddresses mainAccountRow [(paidAddressRow, 50000)])
        `shouldBe` Just [[TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "Invoice 42", SatoshiCell 50000, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 50000, CountCell 1, CountCell 1, TimeCell seenTime]]

    it "leaves the label of an address without one empty" $
      sheetRows <$> listingSheet (ResponseAddresses mainAccountRow [(unusedAddressRow, 0)])
        `shouldBe` Just [[TextCell "/0/1", TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", EmptyCell, SatoshiCell 0, SatoshiCell 0, SatoshiCell 0, SatoshiCell 0, CountCell 0, CountCell 0, TimeCell seenTime]]

balanceSheetSpec :: Spec
balanceSheetSpec =
  describe "The sheet of balances" $ do
    it "names its columns, the value the address cannot spend in three parts" $
      sheetHeadings <$> listingSheet (ResponseBalances mainAccountRow [])
        `shouldBe` Just ["Path", "Address", "Label", "Total (sats)", "Spendable (sats)", "Locked by tx (sats)", "Locked by you (sats)", "Not spendable yet (sats)", "Coins"]

    -- 80000 in all, 50000 spendable, 30000 locked by a pending
    -- transaction: 80000 - 50000 - 30000 = 0 not spendable yet
    it "gives an address a row: its path, the address, its label, its amounts in satoshis, and its coins" $
      sheetRows <$> listingSheet (ResponseBalances mainAccountRow [(paidAddressRow, CoinBalance 80000 50000 30000 0)])
        `shouldBe` Just [[TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "Invoice 42", SatoshiCell 80000, SatoshiCell 50000, SatoshiCell 30000, SatoshiCell 0, SatoshiCell 0, CountCell 1]]

    -- 80000 - 50000 spendable - 10000 locked by the user = 20000
    it "gives as not spendable yet what the address cannot spend that nothing locks" $
      sheetRows <$> listingSheet (ResponseBalances mainAccountRow [(paidAddressRow, CoinBalance 80000 50000 0 10000)])
        `shouldBe` Just [[TextCell "/0/0", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "Invoice 42", SatoshiCell 80000, SatoshiCell 50000, SatoshiCell 0, SatoshiCell 10000, SatoshiCell 20000, CountCell 1]]

coinSheetSpec :: Spec
coinSheetSpec =
  describe "The sheet of coins" $ do
    let rowsOf coin = sheetRows <$> listingSheet (ResponseCoins mainAccountRow [coin])

    it "names its columns" $
      sheetHeadings <$> listingSheet (ResponseCoins mainAccountRow [])
        `shouldBe` Just ["Coin", "Time (UTC)", "Value (sats)", "State", "Confirmations", "Block", "Address", "Path", "Address label", "Transaction label"]

    it "gives a coin a row: the coin, the time of its transaction, its value in satoshis, its state, its confirmations and block, then its address and path" $
      rowsOf spendableCoinRow
        `shouldBe` Just [[TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TimeCell seenTime, SatoshiCell 50000, TextCell "spendable", CountCell 3, CountCell 800000, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, EmptyCell]]

    it "says a coin a pending transaction spends is locked by a pending transaction" $
      rowsOf spendableCoinRow {jsonCoinLock = Just LockedByTransaction, jsonCoinSpendable = False}
        `shouldBe` Just [[TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TimeCell seenTime, SatoshiCell 50000, TextCell "locked by a pending transaction", CountCell 3, CountCell 800000, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, EmptyCell]]

    it "leaves the block of an unconfirmed coin empty" $
      rowsOf spendableCoinRow {jsonCoinBlock = Store.MemRef 1700000000, jsonCoinConfirmations = 0, jsonCoinSpendable = False}
        `shouldBe` Just [[TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TimeCell seenTime, SatoshiCell 50000, TextCell "not spendable yet", CountCell 0, EmptyCell, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, EmptyCell]]

    it "gives the label of the address and that of the transaction, each in its column" $
      rowsOf spendableCoinRow {jsonCoinAddressLabel = "Invoice 42", jsonCoinTxLabel = "Salary"}
        `shouldBe` Just [[TextCell "0000000000000000000000000000000000000000000000000000000000000001:0", TimeCell seenTime, SatoshiCell 50000, TextCell "spendable", CountCell 3, CountCell 800000, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "Invoice 42", TextCell "Salary"]]

txSheetSpec :: Spec
txSheetSpec =
  describe "The sheet of txs" $ do
    let onChain summary txid block confirmations dead label =
          TxInfo summary (OnChain (ChainStatus (fakeTxHash txid) block (Just seenTime) confirmations dead)) label Map.empty
        rowsOf txs = sheetRows <$> listingSheet (ResponseTxs mainAccountRow txs)

    it "names its columns" $
      sheetHeadings <$> listingSheet (ResponseTxs mainAccountRow [])
        `shouldBe` Just ["Time (UTC)", "Type", "Amount (sats)", "Fee (sats)", "Fee rate (sat/vB)", "Balance (sats)", "Status", "Confirmations", "Block", "Txid", "Label", "Address", "Path", "Address label", "Other addresses"]

    -- The payment received pays 50000 at /0/0, and its payer paid 1000 of
    -- fee, at 4 sat/vB
    it "gives a payment received a row: its time, type, amount, fee and balance, where it stands, its txid and label, then the address it was paid at" $
      rowsOf [ListedTx (onChain paymentSummary 1 (Store.BlockRef 800000 1) 3 False "Salary") (Just 50000)]
        `shouldBe` Just [[TimeCell seenTime, TextCell "Incoming", SatoshiCell 50000, SatoshiCell 1000, CountCell 4, SatoshiCell 50000, TextCell "confirmed", CountCell 3, CountCell 800000, TextCell "0000000000000000000000000000000000000000000000000000000000000001", TextCell "Salary", TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", EmptyCell, CountCell 0]]

    -- 40000 paid + 1000 of fee = 41000 leave the account, which keeps 9000
    it "gives a payment the address of the other wallet it pays, with no path" $
      rowsOf [ListedTx (onChain spendSummary 2 (Store.BlockRef 800001 1) 2 False "") (Just 9000)]
        `shouldBe` Just [[TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, SatoshiCell 9000, TextCell "confirmed", CountCell 2, CountCell 800001, TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    it "gives the label of the address a payment was received at in its own column" $
      rowsOf [ListedTx (onChain paymentToRent 1 (Store.BlockRef 800000 1) 3 False "") (Just 50000)]
        `shouldBe` Just [[TimeCell seenTime, TextCell "Incoming", SatoshiCell 50000, SatoshiCell 1000, CountCell 4, SatoshiCell 50000, TextCell "confirmed", CountCell 3, CountCell 800000, TextCell "0000000000000000000000000000000000000000000000000000000000000001", EmptyCell, TextCell "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", TextCell "/0/0", TextCell "Rent", CountCell 0]]

    it "says an unconfirmed transaction is unconfirmed, with 0 confirmations and no block" $
      rowsOf [ListedTx (onChain spendSummary 2 (Store.MemRef 1700000000) 0 False "") (Just 9000)]
        `shouldBe` Just [[TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, SatoshiCell 9000, TextCell "unconfirmed", CountCell 0, EmptyCell, TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    -- The balance of the account leaves a dead transaction out
    it "says a dead transaction is dead, and leaves its balance empty" $
      rowsOf [ListedTx (onChain spendSummary 2 (Store.MemRef 1700000000) 0 True "") Nothing]
        `shouldBe` Just [[TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, EmptyCell, TextCell "dead", CountCell 0, EmptyCell, TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    -- /0/0 pays 40000 to /0/1 and 9000 to /1/0: /0/1 is paid the most,
    -- and the account loses the fee of 1000
    it "gives the address a transaction pays the most, and counts the other addresses it pays" $ do
      let internal =
            spendSummary
              { summaryType = TxInternal,
                summaryAmount = -1000,
                summaryOtherOutputs = Map.empty,
                summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 40000 (Deriv :/ 0 :/ 1) ""), (internalAddr 0, MyOutputs 9000 (Deriv :/ 1 :/ 0) "")]
              }
      rowsOf [ListedTx (onChain internal 2 (Store.BlockRef 800001 1) 2 False "") (Just 49000)]
        `shouldBe` Just [[TimeCell seenTime, TextCell "Internal", SatoshiCell (-1000), SatoshiCell 1000, CountCell 4, SatoshiCell 49000, TextCell "confirmed", CountCell 2, CountCell 800001, TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", TextCell "/0/1", EmptyCell, CountCell 1]]

pendingSheetSpec :: Spec
pendingSheetSpec =
  describe "The sheet of pending transactions" $ do
    let prepared signedM online conflicted = TxInfo spendSummary (Pending (PendingStatus (fakeTxHash 7) signedM online conflicted (Just seenTime) [])) "" Map.empty
        rowsOf infos = sheetRows <$> listingSheet (ResponsePendingTxs Nothing [(mainAccountRow, info) | info <- infos])

    it "names its columns, the account first" $
      sheetHeadings <$> listingSheet (ResponsePendingTxs Nothing [])
        `shouldBe` Just ["Account", "Created (UTC)", "Type", "Amount (sats)", "Fee (sats)", "Fee rate (sat/vB)", "Status", "Nosighash", "Txid", "Label", "Address", "Path", "Address label", "Other addresses"]

    -- 40000 paid + 1000 of fee = 41000, at 4 sat/vB
    it "gives a transaction not signed a row: its account, when it was created, its type, amount and fee, its status and nosigHash, with no txid, then what it is for" $
      rowsOf [prepared Nothing False False]
        `shouldBe` Just [[TextCell "main", TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, TextCell "not signed", TextCell "0000000000000000000000000000000000000000000000000000000000000007", EmptyCell, EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    it "gives a signed transaction its txid next to its nosigHash" $
      rowsOf [prepared (Just (fakeTxHash 2)) False False]
        `shouldBe` Just [[TextCell "main", TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, TextCell "signed", TextCell "0000000000000000000000000000000000000000000000000000000000000007", TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]]

    it "says a transaction sent is sent, and one in conflict conflicted" $
      rowsOf [prepared (Just (fakeTxHash 2)) True False, prepared (Just (fakeTxHash 2)) False True]
        `shouldBe` Just
          [ [TextCell "main", TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, TextCell "sent", TextCell "0000000000000000000000000000000000000000000000000000000000000007", TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0],
            [TextCell "main", TimeCell seenTime, TextCell "Outgoing", SatoshiCell (-41000), SatoshiCell 1000, CountCell 4, TextCell "conflicted", TextCell "0000000000000000000000000000000000000000000000000000000000000007", TextCell "0000000000000000000000000000000000000000000000000000000000000002", EmptyCell, TextCell "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", EmptyCell, EmptyCell, CountCell 0]
          ]

walletSheetSpec :: Spec
walletSheetSpec = do
  let savings = mainAccountRow {dBAccountName = "savings", dBAccountIndex = 1}
      paths path = Sheet ["Path"] [[TextCell path]]
      sheetsOf sheet = AccountSheets sheet sheet sheet sheet

  describe "The sheets of the spreadsheet of the wallet" $ do
    it "are named after the six listings, in the order of the help" $
      map fst (walletSheets (Sheet [] []) [] (Sheet [] []))
        `shouldBe` ["Accounts", "Addresses", "Balances", "Coins", "Transactions", "Pending transactions"]

    it "give the rows of each account under a first column that names it, one account after the other" $
      lookup "Addresses" (walletSheets (Sheet [] []) [(mainAccountRow, sheetsOf (paths "/0/0")), (savings, sheetsOf (paths "/0/7"))] (Sheet [] []))
        `shouldBe` Just (Sheet ["Account", "Path"] [[TextCell "main", TextCell "/0/0"], [TextCell "savings", TextCell "/0/7"]])

    it "keep the sheets of the accounts and the pending transactions as the listings give them, since they name their accounts" $ do
      let accounts = Sheet ["Account"] [[TextCell "main"]]
          pendingSheet = Sheet ["Account", "Status"] [[TextCell "main", TextCell "signed"]]
          sheets = walletSheets accounts [(mainAccountRow, sheetsOf (paths "/0/0"))] pendingSheet
      (lookup "Accounts" sheets, lookup "Pending transactions" sheets) `shouldBe` (Just accounts, Just pendingSheet)

  describe "A sheet with the column of its account" $
    it "starts every row with the name of the account, under the heading Account" $
      withAccountColumn mainAccountRow (Sheet ["Path", "Coins"] [[TextCell "/0/0", CountCell 1], [TextCell "/0/1", CountCell 0]])
        `shouldBe` Sheet ["Account", "Path", "Coins"] [[TextCell "main", TextCell "/0/0", CountCell 1], [TextCell "main", TextCell "/0/1", CountCell 0]]

  describe "Sheets stacked" $ do
    it "are the rows of each one after the other, under the headings of the first" $
      stacked [paths "/0/1", Sheet ["Path"] [], paths "/1/0"]
        `shouldBe` Sheet ["Path"] [[TextCell "/0/1"], [TextCell "/1/0"]]

    it "are a sheet of no column when there are none" $
      stacked [] `shouldBe` Sheet [] []
