{-# LANGUAGE OverloadedStrings #-}

-- | The labels of the transactions of a portfolio on a network, a pool:
-- set, replaced and removed by the user, each pool's own. The pool is
-- what portfolio 0 of the test wallet holds on Bitcoin. No summary is
-- stored: a transaction just sent has a label before the sync stores its
-- summary.
module Haskoin.Wallet.Database.TxLabelSpec (spec) where

import Control.Monad.Trans (lift)
import Haskoin
import Haskoin.Wallet.Database.Portfolio (portfolioPool)
import Haskoin.Wallet.Database.TxLabel
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "The label of a transaction" $ do
    it "is none for a transaction the user never labelled" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ txLabel pool (fakeTxHash 1) `dbShouldBe` ""

    it "is the one the user set last" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ setTxLabel pool (fakeTxHash 1) "Rent"
        lift $ setTxLabel pool (fakeTxHash 1) "Rent of October"
        lift $ txLabel pool (fakeTxHash 1) `dbShouldBe` "Rent of October"

    it "is removed by an empty label, which leaves no row behind" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ setTxLabel pool (fakeTxHash 1) "Rent"
        lift $ setTxLabel pool (fakeTxHash 1) ""
        lift $ txLabel pool (fakeTxHash 1) `dbShouldBe` ""
        lift $ tableRows "d_b_tx_label" `dbShouldBe` []

    -- A payment from portfolio 0 to portfolio 1 of the wallet is a
    -- transaction of both
    it "is each portfolio's own, so that both ends of a payment between two portfolios have theirs" $
      inDatabaseE $ do
        payer <- testPool ctx
        paid <- (`portfolioPool` btc) <$> nextPortfolioOf ctx testMnemonic
        lift $ setTxLabel payer (fakeTxHash 1) "To savings"
        lift $ setTxLabel paid (fakeTxHash 1) "From checking"
        lift $ txLabel payer (fakeTxHash 1) `dbShouldBe` "To savings"
        lift $ txLabel paid (fakeTxHash 1) `dbShouldBe` "From checking"

    -- A transaction from before the fork of Bitcoin Cash has the same id
    -- on both chains
    it "is each network's own in one portfolio" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        lift $ setTxLabel (portfolioPool portfolio btc) (fakeTxHash 1) "Rent"
        lift $ txLabel (portfolioPool portfolio btc) (fakeTxHash 1) `dbShouldBe` "Rent"
        lift $ txLabel (portfolioPool portfolio bch) (fakeTxHash 1) `dbShouldBe` ""

    it "is listed with the others of its pool, in the order of the transaction ids, as exportwallet writes them" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        let pool = portfolioPool portfolio btc
        lift $ setTxLabel pool (fakeTxHash 2) "Rent"
        lift $ setTxLabel pool (fakeTxHash 1) "Salary"
        lift $ setTxLabel (portfolioPool portfolio bch) (fakeTxHash 3) "Elsewhere"
        lift $ poolTxLabels pool `dbShouldBe` [(fakeTxHash 1, "Salary"), (fakeTxHash 2, "Rent")]
