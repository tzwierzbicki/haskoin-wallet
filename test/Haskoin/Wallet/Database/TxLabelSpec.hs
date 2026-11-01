{-# LANGUAGE OverloadedStrings #-}

-- | The labels of the transactions of an account on the network: set,
-- replaced and removed by the user, each account's own. The account is
-- account 0 of the test wallet, and the other one account 0 of the second
-- test wallet. No summary is stored: a transaction just sent has a label
-- before the sync stores its summary.
module Haskoin.Wallet.Database.TxLabelSpec (spec) where

import Control.Monad.Trans (lift)
import Haskoin
import Haskoin.Wallet.Database.TxLabel
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx ->
  describe "The label of a transaction" $ do
    it "is none for a transaction the user never labelled" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        lift $ txLabel accId (fakeTxHash 1) `dbShouldBe` ""

    it "is the one the user set last" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        lift $ setTxLabel accId (fakeTxHash 1) "Rent"
        lift $ setTxLabel accId (fakeTxHash 1) "Rent of October"
        lift $ txLabel accId (fakeTxHash 1) `dbShouldBe` "Rent of October"

    it "is removed by an empty label, which leaves no row behind" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        lift $ setTxLabel accId (fakeTxHash 1) "Rent"
        lift $ setTxLabel accId (fakeTxHash 1) ""
        lift $ txLabel accId (fakeTxHash 1) `dbShouldBe` ""
        lift $ tableRows "d_b_tx_label" `dbShouldBe` []

    -- A payment from one account of the wallet to another is a transaction
    -- of both
    it "is each account's own, so that both ends of a payment between two accounts have theirs" $
      inDatabaseE $ do
        (mainId, _) <- testNewAcc ctx "main"
        (otherId, _) <- testNewAcc2 ctx "other"
        lift $ setTxLabel mainId (fakeTxHash 1) "To savings"
        lift $ setTxLabel otherId (fakeTxHash 1) "From checking"
        lift $ txLabel mainId (fakeTxHash 1) `dbShouldBe` "To savings"
        lift $ txLabel otherId (fakeTxHash 1) `dbShouldBe` "From checking"
