{-# LANGUAGE OverloadedStrings #-}

-- | Tags as the categories of the journal for ledger: which names are
-- accounts of a journal, and which account a posting goes to when tags
-- stand for accounts
module Haskoin.Wallet.JournalAccountsSpec (spec) where

import qualified Data.Map.Strict as Map
import Haskoin.Wallet.JournalAccounts
import Haskoin.Wallet.TestUtils (coldTag, kycTag, rentTag, salaryTag)
import Test.Hspec

-- | The table of the tests: rent and cold stand for accounts of
-- expenses, and salary for one of income. kyc stands for none.
table :: TagAccounts
table =
  Map.fromList
    [ (rentTag, "Expenses:Housing:Rent"),
      (coldTag, "Expenses:Housing:Heating"),
      (salaryTag, "Income:Salary")
    ]

spec :: Spec
spec = do
  describe "The name of an account of the journal" $ do
    it "is names with a colon between them, taken as written" $
      readAccountName "Expenses:Housing:Rent" `shouldBe` Right "Expenses:Housing:Rent"

    it "may be one name, without a colon" $
      readAccountName "Rent" `shouldBe` Right "Rent"

    it "may have single spaces, accents and digits in a name" $
      readAccountName "Dépenses:Loyer de 2023" `shouldBe` Right "Dépenses:Loyer de 2023"

    it "may have the signs - _ & ' . / +" $
      readAccountName "Expenses:Food & Drink:Kid's school/R+D_U.S.-tax" `shouldBe` Right "Expenses:Food & Drink:Kid's school/R+D_U.S.-tax"

    it "cannot be empty" $
      readAccountName "" `shouldBe` Left "it is empty"

    it "cannot have nothing between two colons" $
      readAccountName "Expenses::Rent" `shouldBe` Left "a name is missing before or after a colon"

    it "cannot end with a colon" $
      readAccountName "Expenses:Rent:" `shouldBe` Left "a name is missing before or after a colon"

    -- ledger reads what follows two spaces as the amount of the posting
    it "cannot have two spaces in a row, which end the name of an account in ledger" $
      readAccountName "Expenses:Housing  Rent" `shouldBe` Left "it has two spaces in a row, which end the name of an account in ledger"

    it "cannot have a space right after a colon" $
      readAccountName "Expenses: Rent" `shouldBe` Left "it has a space at the start or at the end of a name"

    -- What follows a semicolon would be read as a note of the posting
    it "cannot have a semicolon, which starts a note in ledger" $
      readAccountName "Expenses:Rent;forged"
        `shouldBe` Left "it has the character ';'. An account is made of letters, digits, spaces and the signs - _ & ' . / +, with a colon between its names"

    it "cannot have a line break, which would start a new line of the journal" $
      readAccountName "Expenses:Rent\nAssets:Bank"
        `shouldBe` Left "it has the character '\\n'. An account is made of letters, digits, spaces and the signs - _ & ' . / +, with a colon between its names"

    -- ledger leaves a posting in parentheses out of the balance of its entry
    it "cannot be in parentheses" $
      readAccountName "(Expenses:Rent)"
        `shouldBe` Left "it has the character '('. An account is made of letters, digits, spaces and the signs - _ & ' . / +, with a colon between its names"

    it "cannot start with a digit" $
      readAccountName "2023:Rent" `shouldBe` Left "an account starts with a letter"

    -- The journal ends with the balance each portfolio must have
    it "cannot be the account of a portfolio, whose balance the journal states" $
      readAccountName "Assets:hw:wallet0:portfolio0"
        `shouldBe` Left "the accounts under Assets:hw are those of the portfolios, whose balances the journal states"

    it "cannot be the account all the portfolios are under" $
      readAccountName "Assets:hw"
        `shouldBe` Left "the accounts under Assets:hw are those of the portfolios, whose balances the journal states"

    it "may be another account of assets, even one whose name starts with hw" $
      readAccountName "Assets:hwallet" `shouldBe` Right "Assets:hwallet"

  describe "What tags say of the account of a posting" $ do
    it "is the account of a tag the table has" $
      filingOf table [rentTag] `shouldBe` FiledUnder "Expenses:Housing:Rent"

    it "is nothing when the table has none of the tags" $
      filingOf table [kycTag] `shouldBe` NotFiled

    it "is nothing without a tag" $
      filingOf table [] `shouldBe` NotFiled

    it "is the account of the one tag the table has, among other tags" $
      filingOf table [kycTag, salaryTag] `shouldBe` FiledUnder "Income:Salary"

    -- hw cannot tell which account is meant, nor split the amount
    it "is disputed when two tags stand for two accounts" $
      filingOf table [coldTag, rentTag] `shouldBe` Disputed

    it "is one account when two tags stand for the same one" $
      filingOf (Map.fromList [(rentTag, "Expenses:Housing"), (coldTag, "Expenses:Housing")]) [coldTag, rentTag]
        `shouldBe` FiledUnder "Expenses:Housing"

    it "is nothing whatever the tags when the table is empty" $
      filingOf noTagAccounts [rentTag, salaryTag] `shouldBe` NotFiled

  -- The tags of the transaction are given first, then those of the
  -- contact paid
  describe "The account of a posting" $ do
    it "is the unknown account when no tag stands for an account" $
      filedAccount "Expenses:Unknown" [NotFiled, NotFiled] `shouldBe` "Expenses:Unknown"

    it "is the account the tags of the transaction stand for" $
      filedAccount "Expenses:Unknown" [FiledUnder "Expenses:Housing:Rent", NotFiled] `shouldBe` "Expenses:Housing:Rent"

    it "is the account the tags of the contact stand for, when those of the transaction stand for none" $
      filedAccount "Expenses:Unknown" [NotFiled, FiledUnder "Expenses:Housing:Rent"] `shouldBe` "Expenses:Housing:Rent"

    -- The deposit paid to the landlord is not a rent
    it "is the account of the transaction rather than that of the contact, when both have one" $
      filedAccount "Expenses:Unknown" [FiledUnder "Assets:Deposits", FiledUnder "Expenses:Housing:Rent"] `shouldBe` "Assets:Deposits"

    it "is the unknown account when the tags of the transaction are disputed, whatever the contact" $
      filedAccount "Expenses:Unknown" [Disputed, FiledUnder "Expenses:Housing:Rent"] `shouldBe` "Expenses:Unknown"

    it "is the unknown account when the tags of the contact are disputed" $
      filedAccount "Expenses:Unknown" [NotFiled, Disputed] `shouldBe` "Expenses:Unknown"
