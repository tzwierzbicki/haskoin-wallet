{-# LANGUAGE OverloadedStrings #-}

-- | Tags as the categories of the journal for ledger: which account of
-- the journal a tag stands for, and which account a payment is posted to
-- when its tags stand for one.
--
-- = What it is for
--
-- hw knows what a transaction moved, and not what the money was for. So
-- the journal posts what goes to others to @Expenses:Unknown@, and what
-- comes from others to @Income:Unknown@ ("Haskoin.Wallet.Cli.Journal").
-- The user had to move each of these to an account of their own, with an
-- entry written by hand in their own journal, for every payment.
--
-- A user who tags their transactions has already said what the money was
-- for. A table of the configuration file, journal-accounts, gives a tag
-- an account, as in rent to @Expenses:Housing:Rent@
-- ("Haskoin.Wallet.Config"). A payment with that tag is then posted to
-- that account. The tags of a contact of the address book count too: one
-- tag on the landlord files every payment to the landlord, and one tag
-- on an employer files every payment that comes from it.
--
-- = Why the table is in the configuration file
--
-- The names of the accounts belong to the books of the user, not to the
-- wallet. A tag is of the database, and goes with it to another computer
-- in a file of labels. The accounts stay with the journal that includes
-- the file of hw. A user who never runs ledger then finds nothing of it
-- in the tags.
--
-- The cost is that a tag renamed or deleted keeps its old name in the
-- table. hw does not write tables in the configuration file
-- ("Haskoin.Wallet.TomlEdit"), so the user renames the line.
--
-- = One account for a posting
--
-- A posting goes to one account. Tags that stand for two accounts say
-- nothing hw can act on: it cannot tell which is meant, nor split the
-- amount. The posting then stays in the unknown account, where the user
-- sees it and sorts it out ('Disputed'). Picking the first tag in the
-- order of the alphabet was the other choice. It would file the payment
-- under an account without a word, and the mistake would look like any
-- other posting.
--
-- The tags of the transaction are asked first, then those of the contact
-- paid, or of the contacts a payment received came from ('filedAccount').
-- A tag on the transaction is about that one payment, and a tag on a
-- contact about every payment to it or from it. So the deposit paid to
-- the landlord, tagged as a deposit, is not filed as a rent.
--
-- A tag stands for the same account whichever way the money goes. A
-- payment received with the tag rent is posted to the account of the
-- rent, below zero: a refund lowers the expense, as accounting has it.
--
-- This module holds the rules that need no database and no file.
module Haskoin.Wallet.JournalAccounts
  ( -- * The table
    TagAccounts,
    noTagAccounts,

    -- * The name of an account
    readAccountName,
    portfoliosAccount,

    -- * The account of a posting
    Filing (..),
    filingOf,
    filedAccount,
  )
where

import Data.Char (isAlpha, isDigit)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Tags (Tag)

{- The table -}

-- | The accounts of the journal that tags stand for: the table
-- journal-accounts of the configuration file. Each account is a name
-- 'readAccountName' took, since the configuration reads them all through
-- it.
type TagAccounts = Map Tag Text

-- | The table of a configuration that gives no tag an account, which is
-- the default: every payment is then posted to the unknown accounts
noTagAccounts :: TagAccounts
noTagAccounts = Map.empty

{- The name of an account -}

-- | The account under which the journal keeps the portfolios, as in
-- @Assets:hw:wallet0:portfolio0@
portfoliosAccount :: Text
portfoliosAccount = "Assets:hw"

-- | The name of an account of the journal, as the configuration file
-- writes it, or why it is no such name. The reason ends a sentence, as
-- in "it has two spaces in a row".
--
-- An account is made of names with a colon between them, as in
-- @Expenses:Housing:Rent@. ledger reads the colons as the levels of a
-- tree. A name is made of letters, digits, single spaces and the signs
-- @- _ & ' . \/ +@, and the account starts with a letter.
--
-- ledger itself takes almost any character. These rules are narrower, so
-- that a name never changes how its line of the journal reads:
--
-- * Two spaces in a row end the name of an account in a posting, and
--   what follows is read as its amount.
--
-- * A semicolon starts a note, and a parenthesis or a bracket at the
--   start makes a posting that is left out of the balance of its entry.
--
-- * A line break would start a new line of the journal.
--
-- An account of the portfolios is refused ('portfoliosAccount'). The
-- journal ends with the balance each of them must have, and a payment
-- posted there a second time would make ledger refuse the whole file.
readAccountName :: Text -> Either String Text
readAccountName name
  | Text.null name = Left "it is empty"
  | Just sign <- Text.find (not . allowed) name =
      Left ("it has the character " <> show sign <> ". An account is made of letters, digits, spaces and the signs - _ & ' . / +, with a colon between its names")
  | any Text.null names = Left "a name is missing before or after a colon"
  | Text.isInfixOf "  " name = Left "it has two spaces in a row, which end the name of an account in ledger"
  | any (\part -> Text.strip part /= part) names = Left "it has a space at the start or at the end of a name"
  | not (startsWithLetter name) = Left "an account starts with a letter"
  | name == portfoliosAccount || Text.isPrefixOf (portfoliosAccount <> ":") name =
      Left ("the accounts under " <> Text.unpack portfoliosAccount <> " are those of the portfolios, whose balances the journal states")
  | otherwise = Right name
  where
    names = Text.splitOn ":" name
    allowed c = isAlpha c || isDigit c || c `elem` (" :-_&'./+" :: String)
    startsWithLetter = maybe False (isAlpha . fst) . Text.uncons

{- The account of a posting -}

-- | What a group of tags says of the account of a posting: those of a
-- transaction, or those of a contact
data Filing
  = -- | None of the tags stands for an account
    NotFiled
  | -- | The tags stand for this one account. Several tags may stand for
    -- it.
    FiledUnder !Text
  | -- | The tags stand for two accounts or more, so they name none
    Disputed
  deriving (Eq, Show)

-- | What tags say of the account of a posting, given the table
filingOf :: TagAccounts -> [Tag] -> Filing
filingOf accounts tags = case nubOrd (mapMaybe (`Map.lookup` accounts) tags) of
  [] -> NotFiled
  [account] -> FiledUnder account
  _ -> Disputed

-- | The account of a posting, given the account it goes to when no tag
-- says otherwise, and what each group of tags says, the group that
-- counts the most first: the tags of the transaction, then those of the
-- contact paid, or of the contacts a payment received came from.
--
-- The first group that stands for an account decides. A group that is
-- disputed decides too, for the unknown account: the groups after it are
-- not asked. The tags of the transaction then said something of this
-- payment, and the contact cannot say which of them was meant.
filedAccount :: Text -> [Filing] -> Text
filedAccount unknown filings = case filter (/= NotFiled) filings of
  FiledUnder account : _ -> account
  _ -> unknown
