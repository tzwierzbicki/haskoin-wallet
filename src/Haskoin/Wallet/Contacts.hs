{-# LANGUAGE OverloadedStrings #-}

-- | The address book: the addresses of others that the user keeps, each
-- with a label and tags. An entry of the book is a contact.
--
-- = What it is for
--
-- hw knows the addresses of its own wallets, and shows them apart in a
-- transaction. Every other address was a stranger's, even the one the
-- user pays every month. The address book gives such an address a name.
-- A transaction that pays it, or that it pays from, then shows it
-- underlined, with its label ("Haskoin.Wallet.Cli.PrettyPrinter"). A
-- payment to it is easier to check before it is signed, and the history
-- says whom each payment went to.
--
-- = A contact is an address, not a person
--
-- A contact is one address on one network. A person with two addresses
-- is two contacts, which may share a label. The other choice was a name
-- with a list of addresses under it. It was turned down: what hw meets
-- in a transaction is an address, and that address must lead to one
-- entry. A tag groups the addresses of one person where that helps.
--
-- The word contact is used, and not address, because an address means an
-- address of the wallet everywhere else in hw: addresses lists them, and
-- receive gives one out.
--
-- = Only the addresses of others
--
-- An address of a wallet of the database is no contact. It has a path, a
-- label and tags of its own already, and the views name it by them. The
-- commands refuse to add one ("Haskoin.Wallet.Commands").
--
-- A wallet may come to hold the address of a contact later. importwallet
-- or recover adds the wallet that owns it. Or the user added an address
-- of their own that this computer had not generated yet, which hw cannot
-- know, and receive or a sync reaches it. The address is then the
-- wallet's everywhere: the views name it by its path, and label, tag,
-- untag and show work on the address of the wallet. The contact stays in
-- the address book, where nothing shows it any more. hw does not delete
-- it: its label and its tags are what the user typed. It says so
-- instead, with the address of the wallet the contact is now (the type
-- t'Holder'), wherever the user meets the contact or the address:
-- contacts marks it, the answer of an address of the wallet names its
-- contact, and sync lists such contacts until they are deleted.
--
-- Whether a wallet holds the address is looked up each time, and never
-- stored: a wallet deleted later makes the contact an address of
-- somebody else again.
--
-- = The address in fours
--
-- A contact shows its address twice. Once as it is, in one piece, so
-- that a double click selects it to copy. And once in groups of four
-- characters ('inFours'), so that the eye can compare it with the
-- address the payee gave, group by group. An address in one piece is
-- compared by its first and last characters alone, which is what an
-- attacker who makes a look-alike address counts on.
--
-- This module holds the contact as the answers show it and the rules
-- that need no database. The database keeps the contacts
-- ("Haskoin.Wallet.Database.Contact").
module Haskoin.Wallet.Contacts
  ( Contact (..),
    Holder (..),
    contactOrder,
    inFours,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Haskoin.Wallet.Database.Portfolio (PortfolioRef, parsePortfolioRef, portfolioRefPairs)
import Haskoin.Wallet.Keys (PoolPath (..))
import Haskoin.Wallet.Tags (ShownTag)

-- | A contact as an answer shows it: an address of the address book,
-- with its label and its tags. Its network comes with the answer, which
-- lists the contacts of each network together.
data Contact = Contact
  { -- | The address, as hw writes it on its network: a Bitcoin Cash
    -- address in the CashAddr form, with its prefix
    contactAddress :: !Text,
    -- | The label the user gave it, or the empty text for none
    contactLabel :: !Text,
    -- | Its tags, with their colours, in the order of the alphabet
    contactTags :: ![ShownTag],
    -- | When it was added to the address book
    contactCreated :: !UTCTime
  }
  deriving (Eq, Show)

instance ToJSON Contact where
  toJSON contact =
    object
      [ "address" .= contactAddress contact,
        "label" .= contactLabel contact,
        "tags" .= contactTags contact,
        "created" .= contactCreated contact
      ]

instance FromJSON Contact where
  parseJSON = withObject "Contact" $ \o ->
    Contact <$> o .: "address" <*> o .:? "label" .!= "" <*> o .:? "tags" .!= [] <*> o .: "created"

-- | The address of a wallet of the database that the address of a
-- contact is: the portfolio that holds it, and its path there, as in
-- bip84\/0\/3 of portfolio1. The network is that of the contact: the
-- same text on another network is another address, which the wallet may
-- not hold.
data Holder = Holder
  { holderPortfolio :: !PortfolioRef,
    holderPath :: !PoolPath
  }
  deriving (Eq, Show)

-- | In JSON, the keys of the portfolio, then the type and the path of
-- the address, as those of an address of the wallet
instance ToJSON Holder where
  toJSON holder =
    object $
      portfolioRefPairs (holderPortfolio holder)
        <> ["type" .= pathKind (holderPath holder), "path" .= pathInAccount (holderPath holder)]

instance FromJSON Holder where
  parseJSON = withObject "address of the wallet" $ \o ->
    Holder <$> parsePortfolioRef o <*> (PoolPath <$> o .: "type" <*> o .: "path")

-- | What the contacts of a network are listed by: the label first, in
-- the order of the alphabet, capital and small letters alike, then the
-- address. A user looks a contact up by its name, so the name orders the
-- book. A contact without a label comes after those that have one: it
-- has no name to be looked up by. The address ends the order, so that
-- two contacts with one label always come in the same order.
contactOrder :: Contact -> (Bool, Text, Text)
contactOrder contact = (Text.null label, Text.toCaseFold label, contactAddress contact)
  where
    label = contactLabel contact

-- | An address in groups of four characters, a space between them, for
-- the eye to compare with another copy of it: bc1q w508 d6qe jxtd. The
-- last group is shorter when the address does not divide by four.
--
-- The prefix of a Bitcoin Cash address stays in one piece, with its
-- colon, before the groups: bitcoincash: qpm2 qszn. It is the same for
-- every address of the network, so it is not what the eye compares, and
-- the groups then start where the address proper does.
--
-- Four is the size the eye takes in at once, and the one bank account
-- numbers are printed in.
inFours :: Text -> Text
inFours address = case Text.breakOnEnd ":" address of
  ("", whole) -> groups whole
  (prefix, payload) -> prefix <> " " <> groups payload
  where
    groups = Text.unwords . Text.chunksOf 4
