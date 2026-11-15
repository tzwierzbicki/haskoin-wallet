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
    contactOrder,
    inFours,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
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
