{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about the address book: a contact in its compact view, its
-- verbose view and its table, what deletecontact did, and the contacts
-- that the wallet holds the address of, as sync and the answer of an
-- address name them.
module Haskoin.Wallet.Cli.PrettyPrinter.Contact
  ( noContacts,
    listedContactDoc,
    compactContactDoc,
    verboseContactDoc,
    contactAnswerDoc,
    contactDeletedDoc,
    contactTableColumns,
    contactRowDoc,
    heldContactsDoc,
    alsoContactField,
  )
where

import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Haskoin
import Haskoin.Wallet.Cli.PrettyPrinter.Base (columns, counted, field, labelWord, secondText, sentence, styled, tagMarks, text, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (hwLine, metavarWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (portfolioWords)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (Column (..), Side (..), column, tableLine, tagsCell, tagsColumn, widestCell)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Contacts (Contact (..), Holder (..), inFours)
import Haskoin.Wallet.Database.Portfolio (WalletNaming)
import Haskoin.Wallet.Keys (poolPathText)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, hsep, indent, pretty, punctuate, vcat, (<+>))

{- The address book -}

-- A contact is an address of somebody else that the user keeps, with a
-- label and tags ("Haskoin.Wallet.Contacts"). Every view of a contact
-- gives its address twice. Once in one piece, first on its line, where a
-- double click selects it to copy. And once in groups of four characters
-- ('inFours'), for the eye to compare with the address the payee gave,
-- group by group.
--
-- The address is not underlined here, as it is in a transaction: in the
-- address book every address is a contact, and the underline would tell
-- nothing.

-- | What contacts says when it has no contact to list, with the command
-- that adds one: the address book is empty until the user fills it, and
-- nothing else in hw leads to newcontact
noContacts :: Doc Style
noContacts =
  vcat
    [ "There are no contacts to display",
      "To add an address of somebody else to the address book, run:" <+> hwLine "newcontact" [metavarWord "ADDRESS", metavarWord "LABEL"]
    ]

-- | The first line of a contact: its address in one piece, then its
-- label, quoted, and its tags, each between brackets, as the first line
-- of an address of the wallet ends
contactLine :: Contact -> Doc Style
contactLine contact =
  columns $
    [pretty (contactAddress contact)]
      <> [labelWord label | let label = contactLabel contact, not (Text.null label)]
      <> [hsep (tagMarks tags) | let tags = contactTags contact, not (null tags)]

-- | A contact as contacts lists it, in the view asked for, the time it
-- was added in the time zone of the computer, with the address of the
-- wallet it is, when a wallet of the database holds its address
-- (@heldWords@)
listedContactDoc :: WalletNaming -> View -> (Contact, Maybe Holder) -> IO (Doc Style)
listedContactDoc naming view (contact, holderM) = do
  added <- utcToLocalZonedTime (contactCreated contact)
  let heldM = heldWords naming <$> holderM
  return $ case view of
    Verbose -> verboseContactDoc Nothing added contact heldM
    -- The table is laid out by the section, which knows its widest
    -- cells ('contactRowDoc')
    _ -> compactContactDoc contact heldM

-- | A contact in two lines: its first line, then its address in groups
-- of four. A contact whose address the wallet holds has a third line,
-- which says so, with the words given (@heldWords@).
compactContactDoc :: Contact -> Maybe (Doc Style) -> Doc Style
compactContactDoc contact heldM =
  vcat $
    contactLine contact
      : (indent 2 <$> pretty (inFours (contactAddress contact)) : ["now" <+> held | Just held <- [heldM]])

-- | A contact with every detail: its first line, then its address in
-- groups of four, the address of the wallet it is now, in the words
-- given, if a wallet holds it (@heldWords@), its network when given, and
-- when it was added. A listing gives no network: its sections say it.
verboseContactDoc :: Maybe Network -> ZonedTime -> Contact -> Maybe (Doc Style) -> Doc Style
verboseContactDoc netM added contact heldM =
  vcat $
    contactLine contact
      : ( indent 2
            <$> [field 8 "In fours" <> pretty (inFours (contactAddress contact))]
              <> [field 8 "Now" <> held | Just held <- [heldM]]
              <> [field 8 "Network" <> text net.name | Just net <- [netM]]
              <> [field 8 "Added" <> text (secondText added)]
        )

-- | A contact as an answer shows it, with every detail and its network:
-- what newcontact answers, and label, tag, untag and show for an address
-- of the address book. No wallet holds the address of such a contact:
-- these commands take an address a wallet holds for that of the wallet.
contactAnswerDoc :: Network -> Contact -> IO (Doc Style)
contactAnswerDoc net contact = do
  added <- utcToLocalZonedTime (contactCreated contact)
  return (verboseContactDoc (Just net) added contact Nothing)

{- A contact the wallet holds -}

-- A wallet of the database may come to hold the address of a contact
-- ("Haskoin.Wallet.Contacts"). The views of the transactions then show
-- the address by its path, and the contact shows nowhere but in
-- contacts. So contacts marks it, with the address of the wallet it is
-- and what to do: the contact is of no use any more, and its tags still
-- count in tags and in the filters of txs until it goes. The mark has the
-- colour of what waits for the user.
--
-- The address book is of the whole database, so the path always names
-- its portfolio, the active one too, as the views name one
-- ('portfolioWords').

-- | What is said of a contact whose address a wallet of the database
-- holds, after the word now or the name of a field: the path of the
-- address with its portfolio, and to delete the contact
heldWords :: WalletNaming -> Holder -> Doc Style
heldWords naming holder =
  styled Waiting $
    cs (poolPathText (holderPath holder)) <> " of " <> portfolioWords naming (holderPortfolio holder) <> ": delete the contact"

-- | The contacts that a wallet of the database holds the address of, as
-- sync ends with them: a line that counts them, then each with its
-- label and its tags, and below it the address of the wallet it is
-- (@heldWords@), under the name of its network when several networks
-- have some. A last sentence says that the tags of such a contact still
-- count, when one has tags: the user then knows why tags and a filter of
-- txs may count a contact that no view shows.
heldContactsDoc :: WalletNaming -> [(Network, [(Contact, Holder)])] -> Doc Style
heldContactsDoc naming groups =
  vcat $
    text heading
      : concatMap section groups
      <> [sentence (wordsOf stillCount) | any (not . null . contactTags . fst) held]
  where
    held = concatMap snd groups
    several = length held > 1
    heading
      | several = "The wallet now holds the addresses of " <> counted (fromIntegral (length held)) "contact" "contacts" <> " of the address book:"
      | otherwise = "The wallet now holds the address of a contact of the address book:"
    section (net, contacts) =
      [indent 2 (text net.name) | length groups > 1]
        <> concat [[indent 2 (contactLine contact), indent 4 ("now" <+> heldWords naming holder)] | (contact, holder) <- contacts]
    stillCount
      | several = "Their tags still count in tags, and in txs --address-tag and txs --untagged, until you delete the contacts."
      | otherwise = "Its tags still count in tags, and in txs --address-tag and txs --untagged, until you delete the contact."

-- | The field of the answer of an address of the wallet that is also in
-- the address book, padded as the other fields of the answer are: the
-- label and the tags of its contact, and to delete the contact. receive
-- says it at the moment it gives such an address out, and show, label
-- and tag when the user names the address, which they may know as a
-- contact.
alsoContactField :: Natural -> Contact -> Doc Style
alsoContactField pad contact =
  field pad "Address book" <> styled Waiting "also a contact" <> named <> styled Waiting ": delete the contact"
  where
    notes = [labelWord label | let label = contactLabel contact, not (Text.null label)] <> tagMarks (contactTags contact)
    named
      | null notes = mempty
      | otherwise = "," <+> hsep notes

-- | What deletecontact did: the contact it deleted, as it was
contactDeletedDoc :: Contact -> Doc Style
contactDeletedDoc contact =
  vcat ["Deleted from the address book", indent 2 (contactLine contact)]

-- | The columns of the table of the contacts given, all those of a
-- section: the address, the address in groups of four, the tags, and the
-- label. The two forms of the address take the width of the widest
-- address listed: the section is read whole, and an address of the
-- address book may be of any kind, from 26 characters to 62.
contactTableColumns :: Natural -> [Contact] -> [Column]
contactTableColumns tagsWide contacts =
  [ column "Address" (widestCell (cs . contactAddress) contacts) LeftSide,
    column "In fours" (widestCell (cs . inFours . contactAddress) contacts) LeftSide,
    tagsColumn tagsWide,
    column "Label" 0 LeftSide
  ]

-- | A contact on one row of its table, under the columns given. The
-- last cell, which no column follows, gives its label, then the address
-- of the wallet it is, after a comma, when a wallet holds its address
-- (@heldWords@).
contactRowDoc :: WalletNaming -> [Column] -> (Contact, Maybe Holder) -> Doc Style
contactRowDoc naming headed (contact, holderM) =
  tableLine . zip headed $
    [ pretty (contactAddress contact),
      pretty (inFours (contactAddress contact)),
      tagsCell (contactTags contact),
      hsep . punctuate "," $
        [labelWord (contactLabel contact) | not (Text.null (contactLabel contact))]
          <> ["now" <+> heldWords naming holder | Just holder <- [holderM]]
    ]
