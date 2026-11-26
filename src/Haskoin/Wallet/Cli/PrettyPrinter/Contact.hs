{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about the address book: a contact in its compact view, its
-- verbose view and its table, and what deletecontact did.
module Haskoin.Wallet.Cli.PrettyPrinter.Contact
  ( noContacts,
    listedContactDoc,
    compactContactDoc,
    verboseContactDoc,
    contactAnswerDoc,
    contactDeletedDoc,
    contactTableColumns,
    contactRowDoc,
  )
where

import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Haskoin
import Haskoin.Wallet.Cli.PrettyPrinter.Base (columns, field, labelWord, secondText, tagMarks, text)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (hwLine, metavarWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (Column (..), Side (..), column, tableLine, tagsCell, tagsColumn, widestCell)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Contacts (Contact (..), inFours)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, hsep, indent, pretty, vcat, (<+>))

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
-- was added in the time zone of the computer
listedContactDoc :: View -> Contact -> IO (Doc Style)
listedContactDoc view contact = do
  added <- utcToLocalZonedTime (contactCreated contact)
  return $ case view of
    Verbose -> verboseContactDoc Nothing added contact
    -- The table is laid out by the section, which knows its widest
    -- cells ('contactRowDoc')
    _ -> compactContactDoc contact

-- | A contact in two lines: its first line, then its address in groups
-- of four
compactContactDoc :: Contact -> Doc Style
compactContactDoc contact =
  vcat [contactLine contact, indent 2 (pretty (inFours (contactAddress contact)))]

-- | A contact with every detail: its first line, then its address in
-- groups of four, its network when given, and when it was added. A
-- listing gives no network: its sections say it.
verboseContactDoc :: Maybe Network -> ZonedTime -> Contact -> Doc Style
verboseContactDoc netM added contact =
  vcat $
    contactLine contact
      : ( indent 2
            <$> [field 8 "In fours" <> pretty (inFours (contactAddress contact))]
              <> [field 8 "Network" <> text net.name | Just net <- [netM]]
              <> [field 8 "Added" <> text (secondText added)]
        )

-- | A contact as an answer shows it, with every detail and its network:
-- what newcontact answers, and label, tag, untag and show for an address
-- of the address book
contactAnswerDoc :: Network -> Contact -> IO (Doc Style)
contactAnswerDoc net contact = do
  added <- utcToLocalZonedTime (contactCreated contact)
  return (verboseContactDoc (Just net) added contact)

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

-- | A contact on one row of its table, under the columns given
contactRowDoc :: [Column] -> Contact -> Doc Style
contactRowDoc headed contact =
  tableLine . zip headed $
    [ pretty (contactAddress contact),
      pretty (inFours (contactAddress contact)),
      tagsCell (contactTags contact),
      if Text.null (contactLabel contact) then mempty else labelWord (contactLabel contact)
    ]
