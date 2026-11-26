{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An address on a line of an answer: one of the wallet, with its path and
-- its label, one of the address book, with its contact, and what is said
-- of it after it or on the line below.
--
-- The views of the coins and of the transactions show addresses the same
-- way, in lines and in tables, so the words are made here once.
module Haskoin.Wallet.Cli.PrettyPrinter.AddressWords
  ( changeWord,
    heldPathText,
    heldPathWord,
    ownAddressWord,
    ownAddressNote,
    ownAddressNoteBelow,
    bookAddressWord,
    contactNote,
    contactNoteBelow,
    fullAddressWords,
    trailingPathWord,
    withNotesBelow,
    wrappingLabelWord,
  )
where

import Data.List (intersperse)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.PrettyPrinter.Base (columns, labelWord, styled, tagMarks)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (PathNaming (..), ofPortfolio, ownPathText)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Keys (PoolPath (..), poolPathText)
import Haskoin.Wallet.Tx.Addresses (HeldAddress (..))
import Haskoin.Wallet.Util
import Prettyprinter (Doc, align, annotate, fillSep, flatAlt, group, hcat, hsep, line, pretty, vcat)

{- The addresses of the wallet -}

-- An address of the wallet is blue, with its label ('OwnAddress'), so
-- that it stands apart from the addresses of others in a transaction, and
-- so that a label on the line below its address is seen to go with it.
-- The compact views and the table name it by its type and its derivation
-- path, which is short and says which address of the portfolio it is. The
-- verbose views give it in full, with its path: the address is what
-- another wallet or a block explorer shows. The compact view of a coin
-- does too, since the address is what tells a coin apart
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Coin.compactCoinDoc').
--
-- An address of another portfolio of the database is an address of the
-- wallet too. It is blue like the others. Blue says whose the address is.
-- That is the first thing a user asks of an address a transaction pays.
-- An address of theirs needs no check. A stranger's must be checked
-- against what the payee gave. The field the address is under, From, To
-- or Change, says what the transaction does with it.
--
-- A payment to another portfolio then shows every address in blue. So
-- does a payment between the addresses of one portfolio. Both mean that
-- nothing left the wallet. Two things tell them apart. The path of the
-- address names the other portfolio. Its value is not green, since the
-- portfolio of the view does not receive it (@addressRow@ of
-- "Haskoin.Wallet.Cli.PrettyPrinter.Transaction").
--
-- The other choice was to leave such an address uncoloured. It would show
-- at a glance that the amount leaves the portfolio. The first line of the
-- view says that already. And an address of the user would look like one
-- to check.

-- | What marks a change address where a label would go: the user cannot
-- label one, and hw knows which addresses are change
changeWord :: Doc Style
changeWord = styled Muted "change address"

-- | The path of an address of the wallet in a transaction, as the view
-- writes it (the type t'PathNaming'). The path of an address of another
-- portfolio always names that portfolio. The path of an address of the
-- portfolio of the view names it when the paths of the view do.
heldPathText :: PathNaming -> HeldAddress -> String
heldPathText paths held = case held.heldIn of
  Just other -> cs (poolPathText held.heldPath) <> ofPortfolio paths other
  Nothing -> ownPathText paths held.heldPath

-- | The path of an address of the wallet where it stands for the address,
-- in the colour of the address, with the type of its account in front, as
-- in bip84\/0\/3 ('poolPathText'), and its portfolio after it when the
-- view names it ('heldPathText'). A portfolio has an address at \/0\/3 in
-- each of its accounts on a network. Here the address itself is not
-- written out, so the type alone tells them apart.
heldPathWord :: PathNaming -> HeldAddress -> Doc Style
heldPathWord paths held = styled OwnAddress (heldPathText paths held)

-- | An address of the wallet in full, in its colour
ownAddressWord :: Network -> Address -> Doc Style
ownAddressWord net addr = styled OwnAddress (addrToString net addr)

-- | The label of an address of the wallet, quoted, in the colour of its
-- address
ownLabelWord :: Text -> Doc Style
ownLabelWord = annotate OwnAddress . labelWord

-- | What is said of an address of the wallet after it on its line: its
-- label, quoted, in the colour of the address, or that it is a change
-- address, which never has a label. Nothing when it has neither. The flag
-- says whether to mark a change address: a view that lists change apart
-- says it once.
ownAddressNote :: Text -> Bool -> [Doc Style]
ownAddressNote label change =
  [ownLabelWord label | not (Text.null label)] <> [changeWord | change]

-- | What is said of an address of the wallet on the line below it, as
-- 'ownAddressNote' says it, but for a label that may wrap at the edge of
-- the screen ('wrappingLabelWord')
ownAddressNoteBelow :: Text -> Bool -> [Doc Style]
ownAddressNoteBelow label change =
  [annotate OwnAddress (wrappingLabelWord label) | not (Text.null label)] <> [changeWord | change]

{- The addresses of the address book -}

-- An address of the address book is one of somebody else that the user
-- added as a contact ("Haskoin.Wallet.Contacts"). A transaction shows it
-- underlined ('BookAddress'), then its label, where it would show the
-- address of a stranger bare. It is always written in full: the address
-- is what the payee gave, and what the user checks a payment against. A
-- path can stand for an address of the wallet, which hw derives itself.
-- A label cannot stand for an address of somebody else: two contacts may
-- share one, and a label is only what the user typed.
--
-- A contact without a label is followed by the words "address book",
-- faint, where its label would be (@bookWord@). So a screen without
-- styles, where the underline is lost, still tells every contact from a
-- stranger.

-- | An address of the address book in full, underlined
bookAddressWord :: Network -> Address -> Doc Style
bookAddressWord net addr = styled BookAddress (addrToString net addr)

-- | What marks an address of the address book that has no label, where
-- the label would go
bookWord :: Doc Style
bookWord = styled Muted "address book"

-- | What is said of an address of the address book after it on its line:
-- its label, quoted, or that it is of the address book when it has none
contactNote :: Contact -> [Doc Style]
contactNote contact
  | Text.null (contactLabel contact) = [bookWord]
  | otherwise = [labelWord (contactLabel contact)]

-- | What is said of an address of the address book on the line below it,
-- as 'contactNote' says it, but for a label that may wrap at the edge of
-- the screen ('wrappingLabelWord'), and with the tags of the contact
-- after it, as the tags of an address of the wallet follow its label
contactNoteBelow :: Contact -> [Doc Style]
contactNoteBelow contact =
  [if Text.null label then bookWord else wrappingLabelWord label]
    <> [hsep (tagMarks tags) | let tags = contactTags contact, not (null tags)]
  where
    label = contactLabel contact

-- | An address of the portfolio of the view in full, in its colour, then
-- its type and its derivation path, if known, faint: the path adds to the
-- address, which is what another wallet or a block explorer shows
fullAddressWords :: PathNaming -> Network -> Address -> Maybe PoolPath -> Doc Style
fullAddressWords paths net addr pathM = columns (ownAddressWord net addr : [trailingPathWord (ownPathText paths path) | Just path <- [pathM]])

-- | The path after an address in full, faint. It is the type and the
-- derivation path, as in bip84\/0\/3, with the portfolio after them when
-- the view names it. The form of the address tells its type too, but only
-- to a reader who knows the forms, and every view writes a path the same
-- way. The portfolio is faint with its path, since the two are read as
-- one name. The first line of a view of another portfolio already says so
-- in yellow.
trailingPathWord :: String -> Doc Style
trailingPathWord = styled Muted

-- | A line that starts with an address in full, and below it, starting
-- under the address, what is said of the address: its label, or the mark
-- of a change address ('ownAddressNoteBelow'), and what else a view adds,
-- two spaces apart. After an address in full, a label would push the line
-- far to the right. On the line below, in the colour of the address, it
-- is still seen to go with it. What is said wraps at the edge of the
-- screen, each line again under the address, so that long labels make no
-- line the terminal would wrap at its first column. A view that names an
-- address by its path keeps the label on the same line instead, since a
-- path is short (@purposeCell@ of
-- "Haskoin.Wallet.Cli.PrettyPrinter.Transaction"). So does the compact
-- view of a coin, the one exception among the views that give an address
-- in full, so that each coin takes two lines
-- ('Haskoin.Wallet.Cli.PrettyPrinter.Coin.compactCoinDoc').
withNotesBelow :: Doc Style -> [Doc Style] -> Doc Style
withNotesBelow first notes = align (vcat (first : [hcat (intersperse apart notes) | not (null notes)]))
  where
    -- Two spaces, or a new line when the next note does not fit
    apart = group (flatAlt line "  ")

-- | A label as 'labelWord' writes it, its words apart where the line may
-- wrap. Only a line that nothing follows on its right can wrap: never a
-- cell of a table, nor a line of columns.
wrappingLabelWord :: Text -> Doc Style
wrappingLabelWord label = annotate UserText (fillSep (pretty <$> Text.splitOn " " ("\"" <> visibleText label <> "\"")))
