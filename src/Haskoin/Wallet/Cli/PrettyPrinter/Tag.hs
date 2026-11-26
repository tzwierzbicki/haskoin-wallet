{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about tags: the tags and what uses each one, and what
-- renametag and deletetag did.
module Haskoin.Wallet.Cli.PrettyPrinter.Tag
  ( recordShownTags,
    tagUsesDoc,
    locksCoinsWords,
    tagLines,
    tagRenamedDoc,
    tagMergedDoc,
    tagDeletedDoc,
  )
where

import Data.List (intercalate, intersperse)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Haskoin.Wallet.Cli.PrettyPrinter.Base (counted, styled, tagWord, text)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (hwLine, metavarWord)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse (..))
import Haskoin.Wallet.Tags (ShownTag (..), Tag, colorName, tagText)
import Haskoin.Wallet.WalletFile (FileTag (..), recordFileTags)
import Haskoin.Wallet.WalletFile.Bip329 (Record (..))
import Numeric.Natural (Natural)
import Prettyprinter (Doc, hcat, indent, pretty, vcat, (<+>))

-- | The tags a record of a file gives, those that are tags, each with
-- the colour the record names for it: what show shows of a file, before
-- the wallet takes anything of it
recordShownTags :: Record -> [ShownTag]
recordShownTags r = fileTag <$> recordFileTags r

-- | The tags of the database, as tags lists them: each on a line, in its
-- colour, then the name of that colour, then how many addresses and
-- transactions have it. The names are padded to the longest, so that the
-- columns line up. A tag nothing has says so: the user may want to
-- delete it. The name of the colour is there for a screen without
-- colours, and for the user who wants to give another tag the same one.
-- A tag that locks its coins says so last, in the colour of what waits,
-- as coins shows a locked coin: it is why a payment leaves those coins.
tagUsesDoc :: [TagUse] -> Doc Style
tagUsesDoc [] =
  vcat
    [ "There are no tags.",
      "To put one on an address or a transaction, run:" <+> hwLine "tag" [metavarWord "ADDRESS|TXID|PATH", metavarWord "TAG"]
    ]
tagUsesDoc uses =
  vcat . tagLines $
    [ (use.tagUseTag, usesWords use.tagUseAddresses use.tagUseTxs use.tagUseContacts : [locksCoinsWords | use.tagUseLocksCoins])
      | use <- uses
    ]

-- | That a tag locks its coins, in the colour of what waits
locksCoinsWords :: Doc Style
locksCoinsWords = styled Waiting "locks its coins"

-- | A line for each tag: the tag in its colour, the name of that colour,
-- then what is said of the tag, with commas between. The names and the
-- colours are padded to the longest, so that the columns line up. A line
-- ends at its last word: nothing pads a tag that has no more to say.
tagLines :: [(ShownTag, [Doc Style])] -> [Doc Style]
tagLines tags = tagLine <$> tags
  where
    nameWidth = maximum (0 : [Text.length (tagText (shownTag tag)) | (tag, _) <- tags])
    colorWidth = maximum (0 : [Text.length (colorText tag) | (tag, _) <- tags])
    colorText tag = maybe "" colorName (shownTagColor tag)
    padding width shown = pretty (Text.replicate (width - Text.length shown) " ")
    tagLine (tag, said) =
      let name = tagText (shownTag tag)
          color = colorText tag
          hasMore = not (null said)
          colorCell = styled Muted (cs color)
       in hcat $
            [tagWord tag]
              <> [padding nameWidth name <> "  " <> colorCell | hasMore || not (Text.null color)]
              <> [padding colorWidth color <> (if colorWidth > 0 then "  " else mempty) <> hcat (intersperse ", " said) | hasMore]

-- | How many addresses, transactions and contacts of the address book
-- have a tag, in words: "2 addresses, 1 transaction, 1 contact", or that
-- nothing has it
usesWords :: Natural -> Natural -> Natural -> Doc Style
usesWords 0 0 0 = styled Muted "not used"
usesWords addresses txs contacts =
  text . intercalate ", " $
    [counted addresses "address" "addresses" | addresses > 0]
      <> [counted txs "transaction" "transactions" | txs > 0]
      <> [counted contacts "contact" "contacts" | contacts > 0]

-- | What renametag did: the name the tag had and the name it has now,
-- in its colour, then what has it. The second line is there for the user
-- to check that the right tag was renamed: a tag on twelve addresses is
-- not the one typed by mistake yesterday.
tagRenamedDoc :: Tag -> TagUse -> Doc Style
tagRenamedDoc old use =
  vcat
    [ "Renamed the tag" <+> text (cs (tagText old)) <+> "to" <+> tagWord use.tagUseTag,
      indent 2 (nowOnLine use)
    ]

-- | What renametag --merge did: the tag that is gone, and the tag it was
-- merged into, in its colour, then what has that tag now. The counts are
-- those of the two tags together, a thing that had both counted once.
tagMergedDoc :: Tag -> TagUse -> Doc Style
tagMergedDoc old use =
  vcat
    [ "Merged the tag" <+> text (cs (tagText old)) <+> "into" <+> tagWord use.tagUseTag,
      indent 2 (nowOnLine use)
    ]

-- | What has a tag now, as a sentence under what was done to the tag,
-- which also says when the tag locks its coins: a merge may have made
-- it do so
nowOnLine :: TagUse -> Doc Style
nowOnLine use = held <> (if use.tagUseLocksCoins then ", and it" <+> styled Waiting "locks its coins" else mempty)
  where
    held = case (use.tagUseAddresses, use.tagUseTxs, use.tagUseContacts) of
      (0, 0, 0) -> "Nothing has it"
      _ -> "It is on" <+> usesWords use.tagUseAddresses use.tagUseTxs use.tagUseContacts

-- | What deletetag did: the tag it deleted, and what had it
tagDeletedDoc :: Tag -> TagDeleted -> Doc Style
tagDeletedDoc tag had =
  vcat
    [ "Deleted the tag" <+> text (cs (tagText tag)),
      indent 2 $ case (had.deletedFromAddresses, had.deletedFromTxs, had.deletedFromContacts) of
        (0, 0, 0) -> "Nothing had it"
        _ -> "It was on" <+> usesWords had.deletedFromAddresses had.deletedFromTxs had.deletedFromContacts
    ]
