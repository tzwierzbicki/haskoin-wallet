{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about the file of a wallet: what importwallet did with each
-- record of a file, and what show tells of such a file.
module Haskoin.Wallet.Cli.PrettyPrinter.WalletFile
  ( importDoc,
    walletFileAnswerDoc,
    walletFileDoc,
  )
where

import Data.List (genericLength, intercalate, intersperse, partition)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.PrettyPrinter.Base (block, columns, commaList, counted, fileHeadingDoc, labelWord, sentence, styled, tagMarks, text, withStyle, wordsOf)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (accountRefLine)
import Haskoin.Wallet.Cli.PrettyPrinter.Portfolio (activeDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.Tag (locksCoinsWords, recordShownTags, tagLines)
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), poolPathText, portfolioAccounts)
import Haskoin.Wallet.Util
import Haskoin.Wallet.WalletFile (AccountImport (..), FileRecord (..), FileShown (..), FileTag (..), ImportReport (..), LabelImport (..), PortfolioImport (..), RecordFate (..), WalletImport (..), importCreated, isContactRecord)
import Haskoin.Wallet.WalletFile.Bip329 (Record (..), RecordType (..), parseKeypath)
import Prettyprinter (Doc, hsep, indent, pretty, punctuate, vcat, (<+>))

{- Imports -}

-- | What importwallet did with a file: each wallet the file names, with
-- what became of it and of its portfolios, then which portfolio is active
-- when the import created one ('activeDoc'), then the keys left aside, a
-- blank line between them. Then come the counts of the import
-- (@importCountsDoc@).
importDoc :: ImportReport -> Maybe ActiveAfter -> Doc Style
importDoc report activeM =
  vcat . intersperse mempty $
    (walletImportDoc False <$> reportWallets report)
      <> [activeDoc active | Just active <- [activeM], not (null (importCreated report))]
      <> [leftAsideDoc "not imported:" name why | AccountLeftAside name why <- reportAccounts report]
      <> [importCountsDoc report]

-- | The counts of an import, a line for each kind: the labels and locks
-- set, the labels of the database that those of the file replaced, what
-- the file labels that the wallet does not have yet, with the addresses
-- among them that hw would not derive from their key path, what is left
-- aside, and the lines that are not records.
--
-- importwallet ends its answer with them, and show ends the view of a
-- file with the same lines (@walletFileParts@): the user reads before
-- the import what it would answer, in the words it would use. A file of
-- another wallet then says at once whether it concerns this wallet, by
-- what it would set and what the wallet does not have.
--
-- The labels set go from the largest thing to the smallest: wallets,
-- portfolios, addresses, transactions. The label of a wallet or a
-- portfolio is its description. Left aside are the labels of wallets and
-- portfolios that keep a description of their own, and what hw keeps no
-- label for.
importCountsDoc :: ImportReport -> Doc Style
importCountsDoc report = vcat summary
  where
    summary =
      [labelsLine]
        <> ["Labels replaced:" <+> commaList (text <$> labelsReplaced) | not (null labelsReplaced)]
        <> ["Tags set:" <+> commaList (text <$> tagsSet) | not (null tagsSet)]
        <> [text ("Address book: " <> counted n "contact" "contacts") | let n = reportContacts report, n > 0]
        <> [text ("Coins locked: " <> show n) | let n = reportCoinsLocked report, n > 0]
        <> [text ("Coins unlocked: " <> show n) | let n = reportCoinsUnlocked report, n > 0]
        <> ["Not in the wallet yet:" <+> commaList (text <$> missing) | not (null missing)]
        <> [indent 2 (sentence (wordsOf (tooFar n))) | let n = reportAddressesTooFar report, n > 0]
        <> ["Left aside:" <+> commaList (text <$> aside) | not (null aside)]
        <> [styled Failure "Lines that are not records:" <+> text (intercalate ", " (show <$> reportUnreadable report)) | not (null (reportUnreadable report))]
    labelsLine = case labelsSet of
      [] -> "Labels set:" <+> styled Muted "none"
      parts -> "Labels set:" <+> commaList (text <$> parts)
    labelsSet =
      countedIf (reportWalletLabels report) "wallet" "wallets"
        <> countedIf (reportPortfolioLabels report) "portfolio" "portfolios"
        <> countedIf (reportAddressLabels report) "address" "addresses"
        <> countedIf (reportTxLabels report) "transaction" "transactions"
    -- The labels of the database that a label of the file took the place
    -- of. The line is there only when one did, so that a label that went
    -- does not go without a word. show names each before the import.
    labelsReplaced =
      countedIf (reportAddressLabelsReplaced report) "address" "addresses"
        <> countedIf (reportTxLabelsReplaced report) "transaction" "transactions"
        <> countedIf (reportContactLabelsReplaced report) "contact" "contacts"
    -- What the file put tags on. The line is there only when it tagged
    -- something: most files hold no tag.
    tagsSet =
      countedIf (reportAddressTags report) "address" "addresses"
        <> countedIf (reportTxTags report) "transaction" "transactions"
    missing =
      countedIf (reportAddressesMissing report) "address" "addresses"
        <> countedIf (reportTxsMissing report) "transaction" "transactions"
        <> countedIf (reportCoinsMissing report) "coin" "coins"
    aside =
      keptOwn (reportWalletLabelsKept report) "wallet" "wallets"
        <> keptOwn (reportPortfolioLabelsKept report) "portfolio" "portfolios"
        <> [counted n "record" "records" <> " of accounts the wallet does not have" | let n = reportOtherAccounts report, n > 0]
        <> [counted n "record" "records" <> " of the portfolios --wallet or --portfolio leaves out" | let n = reportOtherPortfolios report, n > 0]
        <> [counted n "label" "labels" <> " of change addresses" | let n = reportChangeAddresses report, n > 0]
        <> [counted n "contact that is an address" "contacts that are addresses" <> " of the wallet" | let n = reportContactsOwn report, n > 0]
        <> [counted n "record" "records" <> " hw keeps no label for" | let n = reportNotKept report, n > 0]
    -- A count with its noun, when there is anything to count
    countedIf n singular plural = [counted n singular plural | n > 0]
    -- The labels of the file that wallets or portfolios did not take,
    -- since each keeps a description of its own: "1 label of a portfolio
    -- that keeps its own", "2 labels of portfolios that keep their own"
    keptOwn n singular plural =
      countedIf n ("label of a " <> singular <> " that keeps its own") ("labels of " <> plural <> " that keep their own")
    -- Why hw derived none of these addresses from their key path: a file
    -- names any path, and hw generates no further than the scan limit
    -- past the last used address of a chain
    tooFar n = "Of them, " <> counted n "address lies" "addresses lie" <> " over a thousand past the last used address of " <> (if n == 1 then "its" else "their") <> " chain, further than hw generates on the word of a file."

-- | A wallet a file names, and what became of it, or what would with the
-- flag: its name and the fingerprint of its seed, whether it was created
-- or found, then a line for each portfolio the file names, with whether
-- it was created with its accounts or found whole.
--
-- With the flag, which is how show shows a file before the import, the
-- line of a wallet and that of a portfolio end with the description the
-- file brings it (@fileDescriptionDocs@). The answer of importwallet
-- counts these labels instead, among those set and those left aside.
walletImportDoc :: Bool -> WalletImport -> Doc Style
walletImportDoc dryRun wallet =
  vcat $
    columns
      ( [ pretty (walletName (walletImportIndex wallet)) `withStyle` Account,
          pretty (fingerprintToText (walletImportFingerprint wallet)),
          status
        ]
          <> descriptionOf (walletImportLabel wallet)
      )
      : (indent 2 . portfolioDoc <$> walletImportPortfolios wallet)
  where
    descriptionOf labelM = if dryRun then fileDescriptionDocs labelM else []
    status
      | walletImportCreated wallet = if dryRun then "not in the database, and importwallet creates it" else styled Good "created"
      | otherwise = if dryRun then "in the database" else "already in the database"
    -- A portfolio created gets every account at once, one it has gets
    -- none
    portfolioDoc p =
      columns
        ( [ pretty (portfolioName (portfolioImportIndex p)) `withStyle` Account,
            commaList (text <$> if portfolioImportCreated p then [created, added (genericLength portfolioAccounts)] else [added 0])
          ]
            <> descriptionOf (portfolioImportLabel p)
        )
    created = if dryRun then "importwallet creates it" else "created"
    added 0 = "nothing to add"
    added n = counted n "account" "accounts" <> (if dryRun then " to add" else " added")

-- | The description a file brings a wallet or a portfolio, at the end of
-- its line, as the view of a database file ends the line with the
-- description (@shownWalletDoc@ of
-- "Haskoin.Wallet.Cli.PrettyPrinter.Backup"): the label of the file, between
-- quotes.
-- When the wallet or the portfolio has a description of its own,
-- importwallet keeps that one, and the line says so and names it: the
-- user then knows which of the two texts stays, before the import.
fileDescriptionDocs :: Maybe LabelImport -> [Doc Style]
fileDescriptionDocs labelM =
  [labelWord (labelGiven label) | Just label <- [labelM]]
    <> ["left aside: it keeps" <+> labelWord own | Just own <- [labelKeptInstead =<< labelM]]

-- | The labels of the database that the label of a record of a file would
-- replace, after the label, in the words of the answer of importwallet
-- (\"Labels replaced\"). A record with an empty label removes them.
replacedLabelsDocs :: FileRecord -> [Doc Style]
replacedLabelsDocs shown =
  [word <+> hsep (punctuate " and" (labelWord <$> replaced)) | let replaced = fileRecordReplaces shown, not (null replaced)]
  where
    word = if maybe True Text.null (recordLabel (fileRecord shown)) then "removes" else "replaces"

-- | What importwallet would make of a record of a file, at the end of its
-- line, when it would set nothing of it: the words are those of the
-- counts under the records (@importCountsDoc@), which say how many such
-- records there are, and not which. A record importwallet would set
-- says nothing more.
recordFateDocs :: FileRecord -> [Doc Style]
recordFateDocs shown = case fileRecordFate shown of
  FateSet -> []
  FateNotInWallet -> ["not in the wallet yet"]
  FateOtherAccount -> ["left aside: the wallet does not have its account"]
  FateOwnAddress -> ["left aside: an address of the wallet"]

-- | A key of a file that is not one of an account of a wallet of hw: the
-- start of the key, and why it is left aside
leftAsideDoc :: String -> Text -> Text -> Doc Style
leftAsideDoc notDone name why =
  vcat [pretty (visibleText name) `withStyle` Account, indent 2 (styled Failure notDone <+> pretty why)]

{- Files of a wallet -}

-- | A file of a wallet as show answers with it: its heading, which says
-- that it is a BIP 329 file, then what importwallet would make of it
-- (@walletFileParts@), a blank line between the parts
walletFileAnswerDoc :: FilePath -> FileShown -> Doc Style
walletFileAnswerDoc file shown =
  vcat . intersperse mempty $
    fileHeadingDoc "BIP 329 file" file "A file of a wallet, shown as importwallet would take it" : walletFileParts shown

-- | A file of a wallet as show shows it, a blank line between its parts.
-- Each wallet comes first, with what importwallet would do with it and
-- with its portfolios. Then each account comes with the labels and locks
-- of its records, those that have any, and the keys importwallet would
-- leave aside. Then come the labels and locks of no account of the file,
-- then the contacts of the address book, each with its network, then the
-- tags that nothing has (@fileTagsDoc@), and last the counts importwallet
-- would end its answer with (@importCountsDoc@): what it would set, what
-- the wallet does not have yet, what is left aside and the lines that are
-- not records.
--
-- The line of an account names its wallet when the file has several
-- wallets, and its portfolio alone otherwise. That is the rule of every
-- view ('WalletNaming'), counted over the wallets of the file in place
-- of those of the database: the view is of the file, and its first lines
-- name each wallet it holds.
walletFileDoc :: FileShown -> Doc Style
walletFileDoc = vcat . intersperse mempty . walletFileParts

-- | The parts of a file of a wallet, in the order 'walletFileDoc' gives
-- them, for it and for the answer that puts a heading before them
walletFileParts :: FileShown -> [Doc Style]
walletFileParts shown =
  (walletImportDoc True <$> reportWallets (shownReport shown))
    <> [doc | (outcome, records) <- shownAccounts shown, doc <- fileAccountDocs naming outcome records]
    <> [vcat ("Labels without an account in the file" : (indent 2 <$> recordsDocs Nothing others)) | not (null others)]
    <> [vcat ("Address book" : (indent 2 . fileContactRow <$> contacts)) | not (null contacts)]
    <> [fileTagsDoc tags | let tags = shownTagsNotUsed shown, not (null tags)]
    <> [importCountsDoc (shownReport shown)]
  where
    naming = walletNamingOf (length (reportWallets (shownReport shown)))
    -- The contacts of the address book are of no account either, and
    -- come apart from the labels that name none
    (contacts, others) = partition (isContactRecord . fileRecord) (shownOthers shown)

-- | The tags of a file that nothing has, under a heading of their own,
-- in the words of tags: the file brings them back as tags that tags
-- lists as "not used", so show names them before the import. The tags
-- that something has are beside their address, their transaction or
-- their contact. Each comes as tags lists a tag ('tagLines'), without
-- the counts, which the heading gives for all: its colour, and whether
-- it locks its coins, since a lock set up before any address has the tag
-- is what a restore must not lose.
fileTagsDoc :: [FileTag] -> Doc Style
fileTagsDoc tags =
  vcat ("Tags not used" : (indent 2 <$> tagLines [(fileTag tag, [locksCoinsWords | fileTagLocksCoins tag]) | tag <- tags]))

-- | A contact of the address book in a file: its network, its address,
-- then its label, the label of the address book it would replace, and its
-- tags, as importwallet would put it in the address book, and why it
-- would not (@recordFateDocs@). What comes from
-- the file is written out, never as an order to the terminal.
fileContactRow :: FileRecord -> Doc Style
fileContactRow shown =
  columns $
    [text (cs (visibleText network)) | Just network <- [recordNetwork r]]
      <> [pretty (visibleText (recordRef r))]
      <> [labelWord label | Just label <- [recordLabel r], not (Text.null label)]
      <> replacedLabelsDocs shown
      <> [hsep (tagMarks tags) | let tags = recordShownTags r, not (null tags)]
      <> recordFateDocs shown
  where
    r = fileRecord shown

-- | An account of a file with the labels and locks of its records, when
-- it has any: a whole portfolio has twelve accounts, and most carry no
-- label. A key importwallet would leave aside comes with the reason. The
-- account is named with its wallet or not, as given.
fileAccountDocs :: WalletNaming -> AccountImport -> [FileRecord] -> [Doc Style]
fileAccountDocs naming outcome records = case outcome of
  AccountAdded ref -> withRecords ref
  AccountHeld ref -> withRecords ref
  AccountLeftAside name why -> [leftAsideDoc "importwallet leaves it aside:" name why]
  where
    withRecords ref = [vcat (accountRefLine naming ref : (indent 2 <$> recordsDocs (Just (accountRefKind ref)) records)) | not (null records)]

-- | The labels and locks of records, under a heading for each kind. An
-- address comes after its key path, when its record gives one, the paths
-- padded to the longest. The key path of the records of an account has
-- the type of the account given in front, as every view writes a path
-- (bip84\/0\/3). Records of no account keep the key path alone, since
-- nothing says which account it is in. A public key comes in place of an
-- address, in hexadecimal. A transaction comes by its txid and a coin by its
-- outpoint, in full: the wallet may hold neither, so a short id could
-- name nothing. A label is followed by the labels of the database it
-- would replace (@replacedLabelsDocs@). A line ends with why importwallet
-- would set nothing of its record (@recordFateDocs@). What comes from the
-- file is written out, never as an order to the terminal.
recordsDocs :: Maybe AccountKind -> [FileRecord] -> [Doc Style]
recordsDocs kindM shownRecords =
  section (titled "Address" [AddrRecord, PubkeyRecord]) (labelledAddressRow <$> ofTypes [AddrRecord, PubkeyRecord])
    <> section (titled "Transaction" [TxRecord]) (txRow <$> ofTypes [TxRecord])
    <> section "Coins" (coinRow <$> ofTypes [OutputRecord])
  where
    ofTypes kinds = filter ((`elem` kinds) . recordType . fileRecord) shownRecords
    -- The title of a section says tags too when one of its records has
    -- some: a tagged address without a label is under no title that
    -- names labels alone
    titled what kinds
      | any (not . null . recordShownTags . fileRecord) (ofTypes kinds) = what <> " labels and tags"
      | otherwise = what <> " labels"
    section :: String -> [Doc Style] -> [Doc Style]
    section title rows = [vcat (styled Field title : (indent 2 <$> rows)) | not (null rows)]
    keypathOf shown = maybe "" typedPath (parseKeypath =<< recordKeypath (fileRecord shown))
    typedPath path = maybe (pathToStr path) (\kind -> cs (poolPathText (PoolPath kind path))) kindM
    keypathWidth = fromIntegral (maximum (0 : (length . keypathOf <$> ofTypes [AddrRecord, PubkeyRecord])))
    reference shown = pretty (visibleText (recordRef (fileRecord shown)))
    -- The label of the record, then the labels it would replace
    labelOf shown = [labelWord label | Just label <- [recordLabel (fileRecord shown)], not (Text.null label)] <> replacedLabelsDocs shown
    -- The tags of the record that are tags, each with the colour the
    -- file gives it, between brackets as every view writes them
    tagsOf shown = [hsep (tagMarks tags) | let tags = recordShownTags (fileRecord shown), not (null tags)]
    labelledAddressRow shown = columns ([text (block keypathWidth (keypathOf shown)) | keypathWidth > 0] <> [reference shown] <> labelOf shown <> tagsOf shown <> recordFateDocs shown)
    txRow shown = columns (reference shown : labelOf shown <> tagsOf shown <> recordFateDocs shown)
    coinRow shown = columns ([reference shown, if recordSpendable (fileRecord shown) == Just False then "locked" else "unlocked"] <> recordFateDocs shown)
