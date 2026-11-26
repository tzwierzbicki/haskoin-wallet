{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers of the commands, as hw prints them for a person, and the
-- next steps of a payment. Each part of an answer names what it is with a
-- 'Style', as the help does: a field, an amount, an account, a command to
-- type. "Haskoin.Wallet.Cli.Style" says how each one looks, so that the help
-- and the answers speak the same colours.
--
-- An answer is a document of lines. A line that may be missing, such as
-- the label of an address, comes as a list of none or one line.
--
-- This module is the way in: 'prettyPrinter' prints the answer of a
-- command, and 'responsePrinter' makes its document. The documents are made
-- in the modules below this one, in three layers. A module uses the layers
-- under its own, and never a module above it.
--
-- The parts every answer is made of:
--
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Base": text, fields, sentences,
--   amounts, times, confirmations and tags
--
-- What several kinds of answers share:
--
-- * "Haskoin.Wallet.Cli.PrettyPrinter.CommandLine": a command line the user
--   can copy
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Place": the portfolio an answer is
--   about, and the paths of its addresses
-- * "Haskoin.Wallet.Cli.PrettyPrinter.AddressWords": an address on a line,
--   of the wallet or of the address book
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Table": the tables of --tabular
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Balance": the amounts of a balance
--
-- The answers, in modules named as those of "Haskoin.Wallet.Commands":
--
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Portfolio": mnemonics, wallets,
--   portfolios and their table
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Defaults": what showdefault shows
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Sync": syncs
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Tag": tags
-- * "Haskoin.Wallet.Cli.PrettyPrinter.WalletFile": the file of a wallet and
--   its import
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Backup": database files and backups
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Address": addresses, balances and
--   their tables
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Coin": coins and their table
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Transaction": transactions, the
--   tables of txs and pendingtxs
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Payment": what a command says before
--   it prepares a payment, and fee rates
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Sweep": sweeps
-- * "Haskoin.Wallet.Cli.PrettyPrinter.BumpFee": what bumpfee asks
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Contact": the address book
-- * "Haskoin.Wallet.Cli.PrettyPrinter.Listing": the sections and the rows
--   of a listing
-- * "Haskoin.Wallet.Cli.PrettyPrinter.NextSteps": the next steps
module Haskoin.Wallet.Cli.PrettyPrinter
  ( -- * Printing an answer
    prettyPrinter,
    responsePrinter,

    -- * The modules of the answers
    module Haskoin.Wallet.Cli.PrettyPrinter.Base,
    module Haskoin.Wallet.Cli.PrettyPrinter.CommandLine,
    module Haskoin.Wallet.Cli.PrettyPrinter.Place,
    module Haskoin.Wallet.Cli.PrettyPrinter.AddressWords,
    module Haskoin.Wallet.Cli.PrettyPrinter.Table,
    module Haskoin.Wallet.Cli.PrettyPrinter.Balance,
    module Haskoin.Wallet.Cli.PrettyPrinter.Defaults,
    module Haskoin.Wallet.Cli.PrettyPrinter.Portfolio,
    module Haskoin.Wallet.Cli.PrettyPrinter.Sync,
    module Haskoin.Wallet.Cli.PrettyPrinter.Tag,
    module Haskoin.Wallet.Cli.PrettyPrinter.WalletFile,
    module Haskoin.Wallet.Cli.PrettyPrinter.Backup,
    module Haskoin.Wallet.Cli.PrettyPrinter.Address,
    module Haskoin.Wallet.Cli.PrettyPrinter.Coin,
    module Haskoin.Wallet.Cli.PrettyPrinter.Transaction,
    module Haskoin.Wallet.Cli.PrettyPrinter.Payment,
    module Haskoin.Wallet.Cli.PrettyPrinter.Sweep,
    module Haskoin.Wallet.Cli.PrettyPrinter.BumpFee,
    module Haskoin.Wallet.Cli.PrettyPrinter.Contact,
    module Haskoin.Wallet.Cli.PrettyPrinter.Listing,
    module Haskoin.Wallet.Cli.PrettyPrinter.NextSteps,
  )
where

import Control.Monad
import Data.List (intercalate, intersperse)
import Data.Maybe (fromMaybe, maybeToList)
import Data.String.Conversions (cs)
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.Command (AddressCode (..), TxShown (..), WordsLayout (..))
import Haskoin.Wallet.Cli.PrettyPrinter.Address
import Haskoin.Wallet.Cli.PrettyPrinter.AddressWords
import Haskoin.Wallet.Cli.PrettyPrinter.Backup
import Haskoin.Wallet.Cli.PrettyPrinter.Balance
import Haskoin.Wallet.Cli.PrettyPrinter.Base
import Haskoin.Wallet.Cli.PrettyPrinter.BumpFee
import Haskoin.Wallet.Cli.PrettyPrinter.Coin
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine
import Haskoin.Wallet.Cli.PrettyPrinter.Contact
import Haskoin.Wallet.Cli.PrettyPrinter.Defaults
import Haskoin.Wallet.Cli.PrettyPrinter.Listing
import Haskoin.Wallet.Cli.PrettyPrinter.NextSteps
import Haskoin.Wallet.Cli.PrettyPrinter.Payment
import Haskoin.Wallet.Cli.PrettyPrinter.Place
import Haskoin.Wallet.Cli.PrettyPrinter.Portfolio
import Haskoin.Wallet.Cli.PrettyPrinter.Sweep
import Haskoin.Wallet.Cli.PrettyPrinter.Sync
import Haskoin.Wallet.Cli.PrettyPrinter.Table
import Haskoin.Wallet.Cli.PrettyPrinter.Tag
import Haskoin.Wallet.Cli.PrettyPrinter.Transaction
import Haskoin.Wallet.Cli.PrettyPrinter.WalletFile
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag (TagWidths (..))
import Haskoin.Wallet.Keys.MessageSigning (signedMessageText)
import Haskoin.Wallet.ShortId (shortHashText)
import Prettyprinter (indent, vcat, (<+>))
import System.Exit

-- | Print the answer of a command. A listing shows each element in the
-- view given, and an address that goes with a code shows it on a terminal.
-- The words of a new secret come in the layout given. A portfolio is
-- named with its wallet or not, as given: the answer does not say it,
-- since it follows from the wallets of the database ('WalletNaming').
prettyPrinter :: UnitChoice -> View -> WalletNaming -> AddressCode -> TxShown -> WordsLayout -> Response -> IO ()
prettyPrinter choice view naming code shown layout res = do
  responsePrinter choice view naming code shown layout res
  when (responseFailed res) exitFailure

-- | Print the answer of a command, as 'prettyPrinter' does, without the
-- exit status of a failure
responsePrinter :: UnitChoice -> View -> WalletNaming -> AddressCode -> TxShown -> WordsLayout -> Response -> IO ()
responsePrinter choice view naming code shown layout answer =
  case answer of
    -- A transaction just signed, shown in full before the secret was asked
    ResponseTx _ info _ | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    ResponseOutsideTx _ info | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    -- A transaction send, sendall or sweep just sent, shown in full
    -- before the secret too
    ResponseSentPending _ info after _ | SentTx <- shown -> renderIO $ withSyncAfter after (sentDoc info)
    -- The transaction broadcast sent, as the wallet held it
    ResponseSentPending place info after activeM -> do
      shownTx <- txAnswerDoc naming (placeNetwork place) (Just (SeenFrom (placePortfolio place) activeM)) choice info
      renderIO $ withSyncAfter after shownTx
    -- The listings, which the screen reads in chunks ('sectionsPrinter')
    ResponsePortfolios {} -> listing
    ResponseAddresses {} -> listing
    ResponseBalances {} -> listing
    ResponseTxs {} -> listing
    ResponsePendingTxs {} -> listing
    ResponseCoins {} -> listing
    ResponseContacts {} -> listing
    ResponseError err -> renderIO $ errorDoc err
    -- The sheet is right below the words it is for. The warning is
    -- apart, below what was created and above the step that switches to
    -- it, so that it is read before the switch.
    ResponseNewPortfolio secretM masterKeyM portfolio active warningM sheetM ->
      renderIO . vcat . intersperse mempty $
        [secretDoc layout secret | Just secret <- [secretM]]
          <> [secretSheetDoc secret sheet | Just secret <- [secretM], Just sheet <- [sheetM]]
          <> [masterKeyDoc key | Just key <- [masterKeyM]]
          <> [newPortfolioDoc portfolio active]
          <> [sentence (wordsOf (cs warning)) `withStyle` Waiting | Just warning <- [warningM]]
    ResponsePortfolio portfolio -> renderIO =<< listedPortfolioDoc choice portfolio
    -- The outcome first, then the portfolio as portfolios shows it
    ResponseSwitched portfolio -> do
      portfolioDoc <- listedPortfolioDoc choice portfolio
      renderIO $ vcat [text ("The active portfolio is now " <> refText (viewRef portfolio) <> "."), portfolioDoc]
    -- In green when the mnemonic matches, in red otherwise. The master
    -- private key is below, apart, as newportfolio shows it.
    ResponseChecked _ matches said masterKeyM ->
      renderIO . vcat . intersperse mempty $
        styled (if matches then Good else Failure) (cs said) : [masterKeyDoc key | Just key <- [masterKeyM]]
    -- What was deleted, then the backup hw made of it first
    ResponseDeleted said backupM ->
      renderIO . vcat $
        styled Good (cs said) : maybe [] (backupFilesDocs "Before that, the database was copied to") backupM
    -- The keys written, then where the journal is kept from now on, or
    -- that it is gone, when the command turned it on or off
    ResponseDefaults defaults file journalM ->
      renderIO . vcat $
        keyRowsDoc defaults
          <> ["Written to" <+> styled UserText file]
          <> [journalSwitchDoc switched | Just switched <- [journalM]]
    -- The keys of the file, then the active portfolio of the database,
    -- apart from them. No key asked for leaves the file out too.
    ResponseShownDefaults defaults file active ->
      renderIO . vcat . intersperse mempty $
        [vcat (keyRowsDoc defaults <> ["Read from" <+> styled UserText file]) | not (null defaults)]
          <> maybeToList (activeShownDoc active)
    ResponseFile f -> renderIO $ "Written to" <+> styled UserText f
    ResponseBackup database labels ->
      renderIO $
        vcat
          [ "The database was copied to" <+> styled UserText database,
            "Its labels were written to" <+> styled UserText labels
          ]
    ResponseRestored file contents backupM damagedM -> renderIO =<< restoredAnswerDoc file contents backupM damagedM
    ResponseBackups backups -> do
      now <- getCurrentTime
      renderIO (backupsDoc now backups)
    ResponseImport report active -> renderIO $ importDoc report active
    ResponseWalletFile file contents -> renderIO $ walletFileAnswerDoc file contents
    ResponseDatabaseFile file contents -> renderIO =<< databaseFileAnswerDoc file contents
    ResponseAddress ref addr funds tags followedM activeM ->
      renderIO =<< addressAnswerDoc naming code choice activeM ref addr funds tags followedM
    ResponseTags uses -> renderIO (tagUsesDoc uses)
    ResponseTagRenamed old use -> renderIO (tagRenamedDoc old use)
    ResponseTagMerged old use -> renderIO (tagMergedDoc old use)
    ResponseTagDeleted tag had -> renderIO (tagDeletedDoc tag had)
    ResponseContact net contact -> renderIO =<< contactAnswerDoc net contact
    ResponseContactDeleted _ contact -> renderIO (contactDeletedDoc contact)
    ResponseDerivedAddr ref path addr -> renderIO $ derivedAddrDoc naming ref path addr
    ResponseCoin ref coin activeM -> renderIO =<< coinAnswerDoc naming choice activeM ref coin
    ResponseTx place txInfo activeM ->
      renderIO =<< txAnswerDoc naming (placeNetwork place) (Just (SeenFrom (placePortfolio place) activeM)) choice txInfo
    -- Each portfolio's view of the transaction, a blank line between them
    ResponseLabelledTxs held activeM -> renderIO =<< heldTxsDoc naming choice activeM held
    ResponseHeldTxs held activeM -> renderIO =<< heldTxsDoc naming choice activeM held
    ResponsePrepared place txInfo left activeM -> renderIO =<< preparedDoc naming choice place txInfo left activeM
    ResponseSweep placeM net txInfo empty unswept activeM ->
      renderIO =<< sweepDoc naming choice (flip SeenFrom activeM . placePortfolio <$> placeM) net txInfo empty unswept
    ResponseOutsideTx net txInfo -> renderIO =<< txAnswerDoc naming net Nothing choice txInfo
    ResponseSentTx net h after ->
      renderIO . withSyncAfter after $
        vcat
          [ field 9 "Sent" <> text (cs $ txHashToHex h),
            indent 2 $ field 9 "Short id" <> text (shortHashText h),
            indent 2 $ field 9 "Network" <> text net.name
          ]
    -- By the hash pendingtxs showed: the id of a signed transaction. The
    -- coins it spent are free to spend again, and the change addresses it
    -- paid are free for the next transaction.
    ResponseDeleteTx nosigH txidM coins addrs ->
      renderIO $
        vcat
          [ field 9 "Deleted" <> styled PendingHash (cs $ txHashToHex deleted),
            indent 2 $ field 9 "Short id" <> styled PendingHash (shortHashText deleted),
            indent 2 $ field 9 "Freed" <> freed
          ]
      where
        deleted = fromMaybe nosigH txidM
        freed = case [counted coins "coin" "coins" | coins > 0] <> [counted addrs "change address" "change addresses" | addrs > 0] of
          [] -> text "nothing"
          parts -> text (intercalate ", " parts)
    -- As it is, byte for byte, as signmsg --output writes it: verifymsg
    -- --input reads it back, and other wallets check the message exactly
    ResponseSignMsg m s a -> Text.putStr $ signedMessageText a s m
    -- In green when the signature is valid, in red otherwise
    ResponseVerifyMsg _ _ addr valid ->
      renderIO $
        if valid
          then styled Good ("The key of " <> cs addr <> " signed this message")
          else styled Failure ("This is not a signature of this message by the key of " <> cs addr)
    -- The networks left out last, where the user sees them
    ResponseSync results notServed notSynced _ ->
      renderIO . vcat . intersperse mempty $
        [syncResultsPrinter naming choice results | not (null results)]
          <> [notServedDoc notServed | not (null notServed)]
          <> [failedNetworksDoc "not synced" notSynced | not (null notSynced)]
    ResponseRecover searched gap walletM created results elsewhere notServed notSearched activeM hintM usedOnHidden ->
      renderIO . vcat . intersperse mempty $
        [vcat (recoveredDoc searched gap walletM created : [activeDoc active | Just active <- [activeM], not (null created)] <> [text (cs hint) | Just hint <- [hintM]])]
          <> [syncResultsPrinter naming choice results | not (null results)]
          <> [usedOnHiddenDoc naming usedOnHidden | not (null usedOnHidden)]
          <> [foreignFundsDoc choice elsewhere | not (null elsewhere)]
          <> [notServedDoc notServed | not (null notServed)]
          <> [failedNetworksDoc "left out" notSearched | not (null notSearched)]
    ResponseFees net rates -> do
      now <- getCurrentTime
      renderIO $ feeRatesDoc net now rates
  where
    -- An answer of several groups says where each is. The screen reads
    -- a listing chunk by chunk ('chunkDoc'), with the widths of the tags
    -- of the database: an answer printed whole here gives the columns of
    -- tags of a table the width of their heading.
    listing = mapM_ (renderIO . listingDoc) =<< sectionsPrinter choice view naming (TagWidths 0 0 0) (severalGroups answer) answer
    withSyncAfter after sent = vcat (intersperse mempty (sent : syncAfterDocs naming choice after))
