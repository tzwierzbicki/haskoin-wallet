{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The question before a deletion. A command that deletes from the database
-- for good asks first, and says what is about to go ('confirmDeletion').
-- The five commands whose name starts with delete do: deleteportfolio,
-- deletewallet, deletetx, deletetag and deletecontact, each with its
-- notice here. --yes answers for a script. A command that only takes
-- something off, such as untag or unlockcoin, asks nothing: the user puts
-- it back with one command.
module Haskoin.Wallet.Commands.Deletion
  ( confirmDeletion,
    deletePortfolioNotice,
    deleteWalletNotice,
    deleteTagNotice,
    deleteContactNotice,
    deleteTxNotice,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.Config
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.PendingTx
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.ShortId
import Haskoin.Wallet.Tags (ShownTag (..), tagText)
import Haskoin.Wallet.Tx.SignData

-- | Ask before a command deletes something from the database for good,
-- unless --yes. The five commands that start with delete call it:
-- deleteportfolio, deletewallet, deletetx, deletetag and deletecontact.
--
-- The names of some of them are close to those of harmless commands, as
-- deletetag is to untag. So each one first says what is about to go, in
-- the words of the notice given, and deletes only after a y or a yes. A
-- no stops the command with an error, and the database is as it was.
--
-- Each sentence of the notice has a line of its own, and the question
-- comes on the last line, where the answer is typed. The question of a
-- wallet behind the chain is laid out the same way
-- ('Haskoin.Wallet.Commands.Payment.confirmBehind').
--
-- A command calls it after its refusals and before it writes anything.
-- So a deletion that cannot happen is refused without a question, and a
-- no leaves no backup behind.
confirmDeletion :: Bool -> [String] -> String -> ExceptT String (DB IO) ()
confirmDeletion yes notice asked =
  unless yes $ do
    liftIO $ mapM_ (tell terminal) notice
    agreed <- askYes terminal (ScriptOption "--yes") asked
    unless agreed $ throwError "Nothing was deleted"

-- | The sentence of a notice that says whether hw backs up the database
-- before the deletion (@backUpBeforeLosing@)
backupFirstNotice :: Config -> String
backupFirstNotice cfg
  | configAutoBackupDatabase cfg = "hw copies the database and its labels into auto-backups first."
  | otherwise = "The database is not copied first, since autobackup-database is false in the configuration."

-- | A text of the user between double quotes, as the listings show a
-- label or a description
quoted :: Text -> String
quoted said = "\"" <> cs said <> "\""

-- | What deleteportfolio says before its question: the portfolio that
-- goes, with its description when it has one, that nothing goes with it,
-- and whether the database is copied first
deletePortfolioNotice :: Config -> Portfolio -> [String]
deletePortfolioNotice cfg portfolio =
  [portfolioText portfolio <> " will be deleted from the database."]
    <> ["Its description is " <> quoted description <> "." | not (Text.null description)]
    <> [ "It was never used, so no address, coin or transaction goes with it.",
         backupFirstNotice cfg
       ]
  where
    description = dBPortfolioDescription portfolio.portfolioRow

-- | What deletewallet says before its question: the wallet that goes and
-- how many portfolios it has, what tells it from another wallet, what
-- goes with it, that the funds are not touched, and whether the database
-- is copied first.
--
-- The description and the fingerprint are there for the user to check
-- that the index names the wallet they mean. An index alone is easy to
-- mistype.
deleteWalletNotice :: Config -> DBWallet -> Int -> [String]
deleteWalletNotice cfg wallet portfolioCount =
  [ name <> " will be deleted from the database, with " <> portfolios <> ".",
    identity,
    "Every address, label, transaction, coin and pending transaction of the wallet goes with it.",
    "The funds stay on the chain, and the mnemonic can still spend them.",
    backupFirstNotice cfg
  ]
  where
    name = cs (walletName (walletIndex wallet)) :: String
    portfolios
      | portfolioCount == 1 = "its portfolio"
      | otherwise = "its " <> show portfolioCount <> " portfolios"
    fingerprint = cs (fingerprintToText (walletFingerprint wallet)) :: String
    description = dBWalletDescription wallet
    identity
      | Text.null description = "Its fingerprint is " <> fingerprint <> "."
      | otherwise = "Its description is " <> quoted description <> ", and its fingerprint is " <> fingerprint <> "."

-- | What deletetag says before its question: the tag that goes, how many
-- addresses, transactions and contacts lose it, and the command that
-- takes a tag off one of them alone. deletetag is easy to take for
-- untag, so the notice names untag while there is still time. The
-- sentences read the same for one holder and for several.
--
-- A tag that nothing has is said to be so, and untag is left out: there
-- is nothing to take it off.
deleteTagNotice :: TagUse -> [String]
deleteTagNotice use =
  ("The tag " <> name <> " will be deleted from the whole database.") : holders <> unlocked
  where
    -- The coins the tag kept out of the payments are free once it is gone
    unlocked =
      [ "It locks its coins. Once it is deleted, any payment may spend the coins at its addresses."
        | use.tagUseLocksCoins
      ]
    name = cs (tagText (shownTag use.tagUseTag)) :: String
    counts =
      [ count <> " " <> (if held == 1 then one else several)
        | (held, one, several) <-
            [ (use.tagUseAddresses, "address", "addresses"),
              (use.tagUseTxs, "transaction", "transactions"),
              (use.tagUseContacts, "contact", "contacts")
            ],
          held > 0,
          let count = show held
      ]
    holders = case counts of
      [] -> ["No address, no transaction and no contact has it."]
      _ ->
        [ "It comes off " <> listed counts <> ", and no label or other tag changes.",
          "To take a tag off one address, transaction or contact and keep the tag, use untag instead."
        ]
    -- "a", "a and b", or "a, b and c"
    listed [one] = one
    listed parts = intercalate ", " (init parts) <> " and " <> last parts

-- | What deletecontact says before its question: the contact that goes,
-- with its label when it has one, that its tags stay in the database,
-- and what becomes of the transactions that paid it
deleteContactNotice :: Contact -> [String]
deleteContactNotice contact =
  ["The contact " <> cs (contactAddress contact) <> label <> " will be deleted from the address book."]
    <> ["Its tags come off it, and the tags themselves stay." | not (null (contactTags contact))]
    <> ["The transactions that paid the address stay in the history, and show it as the address of a stranger."]
  where
    label
      | Text.null (contactLabel contact) = ""
      | otherwise = " " <> quoted (contactLabel contact)

-- | What deletetx says before its question: the pending transaction that
-- goes, by the short id pendingtxs shows, where it stands, and what
-- becomes of its coins.
--
-- Where it stands tells the user what the deletion costs. An unsigned
-- transaction is prepared again in a moment. A signed one took the
-- mnemonic. One in conflict can only be deleted.
deleteTxNotice :: PendingTx -> [String]
deleteTxNotice held =
  [ "The pending transaction " <> shortHashText (shownHash tx) <> " will be deleted from the wallet, with its label and its tags.",
    standing,
    "The coins it spends can be spent again, and its change address is free again."
  ]
  where
    tx = txSignDataTx held.pendingTxData
    standing
      | held.pendingTxConflicted = "It is in conflict with another transaction, and can no longer be sent."
      | held.pendingTxOnline = "It was sent, and the server no longer knows it."
      | isSigned tx = "It is signed, and was not sent."
      | otherwise = "It is not signed."
