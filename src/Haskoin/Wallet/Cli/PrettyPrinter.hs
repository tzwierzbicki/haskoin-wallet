{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | The answers of the commands, as hw prints them for a person, and the
-- next steps of a payment. Each part of an answer names what it is with a
-- 'Style', as the help does: a field, an amount, an account, a command to
-- type. "Haskoin.Wallet.Cli.Style" says how each one looks, so that the help
-- and the answers speak the same colours.
--
-- An answer is a document of lines. A line that may be missing, such as
-- the label of an address, comes as a list of none or one line.
module Haskoin.Wallet.Cli.PrettyPrinter where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Char (isAlphaNum)
import Data.List (intercalate, intersperse, nub, sortOn)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (UTCTime, ZonedTime, diffUTCTime, getCurrentTime, utcToLocalZonedTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format
import Data.Word
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath, chainPath, coinBalanceNotSpendableYet, coinBalanceUnspendable, isIntPath, pathText)
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..), foreignKindDescription)
import Haskoin.Wallet.Labels (AccountImport (..), ImportReport (..), reportNeedsSync)
import Haskoin.Wallet.MessageSigning (signedMessageText)
import Haskoin.Wallet.Cli.Parser (AddressCode (..), BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..), TxShown (..), View (..), program)
import Haskoin.Wallet.Cli.Parser.Help (commandName)
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Cli.QrCode (paymentUri, qrRows)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Tx.BumpFee (PaidBy (..), Reduction (..), Sent, sentId)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SendAll (DustReport (..), inputFee, valueOf, worthSpendingUpTo)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, annotate, fillSep, flatAlt, group, hcat, hsep, indent, line, pretty, punctuate, vcat, (<+>))
import Text.DocLayout (charWidth, realLength)
import System.Exit
import System.IO

{- Rendering -}

-- | Print the document on stdout
renderIO :: Doc Style -> IO ()
renderIO = renderOn stdout

-- | Print the document on the handle: stdout for an answer, stderr for
-- what a command shows before it asks the user. It is styled on a
-- terminal, and plain otherwise.
renderOn :: Handle -> Doc Style -> IO ()
renderOn h doc = do
  output <- outputOf h
  putDoc h output doc

{- The parts of an answer -}

-- | Text as it is, unstyled
text :: String -> Doc Style
text = pretty

-- | Text in a style
styled :: Style -> String -> Doc Style
styled style = annotate style . pretty

-- | The name of a field padded to the width, with its colon, before its
-- value: "Fee      : "
field :: Natural -> Text -> Doc Style
field n name = styled Field (block n (cs name) <> ":") <> " "

-- | A text padded with spaces after it to the width given, in columns of
-- the terminal ('shownWidth')
block :: Natural -> String -> String
block n str =
  case n `safeSubtract` fromIntegral (shownWidth str) of
    Just missing -> str <> replicate (fromIntegral missing) ' '
    _ -> str

-- | A text padded with spaces before it to the width given, in columns of
-- the terminal ('shownWidth')
rblock :: Natural -> String -> String
rblock n str =
  case n `safeSubtract` fromIntegral (shownWidth str) of
    Just missing -> replicate (fromIntegral missing) ' ' <> str
    _ -> str

-- | How many columns of the terminal a text takes. A character of most
-- scripts takes one, one of the East Asian scripts or an emoji two, and a
-- combining accent none. Counting characters would put a cell after such
-- a text out of its column, so every padding and every cut counts
-- columns. A character of ambiguous width counts one, as most terminals
-- show it.
shownWidth :: String -> Int
shownWidth = realLength

-- | A count, such as of transactions or coins
countPrinter :: Natural -> Doc Style
countPrinter = text . show

{- Amounts -}

-- | An amount the wallet holds or gets, in green unless zero
amountPrinter :: AmountUnit -> Word64 -> Doc Style
amountPrinter unit = integerAmountPrinter unit . fromIntegral

-- | An amount coloured by its sign: green when it comes in, red when it
-- goes out
integerAmountPrinter :: AmountUnit -> Integer -> Doc Style
integerAmountPrinter unit amnt
  | amnt > 0 = amountIn (Just Incoming) unit amnt
  | amnt < 0 = amountIn (Just Outgoing) unit amnt
  | otherwise = amountIn Nothing unit amnt

-- | An amount neither in nor out, such as a fee: unstyled
neutralAmount :: AmountUnit -> Integer -> Doc Style
neutralAmount = amountIn Nothing

-- | The number of an amount in the style, if any, then its unit
amountIn :: Maybe Style -> AmountUnit -> Integer -> Doc Style
amountIn style unit amnt = maybe id annotate style (pretty (showAmount unit amnt)) <+> pretty (showUnit unit amnt)

{- Printing responses -}

{- Mnemonics -}

-- | A new mnemonic: what it is and where its entropy came from, then its
-- words
mnemonicDoc :: Text -> [Text] -> Doc Style
mnemonicDoc source ws = vcat [heading, wordsDoc ws]
  where
    heading = text ("Mnemonic of " <> show (length ws) <> " words,") <+> styled Muted ("from " <> cs source)

-- | The shares of a new backup: how many they are, of how many words, how
-- many of them rebuild the seed and where their entropy came from, then
-- each share under its number, a blank line between them
sharesDoc :: Text -> Natural -> [[Text]] -> Doc Style
sharesDoc source threshold shares =
  vcat . intersperse mempty $ heading : [vcat [text ("Share " <> show i), wordsDoc share] | (i, share) <- zip [1 :: Natural ..] shares]
  where
    heading = text ("Backup in " <> counted (fromIntegral (length shares)) "share" "shares" <> " of " <> show (maybe 0 length (listToMaybe shares)) <> " words, " <> needed <> ",") <+> styled Muted ("from " <> cs source)
    needed
      | length shares == 1 = "which is the whole backup"
      | otherwise = "any " <> show threshold <> " of them rebuild the seed"

-- | The words of a mnemonic, four on a line, each after its number, in
-- columns. A word of the BIP39 list has at most 8 letters.
wordsDoc :: [Text] -> Doc Style
wordsDoc ws = indent 2 . vcat $ cellsLine . map cell <$> chunksOf 4 (zip [1 :: Natural ..] ws)
  where
    cell (i, w) =
      let number = rblock 2 (show i) <> "."
       in (12, number <> " " <> cs w, styled Field number <+> styled Secret (cs w))

{- Accounts -}

-- | An account as a listing shows it, in the view asked for, the time it
-- was created in the time zone of the computer
listedAccountDoc :: View -> AmountUnit -> DBAccount -> Word64 -> IO (Doc Style)
listedAccountDoc view unit acc spendable = do
  created <- utcToLocalZonedTime (dBAccountCreated acc)
  return $ case view of
    Compact -> compactAccountDoc unit acc spendable
    Verbose -> verboseAccountDoc unit created acc spendable
    -- The table needs every account for the width of its names:
    -- rowsPrinter lays it out ('accountRowDoc')
    Table -> compactAccountDoc unit acc spendable

-- | The first line of an account: its name, its network and its
-- derivation path, the three things deleteacc asks for
accountLine :: DBAccount -> Doc Style
accountLine acc =
  columns
    [ pretty (dBAccountName acc) `withStyle` Account,
      text (accountNetwork acc).name,
      text (pathToStr (accountPath acc))
    ]

-- | An account in two lines: its first line, then what it can spend now,
-- what is unconfirmed when anything is, and how many coins it holds
compactAccountDoc :: AmountUnit -> DBAccount -> Word64 -> Doc Style
compactAccountDoc unit acc spendable =
  vcat [accountLine acc, indent 2 (commaList (spendable' : unconfirmed <> [coins]))]
  where
    spendable' = "spendable" <+> amountPrinter unit spendable
    -- An unconfirmed payment from another wallet cannot be spent yet
    unconfirmed = ["unconfirmed" <+> waitingAmount unit v | let v = dBAccountBalanceUnconfirmed acc, v > 0]
    coins = text (counted (fromIntegral (dBAccountBalanceCoins acc)) "coin" "coins")

-- | An account with every detail: its first line, then its fields
verboseAccountDoc :: AmountUnit -> ZonedTime -> DBAccount -> Word64 -> Doc Style
verboseAccountDoc unit created acc spendable =
  vcat $
    accountLine acc
      : ( indent 2
            <$> [ field 11 "Fingerprint" <> maybe (styled Muted "unknown") pretty (dBAccountFingerprint acc),
                  field 11 "Addresses" <> text (show (dBAccountExternal acc) <> " receiving, " <> show (dBAccountInternal acc) <> " change"),
                  field 11 "Created" <> text (secondText created),
                  field 11 "Confirmed" <> amountPrinter unit (dBAccountBalanceConfirmed acc),
                  field 11 "Unconfirmed" <> waitingAmount unit (dBAccountBalanceUnconfirmed acc),
                  field 11 "Spendable" <> amountPrinter unit spendable,
                  field 11 "Coins" <> countPrinter (fromIntegral (dBAccountBalanceCoins acc))
                ]
        )

-- | The descriptor of an account as descriptor shows it: the descriptor
-- alone on its first line, to copy into another wallet, then its account
-- and network, that the fingerprint of the seed is unknown when it is,
-- since the descriptor then names no origin, and the key with the prefix
-- of SLIP-132 when the account has one, a zpub for a bip84 account, to
-- copy into a wallet that takes only that form
descriptorDoc :: DBAccount -> Text -> Maybe Text -> Doc Style
descriptorDoc acc descriptor slip132 =
  vcat $
    text (cs descriptor)
      : ( indent 2
            <$> [ field 11 "Account" <> pretty (dBAccountName acc) `withStyle` Account,
                  field 11 "Network" <> text (accountNetwork acc).name
                ]
              <> [field 11 "Fingerprint" <> styled Muted "unknown" | isNothing (dBAccountFingerprint acc)]
              <> [field 11 "SLIP-132" <> text (cs key) | Just key <- [slip132]]
        )

-- | The document in the style
withStyle :: Doc Style -> Style -> Doc Style
withStyle doc style = annotate style doc

-- | Cells side by side, two spaces apart
columns :: [Doc Style] -> Doc Style
columns = hcat . intersperse "  "

-- | Items one after the other, a comma between them
commaList :: [Doc Style] -> Doc Style
commaList = hcat . intersperse ", "

-- | A count with its noun: "no coins", "1 coin", "3 coins"
counted :: Natural -> String -> String -> String
counted 0 _ plural = "no " <> plural
counted 1 singular _ = "1 " <> singular
counted n _ plural = show n <> " " <> plural

{- Syncs -}

-- | The accounts synced, each with the results of its sync, a blank line
-- between them
syncResultsPrinter :: AmountUnit -> [SyncRes] -> Doc Style
syncResultsPrinter unit results = vcat . intersperse mempty $ syncResultDoc unit <$> results

-- | An account synced: its two lines as accounts shows them, the block it
-- is synced to, and what changed, one kind of change a line. What did not
-- change is left out, and a sync that changed nothing says so.
syncResultDoc :: AmountUnit -> SyncRes -> Doc Style
syncResultDoc unit result =
  vcat $ compactAccountDoc unit (syncResAccount result) (syncResSpendable result) : (indent 2 <$> synced : changes)
  where
    synced
      | null changes = "synced at block" <+> height <> ", nothing new"
      | otherwise = "synced at block" <+> height
    height = text (show (syncResBlockHeight result))
    changes = updated <> sentAgain <> conflicted <> found
    updated = case (syncResTxUpdates result, syncResCoinUpdates result) of
      (0, 0) -> []
      (t, 0) -> [text (counted t "transaction" "transactions") <+> "updated"]
      (0, c) -> [text (counted c "coin" "coins") <+> "updated"]
      (t, c) -> [text (counted t "transaction" "transactions" <> " and " <> counted c "coin" "coins") <+> "updated"]
    sentAgain = [text (counted r "transaction" "transactions") <+> "sent again" | let r = syncResRebroadcast result, r > 0]
    -- A conflicted transaction can no longer be sent
    conflicted = [styled Failure (counted k "pending transaction" "pending transactions" <> " conflicted") | let k = syncResConflicted result, k > 0]
    found = [text (counted n "new address" "new addresses") <+> "found" | let n = syncResNewAddrs result, n > 0]

-- | The accounts the sync left out, each with the failure of its store, a
-- blank line between them
notSyncedPrinter :: [(DBAccount, Text)] -> Doc Style
notSyncedPrinter accs = vcat . intersperse mempty $ notSyncedDoc <$> accs

notSyncedDoc :: (DBAccount, Text) -> Doc Style
notSyncedDoc (acc, failure) =
  vcat [accountLine acc, indent 2 (styled Failure "not synced:" <+> pretty failure)]

-- | The funds of a seed at accounts hw does not hold, as recover shows
-- them: a line for each account, with its path, its kind, how many of its
-- addresses were used and what they hold, then what to do with the funds
foreignFundsDoc :: AmountUnit -> Network -> [ForeignFunds] -> Doc Style
foreignFundsDoc unit net elsewhere =
  vcat $
    "The seed was paid at paths hw cannot spend from:"
      : (indent 2 . fundsLine <$> elsewhere)
      <> [indent 2 "A wallet that holds such accounts can move the funds to an address of this wallet."]
  where
    pathOf = pathToStr . foreignPath net
    width = fromIntegral $ maximum (0 : (length . pathOf <$> elsewhere))
    fundsLine funds =
      text (block width (pathOf funds))
        <> "  "
        <> text (foreignKindDescription (foreignKind funds) <> ": " <> counted (foreignUsed funds) "address" "addresses" <> " used, ")
        <> left (foreignBalance funds)
    left 0 = "nothing left"
    left balance = amountPrinter unit balance <+> "left"

{- Imports -}

-- | What importacc did with a file: each account the file names, with what
-- became of it, a blank line between them, then the labels and locks set,
-- what the file labels that the wallet does not have yet, with the
-- addresses among them that hw would not derive from their key path, what
-- hw keeps no label for, and the lines that are not records
importDoc :: ImportReport -> Doc Style
importDoc report = vcat . intersperse mempty $ (accountImportDoc <$> reportAccounts report) <> [vcat summary]
  where
    summary =
      [labelsLine]
        <> [text ("Coins locked: " <> show n) | let n = reportCoinsLocked report, n > 0]
        <> [text ("Coins unlocked: " <> show n) | let n = reportCoinsUnlocked report, n > 0]
        <> ["Not in the wallet yet:" <+> commaList (text <$> missing) | not (null missing)]
        <> [indent 2 (sentence (wordsOf (tooFar n))) | let n = reportAddressesTooFar report, n > 0]
        <> ["Left aside:" <+> commaList (text <$> aside) | not (null aside)]
        <> [styled Failure "Lines that are not records:" <+> text (intercalate ", " (show <$> reportUnreadable report)) | not (null (reportUnreadable report))]
    labelsLine = case countedIf (reportAddressLabels report) "address" "addresses" <> countedIf (reportTxLabels report) "transaction" "transactions" of
      [] -> "Labels set:" <+> styled Muted "none"
      parts -> "Labels set:" <+> commaList (text <$> parts)
    missing =
      countedIf (reportAddressesMissing report) "address" "addresses"
        <> countedIf (reportTxsMissing report) "transaction" "transactions"
        <> countedIf (reportCoinsMissing report) "coin" "coins"
    aside =
      [counted n "record" "records" <> " of accounts the wallet does not have" | let n = reportOtherAccounts report, n > 0]
        <> [counted n "label" "labels" <> " of change addresses" | let n = reportChangeAddresses report, n > 0]
        <> [counted n "record" "records" <> " hw keeps no label for" | let n = reportNotKept report, n > 0]
    -- A count with its noun, when there is anything to count
    countedIf n singular plural = [counted n singular plural | n > 0]
    -- Why hw derived none of these addresses from their key path: a file
    -- names any path, and hw generates no further than the scan limit
    -- past the last used address of a chain
    tooFar n = "Of them, " <> counted n "address lies" "addresses lie" <> " over a thousand past the last used address of " <> (if n == 1 then "its" else "their") <> " chain, further than hw generates on the word of a file."

-- | An account a file names, as accounts shows it, and what became of it.
-- Under a created one, the note on a kind taken on trust, if any, wrapped
-- at the width of the screen.
accountImportDoc :: AccountImport -> Doc Style
accountImportDoc = \case
  AccountCreated acc noteM -> vcat ([accountLine acc, indent 2 (styled Good "created")] <> [indent 2 (sentence (wordsOf (cs note))) | Just note <- [noteM]])
  AccountKept acc Nothing -> vcat [accountLine acc, indent 2 "already in the wallet"]
  AccountKept acc (Just why) -> vcat [accountLine acc, indent 2 ("already in the wallet, and keeps its name:" <+> pretty why)]
  AccountRenamed was acc -> vcat [accountLine acc, indent 2 ("renamed from" <+> pretty was `withStyle` Account)]
  -- The name of a refused account is the label of a record, or the start
  -- of a line, of the file
  AccountRefused name why -> vcat [pretty (visibleText name) `withStyle` Account, indent 2 (styled Failure "not imported:" <+> pretty why)]

{- Addresses -}

-- | An address as addresses lists it, in the view asked for, the time it
-- was created in the time zone of the computer. Its path is padded to the
-- given width, that of the longest path listed ('pathWidth').
listedAddressDoc :: View -> AmountUnit -> Natural -> DBAddress -> Word64 -> IO (Doc Style)
listedAddressDoc view unit pad addr spendable = do
  created <- utcToLocalZonedTime (dBAddressCreated addr)
  return $ case view of
    Compact -> compactAddressDoc unit pad addr spendable
    Verbose -> verboseAddressDoc unit pad created addr spendable
    -- The table needs the account for the width of its addresses:
    -- rowsPrinter lays it out ('addressRowDoc')
    Table -> compactAddressDoc unit pad addr spendable

-- | The first line of an address: its derivation path, which tells
-- receiving and change addresses apart, padded to the width given, then
-- the address and its label. In a listing of addresses of both kinds, a
-- change address says so in place of a label, which it never has.
addressLine :: Bool -> Natural -> DBAddress -> Doc Style
addressLine mixed pad addr =
  columns $
    [text (block pad (cs (pathText (addrPath addr)))), pretty (dBAddressAddress addr)]
      <> [labelWord label | let label = dBAddressLabel addr, not (Text.null label)]
      <> [changeWord | mixed, dBAddressInternal addr]

-- | What marks a change address where a label would go: the user cannot
-- label one, and hw knows which addresses are change
changeWord :: Doc Style
changeWord = styled Muted "change address"

-- | An address in two lines: its first line, then what it can spend now
-- and what it received in how many transactions, or that it is unused.
-- addresses lists one kind of address at a time, so none is marked.
compactAddressDoc :: AmountUnit -> Natural -> DBAddress -> Word64 -> Doc Style
compactAddressDoc unit pad addr spendable =
  vcat [addressLine False pad addr, indent 2 activity]
  where
    txs = fromIntegral (dBAddressBalanceTxs addr)
    activity
      | txs == 0 = styled Muted "unused"
      | otherwise =
          commaList
            [ "spendable" <+> amountPrinter unit spendable,
              "received" <+> amountPrinter unit (dBAddressBalanceReceived addr) <+> "in" <+> text (counted txs "transaction" "transactions")
            ]

-- | An address with every detail: its first line, then its fields
verboseAddressDoc :: AmountUnit -> Natural -> ZonedTime -> DBAddress -> Word64 -> Doc Style
verboseAddressDoc unit pad created addr spendable =
  vcat $
    addressLine False pad addr
      : ( indent 2
            <$> [ field 12 "Transactions" <> countPrinter (fromIntegral (dBAddressBalanceTxs addr)),
                  field 12 "Received" <> amountPrinter unit (dBAddressBalanceReceived addr),
                  field 12 "Confirmed" <> amountPrinter unit (dBAddressBalanceConfirmed addr),
                  field 12 "Unconfirmed" <> waitingAmount unit (dBAddressBalanceUnconfirmed addr),
                  field 12 "Spendable" <> amountPrinter unit spendable,
                  field 12 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr)),
                  field 12 "Created" <> text (secondText created)
                ]
        )

-- | An address as an answer shows it, with its QR code when the command
-- gives the address out and someone can scan the screen
addressAnswerDoc :: AddressCode -> AmountUnit -> DBAccount -> DBAddress -> Word64 -> IO (Doc Style)
addressAnswerDoc code unit acc addr spendable = do
  details <- listedAddressDoc Verbose unit (pathWidth [addr]) addr spendable
  terminal <- hIsTerminalDevice stdout
  let rows
        | code == WithCode && terminal = fromMaybe [] (qrRows (paymentUri (accountNetwork acc) (dBAddressAddress addr)))
        | otherwise = []
  return (withCodeDoc details rows)

-- | Text, then a QR code drawn in the rows given. The code goes under the
-- text and never beside it: a terminal wraps its long lines again when it
-- is resized, which would mix the text into the code.
withCodeDoc :: Doc Style -> [Text] -> Doc Style
withCodeDoc details rows = vcat (details : (annotate QrCode . pretty <$> rows))

-- | The address at a path of the account, as checkaddr shows it: its
-- path, the address and its label when the wallet has one, as the first
-- line of addresses, then its account and network. The user compares the
-- address with the one the online computer showed.
checkAddrDoc :: DBAccount -> SoftPath -> Text -> Text -> Doc Style
checkAddrDoc acc path addr label =
  vcat
    [ columns $ [text (cs (pathText path)), pretty addr] <> [labelWord label | not (Text.null label)],
      indent 2 (accountField acc),
      indent 2 (field 9 "Network" <> text (accountNetwork acc).name)
    ]

-- | The width of the longest derivation path of the addresses listed
pathWidth :: [DBAddress] -> Natural
pathWidth addrs = fromIntegral $ maximum (0 : (Text.length . pathText . addrPath <$> addrs))

-- | An address as balances lists it, in the view asked for. The table
-- needs the account for the widths of its columns: rowsPrinter lays it
-- out ('balanceRowDoc').
listedBalanceDoc :: View -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> Doc Style
listedBalanceDoc view unit pad addr balance = case view of
  Verbose -> verboseBalanceDoc unit pad addr balance
  _ -> coinBalanceDoc unit pad addr balance

-- | An address as balances shows it: its first line, with a change
-- address marked as one among the receiving addresses, then the value of
-- its coins, what it can spend now, and the rest, which waits
coinBalanceDoc :: AmountUnit -> Natural -> DBAddress -> CoinBalance -> Doc Style
coinBalanceDoc unit pad addr balance =
  vcat
    [ addressLine True pad addr,
      indent 2 $
        commaList
          [ "total" <+> amountPrinter unit (coinBalanceTotal balance),
            "spendable" <+> amountPrinter unit (coinBalanceSpendable balance),
            "unspendable" <+> waitingAmount unit (coinBalanceUnspendable balance)
          ]
    ]

-- | An address as balances --verbose shows it: its first line, then its
-- fields. The value of its coins and how many there are, then what it can
-- spend now, and what it cannot, split as coins tells the state of each
-- coin: locked by a pending transaction, locked by the user, or not
-- spendable yet. The compact view gives the sum of the three; this one
-- tells which to wait for, and which to unlock.
verboseBalanceDoc :: AmountUnit -> Natural -> DBAddress -> CoinBalance -> Doc Style
verboseBalanceDoc unit pad addr balance =
  vcat $
    addressLine True pad addr
      : ( indent 2
            <$> [ field 17 "Total" <> amountPrinter unit (coinBalanceTotal balance),
                  field 17 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr)),
                  field 17 "Spendable" <> amountPrinter unit (coinBalanceSpendable balance),
                  field 17 "Locked by tx" <> waitingAmount unit (coinBalanceLockedByTx balance),
                  field 17 "Locked by you" <> waitingAmount unit (coinBalanceLockedByUser balance),
                  field 17 "Not spendable yet" <> waitingAmount unit (coinBalanceNotSpendableYet balance)
                ]
        )

-- | An amount that waits to be spent: in the colour of what waits, unless
-- it is zero. Unconfirmed amounts take it too: what another wallet pays
-- cannot be spent before it confirms, and the wallet does not tell its
-- own unconfirmed coins apart in a balance.
waitingAmount :: AmountUnit -> Word64 -> Doc Style
waitingAmount unit 0 = neutralAmount unit 0
waitingAmount unit v = amountIn (Just Waiting) unit (fromIntegral v)

-- | Addresses as balances lists them, their paths aligned, a blank line
-- between them. preparetx --change-back shows the addresses it can send
-- the change back to this way.
coinBalancesPrinter :: AmountUnit -> [(DBAddress, CoinBalance)] -> Doc Style
coinBalancesPrinter unit addrs = vcat . intersperse mempty $ uncurry (coinBalanceDoc unit (pathWidth (fst <$> addrs))) <$> addrs

{- Coins -}

-- A coin shows in one of three views. The compact and the verbose views
-- start with the same first line: the time of the transaction that paid
-- it, its value, its state and its confirmations, as a transaction starts
-- with its time. Only the count of the confirmations differs: the compact
-- view stops it at 999+, and the verbose view gives it in full. The table
-- gives a coin one row ('coinRowDoc').

-- | A coin as a listing shows it, in the view asked for, with the time of
-- its transaction in the time zone of the computer. lockcoin and
-- unlockcoin answer with the verbose view, as the commands that answer
-- with one element do.
listedCoinDoc :: View -> Network -> AmountUnit -> JsonCoin -> IO (Doc Style)
listedCoinDoc view net unit coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  return $ case view of
    Compact -> compactCoinDoc net unit time coin
    Verbose -> verboseCoinDoc net unit time coin
    -- The table needs the account for the width of its paths: rowsPrinter
    -- lays it out ('coinRowDoc')
    Table -> compactCoinDoc net unit time coin

-- | The first line of a coin: the time of the transaction that paid it,
-- its value, then its state and its confirmations, written by the
-- function given. The time and the value keep the columns of the first
-- line of a transaction, so that the values of a listing line up.
coinLine :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinLine countText unit time coin =
  columns
    [ minuteWord time,
      amountCell (Just Incoming) "" unit (fromIntegral (jsonCoinValue coin)),
      commaList [coinState "locked by a pending transaction" coin, confirmations]
    ]
  where
    confirmations
      | jsonCoinConfirmations coin == 0 = styled Waiting "unconfirmed"
      | otherwise = text (countText (jsonCoinConfirmations coin))

-- | Whether a coin can be spent now, in the colour of its state: spendable,
-- locked by a pending transaction or by the user, or waiting for a
-- confirmation. The words for a coin a pending transaction spends are
-- given: the compact view spells them out, and the table has little room.
coinState :: String -> JsonCoin -> Doc Style
coinState lockedByTransaction coin = case jsonCoinLock coin of
  Just LockedByTransaction -> styled Waiting lockedByTransaction
  Just LockedByUser -> styled Waiting "locked by you"
  Nothing
    | jsonCoinSpendable coin -> styled Good "spendable"
    | otherwise -> styled Waiting "not spendable yet"

-- | The address that holds a coin, as both views give it: in full, then
-- its path, with its label, or the mark of a change address, on the line
-- below
coinAddressDoc :: Network -> JsonCoin -> Doc Style
coinAddressDoc net coin =
  withNotesBelow
    (fullAddressWords net (jsonCoinAddress coin) (jsonCoinPath coin))
    (ownAddressNoteBelow (jsonCoinAddressLabel coin) (jsonCoinChange coin))

-- | A coin in three lines: its first line, its confirmations counted up to
-- 999+, the address that holds it, and the coin itself, as lockcoin and
-- --coin take it. The address is in full, then its path, and its label
-- on the line below ('coinAddressDoc'), which makes four lines. It is the
-- one compact view that gives an address of the wallet in full: what
-- tells a coin apart is the address that holds it, which another wallet
-- or a block explorer shows, more than its path. The label of the address
-- stays, since a user may sort coins by the labels of their addresses.
-- The label of the transaction that paid the coin says less of the coin,
-- and only the verbose view gives it.
compactCoinDoc :: Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
compactCoinDoc net unit time coin =
  vcat
    [ coinLine cappedConfirmations unit time coin,
      indent 2 ("at" <+> coinAddressDoc net coin),
      indent 2 (styled Muted (coinText (jsonCoinOutpoint coin)))
    ]

-- | A coin with every detail: its first line, with the full count of its
-- confirmations, then its fields. The label of its transaction comes
-- first, as in the verbose view of a transaction, then the coin, the
-- block that holds it, the time of its transaction to the second with its
-- time zone, and its address in full with its path. The label of the
-- address, or the mark of a change address, is on the line below the
-- address, as in the verbose view of a transaction. An unconfirmed coin
-- is in no block yet, and shows none.
verboseCoinDoc :: Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
verboseCoinDoc net unit time coin =
  vcat $
    coinLine fullConfirmations unit time coin
      : ( indent 2
            <$> [field 7 "Label" <> labelWord label | let label = jsonCoinTxLabel coin, not (Text.null label)]
              <> [field 7 "Coin" <> text (coinText (jsonCoinOutpoint coin))]
              <> [field 7 "Block" <> text (show height) | Store.BlockRef height _ <- [jsonCoinBlock coin]]
              <> [field 7 "Time" <> text (secondText t) | Just t <- [time]]
              <> [field 7 "Address" <> coinAddressDoc net coin]
        )

{- Transactions -}

-- A transaction shows in one of three views. The compact and the verbose
-- views start with the same first line: its time, its type, its amount and
-- where it stands. Only the count of the confirmations differs: the compact
-- view stops it at 999+, and the verbose view gives it in full. The
-- compact view then gives the balance after it, in txs, then whom it pays
-- and its hash. It names an address of the wallet by its path. The
-- verbose view gives every detail below the first line, as fields, every
-- address in full. The table gives the first line and the balance, on one
-- row, in columns under a header, and what the transaction is for
-- ('purposeCell').

-- | A transaction as a listing shows it, in the view asked for, with its
-- time in the time zone of the computer. A listing of every account names
-- the account of each transaction. The balance after it is the one txs
-- gives: the other listings have none.
listedTxDoc :: View -> Network -> AmountUnit -> Maybe DBAccount -> Maybe Integer -> TxInfo -> IO (Doc Style)
listedTxDoc view net unit accountM balanceM info = do
  time <- localTxTime info
  return $ case view of
    Compact -> compactTxDoc net unit accountM balanceM time info
    Verbose -> verboseTxDoc net unit [accountField acc | Just acc <- [accountM]] balanceM time info
    Table -> txRowDoc net unit balanceM time info

-- | The account of a transaction, as a field of the verbose view
accountField :: DBAccount -> Doc Style
accountField acc = field 9 "Account" <> pretty (dBAccountName acc) `withStyle` Account

-- | The one transaction a command answers with, with every detail, naming
-- its account when it is one of the wallet, and its network. The user sees
-- whose coins it moves, and which coins: private keys, coins and the legacy
-- addresses of Bitcoin Cash read the same on Bitcoin, and a sweep outside
-- the wallet is on the network of its file.
txAnswerDoc :: Network -> Maybe DBAccount -> AmountUnit -> TxInfo -> IO (Doc Style)
txAnswerDoc net accountM unit info = do
  time <- localTxTime info
  return $ verboseTxDoc net unit named Nothing time info
  where
    named = [accountField acc | Just acc <- [accountM]] <> [field 9 "Network" <> text net.name]

-- | The time a transaction shows: when the store first saw it, for one on
-- the chain, or when the wallet started holding it, for one in
-- preparation. None when hw does not know it: for a summary the next sync
-- has to fetch again, or a transaction read from a file.
txTime :: TxInfo -> Maybe UTCTime
txTime info = case txInfoStatus info of
  OnChain status -> chainTime status
  Pending status -> pendingCreated status

-- | That time in the time zone of the computer, as the clock read it then.
-- A transaction of the summer shows summer time, even in winter.
localTxTime :: TxInfo -> IO (Maybe ZonedTime)
localTxTime = traverse utcToLocalZonedTime . txTime

-- | A time to the minute, as the first line of a transaction shows it:
-- "31 Oct 2026 09:12", always 'minuteWidth' characters
minuteText :: ZonedTime -> String
minuteText = formatTime defaultTimeLocale "%d %b %Y %H:%M"

minuteWidth :: Natural
minuteWidth = 17

-- | A time to the minute, as a first line starts with it, in a column of
-- 'minuteWidth' characters: "31 Oct 2026 09:12", or "time unknown",
-- faint, when hw does not know the time
minuteWord :: Maybe ZonedTime -> Doc Style
minuteWord = maybe (styled Muted (block minuteWidth "time unknown")) (text . minuteText)

-- | A time to the second, with its time zone: "31 Oct 2026 09:12:40 CET"
secondText :: ZonedTime -> String
secondText = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %Z"

-- | The first line of a transaction: its time, its type, what it adds to
-- the account or takes from it, fee included, and where it stands. Its
-- columns line up from one transaction to the next. The function given
-- writes the confirmations: 'cappedConfirmations' in the compact view,
-- 'fullConfirmations' in the verbose view.
headlineDoc :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
headlineDoc countText unit time (TxInfo summary status _ _) =
  hcat
    [ minuteWord time,
      "  ",
      text (typeWord (summaryType summary)),
      "  ",
      amountColumn unit (summaryAmount summary),
      "  ",
      statusWords countText (summaryFeeByte summary) status
    ]

-- | The type of a transaction. The three words have the same length,
-- which keeps the amounts in a column. They hold before a transaction is
-- sent as well as after, unlike "sent" and "received".
typeWord :: TxType -> String
typeWord = \case
  TxDebit -> "Outgoing"
  TxCredit -> "Incoming"
  TxInternal -> "Internal"

-- | The length of the three words of 'typeWord'
typeWidth :: Natural
typeWidth = 8

-- | What a transaction adds to the account or takes from it, with its
-- sign, green when it comes in and red when it goes out
amountColumn :: AmountUnit -> Integer -> Doc Style
amountColumn unit amnt = amountCell style sign unit amnt
  where
    (style, sign) = changeSign amnt

-- | The style and the sign before the number of what a transaction adds
-- to the account or takes from it: green with a plus when it comes in,
-- red when it goes out, where the number has its minus already
changeSign :: Integer -> (Maybe Style, String)
changeSign amnt
  | amnt > 0 = (Just Incoming, "+")
  | amnt < 0 = (Just Outgoing, "")
  | otherwise = (Nothing, "")

-- | An amount in a column: its number after the sign given, right-aligned
-- in a column wide enough for 9 999 bitcoins in any unit, in the style if
-- any, then its unit padded to its plural, so that the amounts of a
-- listing line up. A larger amount pushes the rest of its line.
amountCell :: Maybe Style -> String -> AmountUnit -> Integer -> Doc Style
amountCell style sign unit amnt =
  maybe id annotate style (amountNumber sign unit amnt)
    <+> pretty (block unitWidth (cs (showUnit unit amnt)))
  where
    unitWidth = fromIntegral (Text.length (showUnit unit 2))

-- | The number of an amount, as it stands in the column of the amounts
amountNumber :: String -> AmountUnit -> Integer -> Doc Style
amountNumber sign unit amnt = pretty (rblock amountWidth (sign <> cs (showAmount unit amnt)))

-- | The width of the number of an amount in a column: that of 9 999
-- bitcoins with their sign in any unit, such as "-9999.00000000". More
-- is rare in a wallet. An amount that does not fit pushes the rest of its
-- line, rather than every line making room for 21 million bitcoins.
amountWidth :: Natural
amountWidth = 14

-- | The balance of the account after a transaction, on the line below its
-- first line: its word under the type and its number under the amount, so
-- that the eye goes from what changed to what is left. Nothing follows
-- the unit, which needs no padding here.
balanceLine :: AmountUnit -> Integer -> Doc Style
balanceLine unit balance =
  hcat
    [ pretty (block minuteWidth ""),
      "  ",
      pretty (block typeWidth "Balance"),
      "  ",
      amountNumber "" unit balance,
      " ",
      pretty (showUnit unit balance)
    ]

-- | Where a transaction stands, in a few words. One that waits shows its
-- fee rate, which tells how soon a block may take it: unconfirmed, or in
-- preparation. One in conflict can no longer be sent, and a dead one was
-- replaced or dropped from the network: their fee rates no longer matter.
-- The function given writes the confirmations of one in a block.
statusWords :: (Natural -> String) -> Natural -> TxStatus -> Doc Style
statusWords countText feeByte = \case
  OnChain status
    | chainDead status -> styled Failure "dead"
    | chainConfirmations status == 0 -> styled Waiting "unconfirmed" <> rate
    | otherwise -> text (countText (chainConfirmations status))
  Pending status
    | pendingConflicted status -> styled Failure "conflicted"
    | pendingOnline status -> styled Good "sent" <> rate
    | pendingSigned status -> styled Waiting "signed, not sent" <> rate
    | otherwise -> styled Waiting "not signed" <> rate
  where
    rate = ", " <> text (show feeByte <> " sat/vB")

-- | A number of confirmations in full: "1 confirmation", "1234
-- confirmations". The verbose views give it, since they give every
-- detail.
fullConfirmations :: Natural -> String
fullConfirmations n = counted n "confirmation" "confirmations"

-- | A number of confirmations up to 999: "1 confirmation", "998
-- confirmations", and "999+ confirmations" for more. The compact views
-- give it. Past a few blocks, how many more no longer matters, and a
-- count that grows without end would push the rest of a line. The
-- verbose views give the full count, and the JSON answers the number.
cappedConfirmations :: Natural -> String
cappedConfirmations n
  | n > countCap = cappedCount n <> " confirmations"
  | otherwise = fullConfirmations n

-- | A number up to 'countCap', alone: "998", and "999+" for more. The
-- table of txs gives the confirmations this way, under a heading that
-- names them.
cappedCount :: Natural -> String
cappedCount n
  | n > countCap = show countCap <> "+"
  | otherwise = show n

-- | The largest number of confirmations every view but the verbose one
-- counts to
countCap :: Natural
countCap = 999

-- | A transaction in a few lines: its first line, the balance after it if
-- the listing gives one, whom it pays, and the hash to name it by. A
-- transaction the user labelled shows its label on a line of its own,
-- above whom it pays: the label says what the transaction is for, better
-- than any of its numbers. A replacement that bumpfee prepared also names
-- the transactions it replaces, before its hash.
compactTxDoc :: Network -> AmountUnit -> Maybe DBAccount -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
compactTxDoc net unit accountM balanceM time info@(TxInfo summary status label _) =
  vcat $
    [headlineDoc cappedConfirmations unit time info]
      <> [balanceLine unit balance | Just balance <- [balanceM]]
      <> [indent 2 (labelWord label) | not (Text.null label)]
      <> (indent 2 <$> payeeLine net accountM summary)
      <> [indent 2 ("replaces" <+> text (cs (txHashToHex replaced))) | Pending p <- [status], replaced <- pendingReplaces p]
      <> [indent 2 (hashWord status)]

-- | Whom a transaction pays, in one line: the largest output of those it
-- sends to, with its label, and how many more there are. A payment the
-- wallet receives is paid "at" an address of the wallet, the others "to"
-- an address. An address of the wallet is named by its path. The account
-- comes first when the listing has every account's. None for a
-- transaction that pays no address and names no account.
payeeLine :: Network -> Maybe DBAccount -> TxSummary -> [Doc Style]
payeeLine net accountM summary =
  [hcat (intersperse ", " parts) | not (null parts)]
  where
    parts =
      ["account" <+> pretty (dBAccountName acc) `withStyle` Account | Just acc <- [accountM]]
        <> [payeeWords net payee | Just payee <- [mainPayee summary]]

-- | The address a transaction pays the most, with the word before it and
-- how many other addresses it pays
data Payee = Payee
  { -- | "at" for a payment the wallet receives, which it is paid at, and
    -- "to" for the others
    payeePreposition :: !Text,
    payeeRow :: !TxAddress,
    payeeOthers :: !Int
  }

-- | The address a transaction pays the most, of those it sends to: the
-- address of another wallet for a payment, one of the wallet for a
-- payment received or one between its addresses. None for a transaction
-- that pays no address.
mainPayee :: TxSummary -> Maybe Payee
mainPayee summary =
  case sortOn (Down . txAddressValue) (fst (paidTo summary)) of
    [] -> Nothing
    largest : others -> Just (Payee preposition largest (length others))
  where
    preposition = if summaryType summary == TxCredit then "at" else "to"

-- | Whom a transaction pays, as the compact view says it: "at /0/3
-- \"Rent\"", "to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 2 more". An
-- address of the wallet is named by its path, with its label or the mark
-- of a change address. The address of another wallet is in full.
payeeWords :: Network -> Payee -> Doc Style
payeeWords net (Payee preposition row others) =
  hsep $ [text (cs preposition), target] <> note <> more
  where
    target = case txAddressOwn row of
      Just (path, _) -> ownPathWord path
      Nothing -> text (addrToString net (txAddress row))
    -- A path is short: its label stays on its line
    note = case txAddressOwn row of
      Just (path, label) -> ownAddressNote label (isIntPath path)
      Nothing -> []
    more = if others > 0 then ["and", text (show others), "more"] else []

{- The addresses of the wallet -}

-- An address of the wallet is blue, with its label ('OwnAddress'), so
-- that it stands apart from the addresses of others in a transaction, and
-- so that a label on the line below its address is seen to go with it.
-- The compact views and the table name it by its derivation path, which is
-- short and says which address of the account it is. The verbose views
-- give it in full, with its path: the address is what another wallet or a
-- block explorer shows. The compact view of a coin does too, since the
-- address is what tells a coin apart ('compactCoinDoc').

-- | The derivation path of an address of the wallet, in its colour
ownPathWord :: SoftPath -> Doc Style
ownPathWord path = styled OwnAddress (pathToStr path)

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

-- | An address of the wallet in full, in its colour, then its derivation
-- path, if known, faint: the path adds to the address, which is what
-- another wallet or a block explorer shows
fullAddressWords :: Network -> Address -> Maybe SoftPath -> Doc Style
fullAddressWords net addr pathM = columns (ownAddressWord net addr : [trailingPathWord path | Just path <- [pathM]])

-- | The derivation path after an address in full, faint
trailingPathWord :: SoftPath -> Doc Style
trailingPathWord = styled Muted . pathToStr

-- | A line that starts with an address in full, and below it, starting
-- under the address, what is said of the address: its label, or the mark
-- of a change address ('ownAddressNoteBelow'), and what else a view adds,
-- two spaces apart. After an address in full, a label would push the line
-- far to the right. On the line below, in the colour of the address, it
-- is still seen to go with it. What is said wraps at the edge of the
-- screen, each line again under the address, so that long labels make no
-- line the terminal would wrap at its first column. A view that names an
-- address by its path keeps the label on the same line instead, since a
-- path is short ('payeeWords', 'purposeCell').
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

-- | The label of an address or of a transaction, quoted, which sets it
-- apart on a line. A label may come from a file, so every character that
-- is not text, a line break included, shows as an escape ('visibleText'):
-- a label cannot write lines of its own, such as a recipient the
-- transaction does not pay.
labelWord :: Text -> Doc Style
labelWord label = styled UserText ("\"" <> cs (visibleText label) <> "\"")

-- | The hash to name a transaction by. For one on the chain, its txid,
-- faint, since the user rarely types it. For one in preparation, its
-- nosigHash until it is signed, then its txid, as the commands take it.
hashWord :: TxStatus -> Doc Style
hashWord (OnChain status) = styled Muted (cs (txHashToHex (chainTxid status)))
hashWord (Pending status) = styled PendingHash (cs (txHashToHex (pendingShownHash status)))

{- Tables -}

-- accounts, txs, addresses, coins, balances and pendingtxs show each
-- element on one row with --tabular, in columns, under a header that names
-- them. The screen reads a listing in
-- chunks of rows (see "Haskoin.Wallet.Cli.Listing"), and only the first
-- chunk has the header. A column therefore cannot take the width of its
-- widest cell: a later chunk would not line up with the first one. Each
-- column has a width fixed in advance instead, wide enough for nearly
-- every cell. A cell wider than its column pushes the rest of its row.
--
-- A label has no width to fix in advance. It goes in the last column,
-- which ends the row and pushes nothing, and it is cut to
-- 'tableLabelLength' characters. The verbose view gives it whole. The
-- tables of transactions cut their last cell at 'purposeWidth' instead,
-- the address of another wallet included. An address in a column of
-- addresses is never cut.
--
-- A table has no frame, only lines between the columns and one under the
-- header. A frame needs a line under the last row, and the screen does not
-- know which chunk is the last before it shows it.

-- | Which side of its column a cell keeps to: words to the left, numbers
-- to the right, so that their points line up
data Side = LeftSide | RightSide

-- | A column of a table: its heading, its width, and the side its cells
-- keep to. Make one with 'column', which fits the heading in.
data Column = Column
  { columnHeading :: !String,
    columnWidth :: !Natural,
    columnSide :: !Side
  }

-- | A column with its heading, as wide as its cells need or as its
-- heading, whichever is wider. The last column of a table is given no
-- width of its own: its cells run past its heading.
column :: String -> Natural -> Side -> Column
column heading width = Column heading (max width (fromIntegral (shownWidth heading)))

-- | The heading of a column of amounts, which names their unit, so that
-- the cells hold numbers alone: "Amount (btc)", "Amount (sats)". The unit
-- is in the plural, as an amount of 2 shows it.
amountHeading :: String -> AmountUnit -> String
amountHeading name unit = name <> " (" <> cs (showUnit unit 2) <> ")"

-- | How many characters of a label the table of addresses shows at most,
-- the ellipsis that ends a longer one included. Quoted, a label then takes
-- 42 characters, as wide as a native SegWit address. The table of txs
-- cuts its whole last cell instead ('purposeWidth').
tableLabelLength :: Int
tableLabelLength = 40

-- | A label as a table shows it: whole, or its first characters and an
-- ellipsis when it is longer than 'tableLabelLength'
tableLabel :: Text -> Text
tableLabel = cutVisible tableLabelLength

-- | A text in the columns given at most: whole, or its first characters
-- and an ellipsis. The cut is made in the text as the terminal shows it,
-- escapes included ('visibleText'), and counts columns ('shownWidth'), so
-- that a wide character counts twice. The ellipsis takes one column, so
-- at least one is given.
cutVisible :: Int -> Text -> Text
cutVisible most label
  | shownWidth (cs shown) <= most = shown
  | otherwise = Text.pack (takeColumns (most - 1) (Text.unpack shown)) <> "…"
  where
    shown = visibleText label

-- | The first characters of a text that take the columns given at most. A
-- combining accent after the last of them stays with it.
takeColumns :: Int -> String -> String
takeColumns _ [] = []
takeColumns room (c : rest)
  | charWidth c > room = []
  | otherwise = c : takeColumns (room - charWidth c) rest

-- | A part of the text of a cell, with the styles it shows in. A cell
-- made of pieces can be measured, which a document cannot, to tell a
-- label the room it has.
data Piece = Piece ![Style] !Text

-- | The columns pieces take ('shownWidth')
piecesLength :: [Piece] -> Int
piecesLength pieces = sum [shownWidth (cs t) | Piece _ t <- pieces]

-- | Pieces as a document, each in its styles
piecesDoc :: [Piece] -> Doc Style
piecesDoc pieces = hcat [foldr annotate (pretty t) styles | Piece styles t <- pieces]

-- | A label, quoted, in the characters given at most, quotes included.
-- A longer label is cut inside its quotes, which then still close it.
quotedPiece :: [Style] -> Int -> Text -> Piece
quotedPiece styles most label = Piece styles ("\"" <> cutVisible (most - 2) label <> "\"")

-- | The header of a table: the heading of each column, then a line under
-- them
tableHeader :: [Column] -> [Doc Style]
tableHeader headed =
  [ tableLine [(headedColumn, styled Field (columnHeading headedColumn)) | headedColumn <- headed],
    tableRule headed
  ]

-- | A row of a table: each cell on its side of its column, padded to the
-- width of the column, and a faint line between two cells. The last cell
-- gets no padding when it keeps to the left, and an empty one no space
-- before it either: a line ends without spaces.
tableLine :: [(Column, Doc Style)] -> Doc Style
tableLine cells = hcat (zipWith cellAt [1 ..] cells)
  where
    cellAt :: Int -> (Column, Doc Style) -> Doc Style
    cellAt i (cellColumn, doc)
      | i == 1 = padded i cellColumn doc
      | i == length cells && null (renderPlain doc) = " " <> bar
      | otherwise = " " <> bar <> " " <> padded i cellColumn doc
    bar = styled Muted "│"
    padded i cellColumn doc =
      let missing = pretty (replicate (fromIntegral (columnWidth cellColumn) - shownWidth (renderPlain doc)) ' ')
       in case columnSide cellColumn of
            LeftSide
              | i == length cells -> doc
              | otherwise -> doc <> missing
            RightSide -> missing <> doc

-- | The line under the header of a table: a dash under each character of
-- the header, and a cross under each line between two columns. A column
-- has a space on each side of its cells, except on the left of the first
-- one and on the right of the last one.
tableRule :: [Column] -> Doc Style
tableRule headed = styled Muted (intercalate "┼" (zipWith dashes [1 ..] headed))
  where
    dashes :: Int -> Column -> String
    dashes i ruledColumn = replicate (fromIntegral (columnWidth ruledColumn) + spaceBefore i + spaceAfter i) '─'
    spaceBefore i = if i > 1 then 1 else 0
    spaceAfter i = if i < length headed then 1 else 0

{- The table of txs -}

-- The last column says what a transaction is for, as a bank statement
-- does. An address of the wallet is named by its path, which is short,
-- then the label of the transaction, or else that of the address. The
-- address of another wallet gives way to the label of the transaction,
-- since the two would not fit. A label is cut so that the cell takes
-- 'purposeWidth' columns at most. An address is never cut.
--
-- The table leaves out the txid. Its 64 characters would make a row too
-- wide for most screens, and its first characters alone would name it
-- nowhere, since no command takes part of a txid. The compact and the
-- verbose views give it.

-- | The columns of the table of txs
txColumns :: AmountUnit -> [Column]
txColumns unit =
  [ column "Time" minuteWidth LeftSide,
    column "Type" typeWidth LeftSide,
    column (amountHeading "Amount" unit) amountWidth RightSide,
    column (amountHeading "Balance" unit) amountWidth RightSide,
    -- Its cells take four characters at most, "999+" and "dead": the
    -- heading sets its width
    column "Confs" 4 RightSide,
    column "Description" 0 LeftSide
  ]

-- | The confirmations of a transaction, as a number alone in its column of
-- the table, up to "999+". An unconfirmed transaction has 0, in the
-- colour of what waits. It gives no fee rate, which would not fit. The
-- compact view gives it. A dead transaction says it is dead: it will
-- never confirm, and a count would hide that. txs lists no transaction in
-- preparation, but one would say how far it got, in the words of the
-- compact view, as short as they go.
tableConfirmations :: TxStatus -> Doc Style
tableConfirmations = \case
  OnChain status
    | chainDead status -> styled Failure "dead"
    | otherwise -> confsCell (chainConfirmations status)
  Pending status -> pendingStatusWord status

-- | A number of confirmations in a column of a table, up to "999+". None
-- is 0, in the colour of what waits.
confsCell :: Natural -> Doc Style
confsCell 0 = styled Waiting "0"
confsCell n = text (cappedCount n)

-- | How far a transaction in preparation got, in the words of the compact
-- view, as short as they go: not signed, signed, sent, or conflicted
pendingStatusWord :: PendingStatus -> Doc Style
pendingStatusWord status
  | pendingConflicted status = styled Failure "conflicted"
  | pendingOnline status = styled Good "sent"
  | pendingSigned status = styled Waiting "signed"
  | otherwise = styled Waiting "not signed"

-- | The time of a transaction in a column of a table, to the minute
timeCell :: Maybe ZonedTime -> Doc Style
timeCell = maybe (styled Muted "time unknown") (text . minuteText)

-- | What a transaction adds to the account or takes from it, as a number
-- in a column of a table, with its sign and its colour
signedAmountCell :: AmountUnit -> Integer -> Doc Style
signedAmountCell unit amount = maybe id annotate style (text (sign <> cs (showAmount unit amount)))
  where
    (style, sign) = changeSign amount

-- | The header of the table of txs
txTableHeader :: AmountUnit -> [Doc Style]
txTableHeader = tableHeader . txColumns

-- | A transaction on one row of the table of txs: its time, its type, its
-- amount, the balance after it and its confirmations, each in its column,
-- then what it is for ('purposeCell'). The amount has its sign and its
-- colour, as on the first line of the compact view. A dead transaction leaves its balance empty, since
-- the balance does not count it. The table lists one account, so no row
-- names its account.
txRowDoc :: Network -> AmountUnit -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
txRowDoc net unit balanceM time (TxInfo summary status label _) =
  tableLine (zip (txColumns unit) cells)
  where
    cells =
      [ timeCell time,
        text (typeWord (summaryType summary)),
        signedAmountCell unit (summaryAmount summary),
        maybe mempty (text . cs . showAmount unit) balanceM,
        tableConfirmations status,
        purposeCell net label summary
      ]

-- | The most characters of the last cell of the tables of transactions.
-- A label is cut to fit them, but an address is never cut.
purposeWidth :: Int
purposeWidth = 50

-- | What a transaction is for, as the last cell of the table of txs says
-- it. A payment received, or one between the addresses of the wallet,
-- gives the path of the address it pays the most, then the label of the
-- transaction, or else the label of that address, or else that it is a
-- change address: "at /0/3 \"Salary\"". A payment gives its label alone,
-- or else the address of the other wallet it pays the most: "to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU". Either tells how many other
-- addresses it pays. A label is cut inside its quotes, so that the cell
-- takes 'purposeWidth' columns at most. An address is never cut, however
-- long: one read in part is how a lookalike passes for the right one, and
-- the end of a Bitcoin Cash address, which people compare, would go
-- first. A long address makes its row wider instead.
purposeCell :: Network -> Text -> TxSummary -> Doc Style
purposeCell net label summary = piecesDoc pieces
  where
    pieces = case mainPayee summary of
      Just payee
        | isJust (txAddressOwn (payeeRow payee)) || Text.null label -> payeePieces payee
      _ -> [quotedPiece [UserText] purposeWidth label | not (Text.null label)]
    payeePieces (Payee preposition row others) =
      let before = [Piece [] (preposition <> " "), target row]
          after = [Piece [] (" and " <> cs (show others) <> " more") | others > 0]
          -- What the label may take: the cell, but for the rest and the
          -- space before the label
          room = purposeWidth - piecesLength (before <> after) - 1
       in before <> concat [[Piece [] " ", note] | Just note <- [noteOf room row]] <> after
    target row = case txAddressOwn row of
      Just (path, _) -> Piece [OwnAddress] (cs (pathToStr path))
      Nothing -> Piece [] (cs (addrToString net (txAddress row)))
    noteOf room row = case txAddressOwn row of
      Just (path, ownLabel)
        | not (Text.null label) -> Just (quotedPiece [UserText] room label)
        | not (Text.null ownLabel) -> Just (quotedPiece [OwnAddress, UserText] room ownLabel)
        | isIntPath path -> Just (Piece [Muted] "change address")
      _ -> Nothing

{- The table of addresses -}

-- The table of addresses gives what the compact view gives: the path, the
-- address, what it can spend, what it received and in how many
-- transactions, then its label. addresses lists the receiving addresses
-- or the change addresses, never both at once. A change address never
-- has a label, so the table of change addresses has no column for one.

-- | The columns of the table of the addresses of the account, on its
-- receiving chain or on its change chain. The path takes the width given,
-- that of the longest path of the account on the chain
-- ('longestPathWidth'), and the address that of the widest address the
-- account can have ('addressWidth').
addressTableColumns :: AmountUnit -> DBAccount -> Natural -> Bool -> [Column]
addressTableColumns unit acc pad internal =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth acc) LeftSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Received" unit) amountWidth RightSide,
    column "Txs" txCountWidth RightSide
  ]
    <> [column "Label" 0 LeftSide | not internal]

-- | The width of the column of the numbers of transactions: that of
-- 99 999 transactions. An address used that often is rare: hw gives out a
-- new address for each payment.
txCountWidth :: Natural
txCountWidth = 5

-- | The width of the widest address the account can have. Its addresses
-- are all of its kind, and bech32 and cashaddr write every address of a
-- kind with as many characters. Base58, which writes the P2PKH addresses
-- of Bitcoin, writes a hash shorter when its first bits are zero. A hash of
-- all ones takes the most characters, so its address is the widest.
addressWidth :: DBAccount -> Natural
addressWidth acc = fromIntegral (length (addrToString (accountNetwork acc) widest))
  where
    widest = case accountKind acc of
      Bip44 -> PubKeyAddress allOnes
      Bip84 -> WitnessPubKeyAddress allOnes
    allOnes = "ffffffffffffffffffffffffffffffffffffffff"

-- | An address on one row of its table, under the columns given: its
-- path, the address, what it can spend now and what it received, in green
-- unless zero, in how many transactions, then its label. An address no
-- transaction paid leaves its amounts empty, which sets it apart from one
-- that was paid and spent everything.
addressRowDoc :: AmountUnit -> [Column] -> DBAddress -> Word64 -> Doc Style
addressRowDoc unit headed addr spendable =
  tableLine (zip headed cells)
  where
    used = dBAddressBalanceTxs addr > 0
    cells =
      [ text (cs (pathText (addrPath addr))),
        pretty (dBAddressAddress addr),
        if used then heldAmountCell unit spendable else mempty,
        if used then heldAmountCell unit (dBAddressBalanceReceived addr) else mempty,
        text (show (dBAddressBalanceTxs addr))
      ]
        <> [labelCell | not (dBAddressInternal addr)]
    labelCell
      | Text.null (dBAddressLabel addr) = mempty
      | otherwise = labelWord (tableLabel (dBAddressLabel addr))

-- | An amount that waits to be spent, as a number in a column of a table:
-- yellow, unless it is zero, as 'waitingAmount'
waitingAmountCell :: AmountUnit -> Word64 -> Doc Style
waitingAmountCell unit amnt = (if amnt > 0 then annotate Waiting else id) (text (cs (showAmount unit (fromIntegral amnt))))

-- | An amount the wallet holds, as a number in a column of a table: green,
-- unless it is zero
heldAmountCell :: AmountUnit -> Word64 -> Doc Style
heldAmountCell unit amnt = (if amnt > 0 then annotate Incoming else id) (text (cs (showAmount unit (fromIntegral amnt))))

{- The table of accounts -}

-- The table of accounts gives what the compact view gives: the name, the
-- network and the derivation path, what the account can spend, what is
-- unconfirmed, and how many coins it holds. The fingerprint and the
-- addresses are for the verbose view.
--
-- The listing of accounts is read whole ('listingWhole'), so its first
-- column takes the width of the longest name, and no name is cut: the name
-- is what the table lists, and what --account takes.

-- | The columns of the table of accounts, the name, the network and the
-- path as wide as the widest of the accounts given
accountTableColumns :: AmountUnit -> [DBAccount] -> [Column]
accountTableColumns unit accs =
  [ column "Account" (widest (cs . visibleText . dBAccountName)) LeftSide,
    column "Network" (widest (\acc -> (accountNetwork acc).name)) LeftSide,
    column "Path" (widest (pathToStr . accountPath)) LeftSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Unconfirmed" unit) amountWidth RightSide,
    -- The heading sets the width: 99 999 coins
    column "Coins" 1 RightSide
  ]
  where
    -- As the terminal shows them, names with their escapes
    widest :: (DBAccount -> String) -> Natural
    widest cell = fromIntegral (maximum (0 : [shownWidth (cell acc) | acc <- accs]))

-- | An account on one row of its table, under the columns given: its
-- name, bold and whole, its network and its derivation path, then what it
-- can spend now, in green unless zero, what is unconfirmed, in yellow
-- unless zero, and how many coins it holds
accountRowDoc :: AmountUnit -> [Column] -> DBAccount -> Word64 -> Doc Style
accountRowDoc unit headed acc spendable =
  tableLine . zip headed $
    [ styled Account (cs (visibleText (dBAccountName acc))),
      text (accountNetwork acc).name,
      text (pathToStr (accountPath acc)),
      heldAmountCell unit spendable,
      waitingAmountCell unit (dBAccountBalanceUnconfirmed acc),
      text (show (dBAccountBalanceCoins acc))
    ]

-- | The name of an account in the table of pending transactions, bold, in
-- 'accountCellMost' columns at most. A longer name is cut, with an
-- ellipsis, so that a long name leaves room for the rest of the row.
accountCell :: DBAccount -> Doc Style
accountCell acc = styled Account (cs (cutVisible (fromIntegral accountCellMost) (dBAccountName acc)))

{- The table of coins -}

-- The table of coins gives what the compact view gives, but the coin
-- itself: the time of the transaction that paid the coin, its value, its
-- state, its confirmations, the address that holds it in full with its
-- path, and the label of the address. The coin, TXID:INDEX, takes 66
-- characters at least, which would make a row too wide. The compact view
-- gives it, as lockcoin and --coin take it.

-- | The columns of the table of coins. The address takes the width of
-- the widest address the account can have ('addressWidth'), and the path
-- the width given, that of the longest path of the account on its two
-- chains, since coins lists the coins of both.
coinTableColumns :: AmountUnit -> DBAccount -> Natural -> [Column]
coinTableColumns unit acc pad =
  [ column "Time" minuteWidth LeftSide,
    column (amountHeading "Value" unit) amountWidth RightSide,
    column "State" coinStateWidth LeftSide,
    column "Confs" 4 RightSide,
    column "Address" (addressWidth acc) LeftSide,
    column "Path" pad LeftSide,
    column "Label" 0 LeftSide
  ]

-- | The width of the column of the states of coins: that of "not
-- spendable yet", the longest
coinStateWidth :: Natural
coinStateWidth = 17

-- | A coin on one row of its table, under the columns given: the time of
-- the transaction that paid it, its value in green, its state, with
-- "locked by tx" for a coin a pending transaction spends, its
-- confirmations, the address in the colour of the wallet's addresses, its
-- path, faint, then the label of the address in that colour, or that it
-- is a change address. The address and its label share a colour, as in
-- the compact view.
coinRowDoc :: Network -> AmountUnit -> [Column] -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinRowDoc net unit headed time coin =
  tableLine $
    zip
      headed
      [ timeCell time,
        heldAmountCell unit (jsonCoinValue coin),
        coinState "locked by tx" coin,
        confsCell (jsonCoinConfirmations coin),
        ownAddressWord net (jsonCoinAddress coin),
        maybe mempty trailingPathWord (jsonCoinPath coin),
        -- The label has a column of its own
        hsep (ownAddressNote (tableLabel (jsonCoinAddressLabel coin)) (jsonCoinChange coin))
      ]

{- The table of balances -}

-- The table of balances gives what the compact view gives: the path, the
-- address, its three amounts, and its label, or that it is a change
-- address. balances lists the addresses of both chains together.

-- | The columns of the table of balances. The path takes the width given,
-- that of the longest path of the account on its two chains, and the
-- address that of the widest address the account can have.
balanceTableColumns :: AmountUnit -> DBAccount -> Natural -> [Column]
balanceTableColumns unit acc pad =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth acc) LeftSide,
    column (amountHeading "Total" unit) amountWidth RightSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Unspendable" unit) amountWidth RightSide,
    column "Label" 0 LeftSide
  ]

-- | An address on one row of the table of balances, under the columns
-- given: its path, the address, the value of its coins and what it can
-- spend, in green unless zero, the rest in yellow unless zero, then its
-- label, or that it is a change address
balanceRowDoc :: AmountUnit -> [Column] -> DBAddress -> CoinBalance -> Doc Style
balanceRowDoc unit headed addr balance =
  tableLine $
    zip
      headed
      [ text (cs (pathText (addrPath addr))),
        pretty (dBAddressAddress addr),
        heldAmountCell unit (coinBalanceTotal balance),
        heldAmountCell unit (coinBalanceSpendable balance),
        waitingAmountCell unit (coinBalanceUnspendable balance),
        labelCell
      ]
  where
    labelCell
      | not (Text.null (dBAddressLabel addr)) = labelWord (tableLabel (dBAddressLabel addr))
      | dBAddressInternal addr = changeWord
      | otherwise = mempty

{- The table of pending transactions -}

-- The table of pending transactions gives what the compact view gives,
-- but the hash: the time it was created, its type, its amount, how far it
-- got, its fee rate, and what it is for, as the table of txs says it. A
-- listing of every account names the account of each in a first column.
-- The listing is read whole ('listingWhole'), so that column is as wide as
-- the longest name it shows, up to 'accountCellMost'. The hash, which the
-- other commands take, would make a row too wide. The compact view gives
-- it.

-- | The columns of the table of pending transactions, with a column of
-- accounts first when the listing has every account's: those given, whose
-- longest name, cut as 'accountCell' cuts it, sets its width
pendingTableColumns :: AmountUnit -> Maybe [DBAccount] -> [Column]
pendingTableColumns unit namedM =
  [column "Account" (widest named) LeftSide | Just named <- [namedM]]
    <> [ column "Time" minuteWidth LeftSide,
         column "Type" typeWidth LeftSide,
         column (amountHeading "Amount" unit) amountWidth RightSide,
         column "Status" pendingStatusWidth LeftSide,
         -- The fee rate has a few digits: the heading sets the width
         column "sat/vB" 1 RightSide,
         column "Description" 0 LeftSide
       ]

  where
    widest accs = fromIntegral (maximum (0 : [min accountCellMost (shownWidth (cs (visibleText (dBAccountName acc)))) | acc <- accs]))

-- | The most columns the name of an account takes in the table of pending
-- transactions. A name is the user's choice, of any length, so a longer
-- one is cut, with an ellipsis ('accountCell').
accountCellMost :: Int
accountCellMost = 20

-- | The width of the column of the statuses of pending transactions: that
-- of "not signed" and "conflicted", the longest
pendingStatusWidth :: Natural
pendingStatusWidth = 10

-- | A pending transaction on one row of its table, under the columns
-- given: its account when given, the time it was created, its type, its
-- amount, how far it got, its fee rate, and what it is for
-- ('purposeCell'). One in conflict can no longer be sent: its fee rate no
-- longer matters, and its cell is empty.
pendingRowDoc :: Network -> AmountUnit -> [Column] -> Maybe DBAccount -> Maybe ZonedTime -> TxInfo -> Doc Style
pendingRowDoc net unit headed accountM time (TxInfo summary status label _) =
  tableLine . zip headed $
    [accountCell acc | Just acc <- [accountM]]
      <> [ timeCell time,
           text (typeWord (summaryType summary)),
           signedAmountCell unit (summaryAmount summary),
           statusCell,
           rateCell,
           purposeCell net label summary
         ]
  where
    -- pendingtxs lists the transactions in preparation only
    (statusCell, rateCell) = case status of
      Pending pending
        | pendingConflicted pending -> (pendingStatusWord pending, mempty)
        | otherwise -> (pendingStatusWord pending, text (show (summaryFeeByte summary)))
      OnChain _ -> (tableConfirmations status, mempty)

-- | A transaction with every detail: its first line, then its fields. The
-- lines given come first: its account in a listing of every account, or
-- its network in an answer. Its label, if any, follows them. The balance
-- after it, if the listing gives one, follows its fee. Every address
-- it spends from or pays is on a line of its own, in full, with its value,
-- and those of the wallet with their paths, in columns. Below an address
-- of the wallet, its label, and for one it spends from, the labels its
-- coins show, if any ('addressRow'). A change address is marked as one,
-- except under Change, which says it.
verboseTxDoc :: Network -> AmountUnit -> [Doc Style] -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
verboseTxDoc net unit named balanceM time info@(TxInfo summary status label inputLabels) =
  vcat $
    headlineDoc fullConfirmations unit time info
      : ( indent 2
            <$> named
              <> [field 9 "Label" <> labelWord label | not (Text.null label)]
              <> statusFields time status
              <> [feeField unit summary]
              <> [field 9 "Balance" <> neutralAmount unit balance | Just balance <- [balanceM]]
              <> lockTimeField summary
              <> section "From" Nothing True from
              <> section "To" (Just Incoming) True to
              <> section "Change" (Just Incoming) False change
        )
  where
    from = spentFrom inputLabels summary
    (to, change) = paidTo summary
    widths = addressColumns net unit (from <> to <> change)
    -- What the wallet receives is green. What it spends, and what others
    -- receive, is neither in nor out of the account.
    section name ownStyle marked rows =
      [field 9 name <> align (vcat (addressRow net unit widths ownStyle marked <$> rows)) | not (null rows)]

-- | The fields that say where a transaction stands: the hash to name it
-- by, its time to the second with its time zone, and what its first line
-- does not explain
statusFields :: Maybe ZonedTime -> TxStatus -> [Doc Style]
statusFields time (OnChain status) =
  [field 9 "Txid" <> text (cs $ txHashToHex (chainTxid status))]
    <> [field 9 "Time" <> text (secondText t) | Just t <- [time]]
    <> [field 9 "Status" <> "replaced, or dropped from the network" | chainDead status]
-- A transaction in preparation shows one hash, the one to name it by: its
-- nosigHash until it is signed, then its id, which the network will know.
statusFields time (Pending status) =
  [ field 9 (if pendingSigned status then "Txid" else "NoSigHash")
      <> styled PendingHash (cs $ txHashToHex (pendingShownHash status))
  ]
    <> [ field 9 "Replaces" <> align (vcat [text (cs $ txHashToHex replaced) | replaced <- pendingReplaces status])
       | not (null (pendingReplaces status))
       ]
    <> [field 9 "Created" <> text (secondText t) | Just t <- [time]]
    <> [ field 9 "Conflict"
           <> styled Failure "A coin it spends was spent by another transaction, or is gone."
           <+> "It can no longer be sent. Delete it with"
           <+> commandWord "deletetx"
           <> "."
       | pendingConflicted status
       ]

-- | The fee and its rate. Below them, the change the fee took in, too
-- small for an output: that change comes neither in nor out.
feeField :: AmountUnit -> TxSummary -> Doc Style
feeField unit summary =
  field 9 "Fee"
    <> align
      ( vcat $
          [neutralAmount unit (fromIntegral (summaryFee summary)) <> "," <+> text (show (summaryFeeByte summary) <> " sat/vB")]
            <> [ "includes" <+> neutralAmount unit (fromIntegral change) <+> "of change too small for an output"
               | Just change <- [summaryChangeInFee summary],
                 change > 0
               ]
      )

-- | The lock time of a transaction that has one: the block it waits for,
-- which hw sets near the best block (see "Haskoin.Wallet.Tx.LockTime"). A
-- lock time of 500,000,000 or more is a Unix time instead, which hw never
-- writes.
lockTimeField :: TxSummary -> [Doc Style]
lockTimeField summary =
  [field 9 "Lock time" <> text (kind <> show lockTime) | lockTime /= 0]
  where
    lockTime = summaryLockTime summary
    kind = if lockTime < 500000000 then "block " else "Unix time "

-- | An address a transaction spends from or pays, as hw shows it
data TxAddress = TxAddress
  { txAddress :: !Address,
    -- | What it spends or receives. None for an address another wallet
    -- spends from: that wallet's coins are nothing to this one.
    txAddressValue :: !(Maybe Natural),
    -- | The derivation path and the label of an address of the wallet
    txAddressOwn :: !(Maybe (SoftPath, Text)),
    -- | The labels the coins spent from an address of the wallet show,
    -- each once. None for an address paid.
    txAddressCoinLabels :: ![Text]
  }

-- | The addresses a transaction spends from: those of the wallet with
-- what they spend and the labels their coins show, then those of other
-- wallets
spentFrom :: Map.Map TxHash Text -> TxSummary -> [TxAddress]
spentFrom inputLabels summary =
  [TxAddress a (Just v) (Just (p, l)) (coinLabelsOf outpoints) | (a, MyInputs v p l outpoints) <- Map.assocs (summaryMyInputs summary)]
    <> [TxAddress a Nothing Nothing [] | a <- Map.keys (summaryOtherInputs summary)]
  where
    coinLabelsOf outpoints = nub [label | outpoint <- outpoints, Just label <- [Map.lookup outpoint.hash inputLabels]]

-- | The addresses a transaction pays, and the change it sends back to the
-- wallet. What a payment sends back to the wallet is its change. In a
-- payment the wallet receives, the outputs to others are the change of the
-- payer, left out. Between the addresses of the wallet, no output can be
-- told to be the change.
paidTo :: TxSummary -> ([TxAddress], [TxAddress])
paidTo summary =
  case summaryType summary of
    TxDebit -> (toOthers, toWallet)
    TxCredit -> (toWallet, [])
    TxInternal -> (toWallet <> toOthers, [])
  where
    toWallet = [TxAddress a (Just v) (Just (p, l)) [] | (a, MyOutputs v p l) <- Map.assocs (summaryMyOutputs summary)]
    toOthers = [TxAddress a (Just v) Nothing [] | (a, v) <- Map.assocs (summaryOtherOutputs summary)]

-- | The widths of the columns of the addresses of a transaction, each that
-- of its widest cell
data AddressColumns = AddressColumns
  { addressColumn :: !Int,
    valueColumn :: !Int,
    pathColumn :: !Int
  }

addressColumns :: Network -> AmountUnit -> [TxAddress] -> AddressColumns
addressColumns net unit rows =
  AddressColumns
    { addressColumn = widest [addrToString net (txAddress row) | row <- rows],
      valueColumn = widest [valueText unit v | Just v <- txAddressValue <$> rows],
      pathColumn = widest [pathToStr p | Just (p, _) <- txAddressOwn <$> rows]
    }
  where
    widest = maximum . (0 :) . map shownWidth

-- | A value of an address, as its column shows it: "0.00050000 btc"
valueText :: AmountUnit -> Natural -> String
valueText unit v = cs (showAmount unit (fromIntegral v) <> " " <> showUnit unit (fromIntegral v))

-- | An address of a transaction: the address, its value right-aligned,
-- then for an address of the wallet, its path. Below them, on a line that
-- starts under the address, an address of the wallet has its label, or
-- that it is a change address when marked, then the labels its coins
-- show, after the word "from". The line below keeps the columns of
-- addresses lined up, however long the labels. An address of the wallet
-- and its label are in its colour, and its value in the style given.
addressRow :: Network -> AmountUnit -> AddressColumns -> Maybe Style -> Bool -> TxAddress -> Doc Style
addressRow net unit widths ownStyle marked row =
  withNotesBelow (cellsLine firstLine) notes
  where
    firstLine =
      [(addressColumn widths, address, addressDoc)]
        <> [(valueColumn widths, padding value <> value, valueDoc value) | Just v <- [txAddressValue row], let value = valueText unit v]
        <> [(pathColumn widths, pathToStr path, trailingPathWord path) | Just (path, _) <- [txAddressOwn row]]
    notes = case txAddressOwn row of
      Just (path, label) ->
        ownAddressNoteBelow label (marked && isIntPath path)
          <> [styled Muted "from" <+> fillSep (punctuate "," (wrappingLabelWord <$> spentLabels)) | let spentLabels = txAddressCoinLabels row, not (null spentLabels)]
      Nothing -> []
    address = addrToString net (txAddress row)
    addressDoc = if isJust (txAddressOwn row) then styled OwnAddress address else text address
    style = if isJust (txAddressOwn row) then ownStyle else Nothing
    -- Right-aligned, so that the points line up. The text of the cell
    -- holds the spaces before the value, so that 'cellsLine' adds none
    -- after it, and the paths line up too.
    padding value = replicate (valueColumn widths - length value) ' '
    valueDoc value = pretty (padding value) <> maybe id annotate style (pretty value)

-- | Cells side by side, two spaces apart, each with the width of its
-- column and its text. Each is padded to the width of its column, but the
-- last: a line ends without spaces. A right-aligned cell comes padded on
-- its left, and its text holds that padding, so that it gets none on its
-- right.
cellsLine :: [(Int, String, Doc Style)] -> Doc Style
cellsLine cells = hcat (intersperse "  " (padded cells))
  where
    padded [] = []
    padded [(_, _, doc)] = [doc]
    padded ((width, content, doc) : rest) = (doc <> pretty (replicate (width - shownWidth content) ' ')) : padded rest

-- | The coins a transaction just prepared leaves in the account, each with
-- why, a blank line between them
leftCoinsDoc :: AmountUnit -> [LeftCoin] -> Doc Style
leftCoinsDoc unit left =
  vcat . intersperse mempty $
    text (counted (fromIntegral (length left)) "coin stays" "coins stay" <> " in the account:")
      : [questionCoinDoc unit coin.leftCoinOutpoint (fromIntegral coin.leftCoinValue) coin.leftCoinTxLabel (text (leftReasonText coin.leftCoinReason)) | coin <- left]

{- The questions of preparesendall and bumpfee -}

-- The reports the commands show on stderr before they ask. A sentence
-- wraps at the width of the screen, as the help does. A coin takes two
-- lines, as in a listing: its value with what is said of it, then the coin
-- itself, as --coin takes it. The amounts are in the unit asked for.

-- | A sentence of words and amounts, wrapped at the width of the screen
sentence :: [Doc Style] -> Doc Style
sentence = fillSep

-- | The words of a text, for a sentence
wordsOf :: String -> [Doc Style]
wordsOf = map pretty . words

-- | A coin in a question: its value, then what is said of it, then the
-- label it shows if any, then the coin as --coin and lockcoin take it
questionCoinDoc :: AmountUnit -> OutPoint -> Natural -> Text -> Doc Style -> Doc Style
questionCoinDoc unit outpoint value label said =
  vcat $
    [columns [amountCell (Just Incoming) "" unit (fromIntegral value), said]]
      <> [indent 2 (labelWord label) | not (Text.null label)]
      <> [indent 2 (styled Muted (coinText outpoint))]

-- | A fee rate, in satoshis per virtual byte
rateDoc :: Natural -> Doc Style
rateDoc feeByte = text (show feeByte <> " sat/vB")

-- | The report on the coins worth less than their fee, as preparesendall
-- shows it before its question: the coins, each with the rates it is worth
-- spending at and the label it shows among those given, then what
-- spending them and leaving them does
dustReportDoc :: AmountUnit -> Map.Map TxHash Text -> DustReport -> Doc Style
dustReportDoc unit labels report =
  vcat . intersperse mempty $ [header] <> (dustCoinDoc <$> report.dustCoins) <> [vcat [spent, left]]
  where
    count = length report.dustCoins
    one = count == 1
    them = if one then "it" else "them"
    amount v = neutralAmount unit (fromIntegral v)
    header
      | one = sentence $ wordsOf "1 coin costs more to spend than it is worth at" <> [rateDoc report.rate <> ",", amount (inputFee report.reportKind report.rate) <> ":"]
      | otherwise = sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ",", amount (inputFee report.reportKind report.rate)] <> wordsOf "each:"
    dustCoinDoc coin = questionCoinDoc unit coin.outpoint (fromIntegral coin.value) (Map.findWithDefault "" coin.outpoint.hash labels) (worth coin)
    worth coin = case worthSpendingUpTo report.reportKind coin of
      0 -> "worth less than its fee at any rate"
      upTo -> "worth spending at" <+> rateDoc upTo <+> "or less"
    dustValue = valueOf report.dustCoins
    spent =
      sentence $
        wordsOf ("Spending " <> them <> " adds")
          <> [amount report.addedFee]
          <> wordsOf "of fee for"
          <> [amount dustValue]
          <> wordsOf "of coins. What remains for the addresses without an amount is"
          <> [amount report.withDust]
          <> wordsOf "instead of"
          <> [amount report.withoutDust <> ",", amount (report.withoutDust - report.withDust)]
          <> wordsOf ("less" <> if report.othersStay then "." else ", and the account is empty.")
    left =
      sentence $
        wordsOf ("Leaving " <> them <> ", the account keeps")
          <> [amount dustValue <> "."]
          <> wordsOf "Once the other coins are gone,"
          <> later
    later = case report.laterRate of
      Just upTo
        | one -> wordsOf "it can only be spent at" <> [rateDoc upTo] <> wordsOf "or less."
        | otherwise -> wordsOf "they can only be spent together, at" <> [rateDoc upTo] <> wordsOf "or less."
      Nothing -> wordsOf ((if one then "it" else "they") <> " can never be spent: any fee would leave less than") <> [amount report.dustLimit <> "."]

-- | What bumpfee says of the transactions that spend the change of the one
-- to replace, before it asks to replace them all
childrenDoc :: [Sent] -> Doc Style
childrenDoc children =
  vcat $
    sentence (wordsOf "Replacing the transaction removes those of the account that spend its change:")
      : [indent 2 (styled Muted (cs (txHashToHex (sentId child)))) | child <- children]

-- | What bumpfee says of the coins to add, before it asks to add them: each
-- with the address that holds it and its path, and the label it shows
-- among those given
addedCoinsDoc :: Network -> AmountUnit -> Map.Map TxHash Text -> Natural -> [(Store.Unspent, Address, SoftPath)] -> Doc Style
addedCoinsDoc net unit labels feeByte added =
  vcat . intersperse mempty $
    header : [questionCoinDoc unit coin.outpoint (fromIntegral coin.value) (Map.findWithDefault "" coin.outpoint.hash labels) (held addr path) | (coin, addr, path) <- added]
  where
    them = if length added == 1 then "this coin" else "these coins"
    header = sentence $ wordsOf "The coins of the transaction cannot pay its fee at" <> [rateDoc feeByte <> ","] <> wordsOf ("and " <> them <> " of the account would:")
    held addr path = "at" <+> fullAddressWords net addr (Just path)

-- | What bumpfee says of the ways the recipients can pay, those of
-- 'reductions', before it asks which: each recipient, numbered when there
-- are several, with what it receives, then what it would receive paying
-- alone, and its share when all can pay, a blank line between them
reducedDoc :: Network -> AmountUnit -> Natural -> [Reduction] -> Doc Style
reducedDoc _ _ _ [] = mempty
reducedDoc net unit feeByte ways@(way : _) =
  vcat . intersperse mempty $ header : rows
  where
    amount v = neutralAmount unit (fromIntegral v)
    missing = [amount way.reductionMissing, "missing"]
    recipients = [(addr, before) | (addr, before, _) <- way.reductionRecipients]
    several = length recipients > 1
    -- What each recipient gets paying it alone, by its position, when it can
    alone = Map.fromList [(i, after) | w <- ways, OneRecipient i <- [w.reductionPaidBy], (_, _, after) <- [w.reductionRecipients !! i]]
    -- What each gets paying its share, when all can
    shares = listToMaybe [[after | (_, _, after) <- w.reductionRecipients] | w <- ways, w.reductionPaidBy == AllRecipients]
    header =
      sentence $
        wordsOf "The coins of the transaction cannot pay its fee at"
          <> [rateDoc feeByte <> ","]
          <> wordsOf "and no coin of the account is added."
          <> who
    who
      | several =
          wordsOf "The" <> missing <> wordsOf ("can come out of what the recipients receive, one of them paying it alone" <> maybe ":" (const ", or all of them in equal shares:") shares)
      | otherwise = wordsOf "The recipient can pay the" <> missing <> wordsOf "out of what it receives:"
    -- The numbers of the recipients, padded to the widest, and what each
    -- would receive under its address
    width = length (show (length recipients))
    rows
      | several =
          [ vcat
              [ indent 2 (columns [text (rblock (fromIntegral width) (show (i + 1))), text (addrToString net addr), amount before]),
                indent (4 + width) (commaList (aloneDoc i : shareDoc i))
              ]
            | (i, (addr, before)) <- zip [0 :: Int ..] recipients
          ]
      | otherwise =
          [ vcat [indent 2 (columns [text (addrToString net addr), amount before]), indent 4 (amount after <+> "paying it")]
            | (addr, before, after) <- way.reductionRecipients
          ]
    aloneDoc i = maybe "too little to pay it alone" (\after -> amount after <+> "paying it alone") (Map.lookup i alone)
    shareDoc i = [amount (afters !! i) <+> "paying its share" | Just afters <- [shares]]

-- | Print the answer of a command. A listing shows each element in the
-- view given, and an address that goes with a code shows it on a terminal.
prettyPrinter :: AmountUnit -> View -> AddressCode -> TxShown -> Response -> IO ()
prettyPrinter unit view code shown res = do
  responsePrinter unit view code shown res
  when (responseFailed res) exitFailure

responsePrinter :: AmountUnit -> View -> AddressCode -> TxShown -> Response -> IO ()
responsePrinter unit view code shown answer =
  case answer of
    -- A transaction just signed, shown in full before the secret was asked
    ResponseTx _ info | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    ResponseOutsideTx _ info | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    -- The listings, which the screen reads in chunks ('rowsPrinter')
    ResponseAccounts {} -> listing
    ResponseAddresses {} -> listing
    ResponseBalances {} -> listing
    ResponseTxs {} -> listing
    ResponsePendingTxs {} -> listing
    ResponseCoins {} -> listing
    ResponseError err -> renderIO $ errorDoc err
    ResponseMnemonic source ws -> renderIO $ mnemonicDoc source ws
    ResponseShares source threshold shares -> renderIO $ sharesDoc source threshold shares
    ResponseAccount acc spendable -> renderIO =<< listedAccountDoc Verbose unit acc spendable
    -- In green when the command did what it was asked, in red otherwise
    ResponseAccResult _ res txt -> renderIO $ styled (if res then Good else Failure) (cs txt)
    ResponseFile f -> renderIO $ "Written to" <+> styled UserText f
    ResponseImport report -> renderIO $ importDoc report
    ResponseDescriptors held -> renderIO $ vcat (intersperse mempty [descriptorDoc acc descriptor slip132 | (acc, descriptor, slip132) <- held])
    ResponseAddress acc addr spendable ->
      renderIO =<< addressAnswerDoc code unit acc addr spendable
    ResponseCheckAddr acc path addr label -> renderIO $ checkAddrDoc acc path addr label
    ResponseTx acc txInfo ->
      renderIO =<< txAnswerDoc (accountNetwork acc) (Just acc) unit txInfo
    -- Each account's view of the transaction, a blank line between them
    ResponseLabelledTxs held ->
      renderIO . vcat . intersperse mempty =<< mapM (\(acc, txInfo) -> txAnswerDoc (accountNetwork acc) (Just acc) unit txInfo) held
    -- The coins left in the account below the transaction, apart from it
    ResponsePrepared acc txInfo left -> do
      renderIO =<< txAnswerDoc (accountNetwork acc) (Just acc) unit txInfo
      unless (null left) $ putStrLn "" >> renderIO (leftCoinsDoc unit left)
    ResponseOutsideTx net txInfo -> renderIO =<< txAnswerDoc net Nothing unit txInfo
    ResponseSentTx net h ->
      renderIO $
        vcat
          [ field 9 "Sent" <> text (cs $ txHashToHex h),
            indent 2 $ field 9 "Network" <> text net.name
          ]
    -- By the hash pendingtxs showed: the id of a signed transaction. The
    -- coins it spent are free to spend again, and the change addresses it
    -- paid are free for the next transaction.
    ResponseDeleteTx nosigH txidM coins addrs ->
      renderIO $
        vcat
          [ field 9 "Deleted" <> styled PendingHash (cs $ txHashToHex (fromMaybe nosigH txidM)),
            indent 2 $ field 9 "Freed" <> freed
          ]
      where
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
    -- The accounts not synced last, where the user sees them
    ResponseSync xs notSynced ->
      renderIO . vcat . intersperse mempty $
        [syncResultsPrinter unit xs | not (null xs)] <> [notSyncedPrinter notSynced | not (null notSynced)]
    ResponseRecover net gap xs elsewhere ->
      renderIO . vcat . intersperse mempty $
        recovered : [foreignFundsDoc unit net elsewhere | not (null elsewhere)]
      where
        recovered
          | null xs = text $ "No account of the seed was paid on " <> net.name <> ": the search ended after " <> show gap <> " accounts in a row without a payment"
          | otherwise = syncResultsPrinter unit xs
    ResponseFees net rates -> do
      now <- getCurrentTime
      renderIO $ feeRatesDoc net now rates
  where
    listing = mapM_ (renderIO . listingDoc) =<< rowsPrinter unit view answer

-- | A transaction just signed, as signtx and signsweep answer on the
-- screen: its id, and the file it was written to, if any. The transaction
-- itself was shown before the secret was asked for.
signedDoc :: Maybe FilePath -> TxInfo -> Doc Style
signedDoc fileM info =
  vcat $
    [styled Good "Signed" <> "," <+> "txid" <+> styled PendingHash (cs (txHashToHex hash)) | Just hash <- [txInfoHash info]]
      <> ["Written to" <+> styled UserText file | Just file <- [fileM]]

-- | The fee rates recent blocks paid on the network, with how fresh they
-- are: the age of the latest of those blocks, which also tells when the
-- network is slow. A latest block mined an hour ago or more waits: its
-- rates may be too low, as the help of fees says.
feeRatesDoc :: Network -> UTCTime -> FeeRates -> Doc Style
feeRatesDoc net now rates =
  vcat
    [ text ("Fee rates of " <> net.name <> ", from " <> blocks),
      indent 2 $ commaList [text (cs (levelName level)) <+> text (show (rateAt level rates)) | level <- [minBound .. maxBound]] <+> "sat/vB",
      indent 2 $ "the latest block was mined" <+> age
    ]
  where
    latest = posixSecondsToUTCTime (fromIntegral rates.ratesLatestTime)
    age
      | diffUTCTime now latest >= 3600 = styled Waiting (agoText now latest)
      | otherwise = text (agoText now latest)
    blocks
      | rates.ratesFromHeight == rates.ratesToHeight = "block " <> show rates.ratesToHeight
      | otherwise = "blocks " <> show rates.ratesFromHeight <> " to " <> show rates.ratesToHeight

-- | How long before now a time was, in round words: "3 minutes ago",
-- "2 hours ago", "5 days ago". A time to come reads as just past.
agoText :: UTCTime -> UTCTime -> String
agoText now time
  | seconds < 60 = "less than a minute ago"
  | minutes < 120 = count minutes "minute"
  | hours < 48 = count hours "hour"
  | otherwise = count days "day"
  where
    seconds = max 0 (floor (diffUTCTime now time)) :: Integer
    minutes = seconds `div` 60
    hours = minutes `div` 60
    days = hours `div` 24
    count n unit = show n <> " " <> unit <> (if n == 1 then "" else "s") <> " ago"

{- Next steps -}

-- | What the user does next after a command of a payment or a sweep,
-- preparetx, preparesendall, preparesweep, signtx, signsweep, sendtx or
-- sync, below its answer. In the pretty output only: a script reading the JSON answer knows
-- its next step.
printNextSteps :: Command -> Response -> IO ()
printNextSteps cmd res =
  forM_ (nextStepsPrinter cmd res) $ \steps -> putStrLn "" >> renderIO steps

-- | A failure, as an answer or a listing reports it
errorDoc :: Text -> Doc Style
errorDoc err = styled Failure "Error:" <+> pretty err

{- Listings -}

-- | The rows of a listing, as the screen shows them
data Rows = Rows
  { -- | What to say when the listing has no row at all
    rowsNone :: Doc Style,
    -- | The lines above the first row: the header of a table. Only the
    -- first chunk of a listing shows them.
    rowsHeader :: [Doc Style],
    -- | A document per row
    rowsDocs :: [Doc Style],
    -- | Whether a blank line sets the rows apart
    rowsApart :: Bool
  }

-- | The rows of a listing whose elements take a few lines each: no
-- header, and a blank line between two elements
apartRows :: Doc Style -> [Doc Style] -> Rows
apartRows none docs = Rows none [] docs True

-- | The rows of an answer that lists, or Nothing for an answer that does
-- not, a blank line between them. A table, of txs or of addresses, has a
-- header instead, and a row under the other. The screen reads a listing in chunks, so the
-- paths of its addresses are padded to the longest path of the account on
-- the chains listed, the same for every chunk, rather than to the longest
-- one shown. The columns of the table have fixed widths for the same
-- reason. The accounts, addresses, transactions and coins show in the view
-- given, their times in the time zone of the computer.
rowsPrinter :: AmountUnit -> View -> Response -> IO (Maybe Rows)
rowsPrinter unit view = \case
  ResponseAccounts accs -> do
    let none = text "There are no accounts to display"
        -- The listing is read whole: these are all the accounts
        headed = accountTableColumns unit (fst <$> accs)
    if view == Table
      then return . Just $ Rows none (tableHeader headed) (uncurry (accountRowDoc unit headed) <$> accs) False
      else Just . apartRows none <$> mapM (uncurry (listedAccountDoc view unit)) accs
  ResponseAddresses acc addrs -> do
    let none = text "There are no addresses to display"
        pad = longestPathWidth acc (nub (dBAddressInternal . fst <$> addrs))
        -- addresses lists the addresses of one chain
        internal = any (dBAddressInternal . fst) addrs
        headed = addressTableColumns unit acc pad internal
    if view == Table
      then return . Just $ Rows none (tableHeader headed) (uncurry (addressRowDoc unit headed) <$> addrs) False
      else Just . apartRows none <$> mapM (uncurry (listedAddressDoc view unit pad)) addrs
  ResponseBalances acc addrs ->
    let none = text "There are no addresses with coins to display"
        pad = longestPathWidth acc [False, True]
        headed = balanceTableColumns unit acc pad
     in return . Just $
          if view == Table
            then Rows none (tableHeader headed) (uncurry (balanceRowDoc unit headed) <$> addrs) False
            else apartRows none (uncurry (listedBalanceDoc view unit pad) <$> addrs)
  ResponseTxs acc txs -> do
    docs <- mapM (\(ListedTx info balanceM) -> listedTxDoc view (accountNetwork acc) unit Nothing balanceM info) txs
    let none = text "There are no transactions to display"
    return . Just $
      if view == Table
        then Rows none (txTableHeader unit) docs False
        else apartRows none docs
  -- Listing every account, each transaction names its own
  ResponsePendingTxs asked held -> do
    let none = case asked of
          Just acc -> "There are no pending transactions in the account " <> cs (dBAccountName acc)
          Nothing -> "There are no pending transactions"
        named acc = if isNothing asked then Just acc else Nothing
        -- The listing is read whole: these are all its accounts
        headed = pendingTableColumns unit (if isNothing asked then Just (fst <$> held) else Nothing)
    if view == Table
      then do
        rows <- mapM (\(acc, info) -> pendingRowDoc (accountNetwork acc) unit headed (named acc) <$> localTxTime info <*> pure info) held
        return . Just $ Rows (text none) (tableHeader headed) rows False
      else do
        docs <- mapM (\(acc, info) -> listedTxDoc view (accountNetwork acc) unit (named acc) Nothing info) held
        return . Just $ apartRows (text none) docs
  ResponseCoins acc coins -> do
    let none = text "There are no coins to display"
        headed = coinTableColumns unit acc (longestPathWidth acc [False, True])
    if view == Table
      then do
        rows <- mapM (\coin -> coinRowDoc (accountNetwork acc) unit headed <$> traverse utcToLocalZonedTime (jsonCoinTime coin) <*> pure coin) coins
        return . Just $ Rows none (tableHeader headed) rows False
      else Just . apartRows none <$> mapM (listedCoinDoc view (accountNetwork acc) unit) coins
  _ -> return Nothing

-- | The width of the longest derivation path of the account on the
-- chains, False for the receiving one and True for the change one: that
-- of its newest address there
longestPathWidth :: DBAccount -> [Bool] -> Natural
longestPathWidth acc chains = fromIntegral $ maximum (0 : (width <$> chains))
  where
    width internal =
      let count = if internal then dBAccountInternal acc else dBAccountExternal acc
       in if count == 0 then 0 else Text.length (pathText (chainPath internal (count - 1)))

-- | A whole listing: its header and its rows, or what to say when it has
-- no row
listingDoc :: Rows -> Doc Style
listingDoc rows
  | null (rowsDocs rows) = rowsNone rows
  | otherwise = vcat (rowsHeader rows <> [rowsBody rows])

-- | The rows of a listing alone, without its header
rowsBody :: Rows -> Doc Style
rowsBody rows = vcat (if rowsApart rows then intersperse mempty (rowsDocs rows) else rowsDocs rows)

-- | A chunk of a listing, the first one or a later one. The first chunk
-- of a listing without rows says so. The first chunk has the header of
-- the listing, if any, and a later one goes on under the rows before it.
-- A later chunk of a listing whose rows are set apart starts with the
-- blank line that sets it apart from the chunk before. An answer that is
-- not a listing is a failure here.
chunkDoc :: AmountUnit -> View -> Bool -> Response -> IO (Doc Style)
chunkDoc unit view first answer = do
  rowsM <- rowsPrinter unit view answer
  return $ case rowsM of
    Just rows
      | first -> listingDoc rows
      | otherwise -> vcat ([mempty | rowsApart rows] <> [rowsBody rows])
    Nothing -> case answer of
      ResponseError err -> errorDoc err
      _ -> errorDoc "The answer is not a listing"

-- | The steps the user may take next, after a command of a payment or a
-- sweep, if any. Each step is a command line, styled as the help shows
-- commands, options and metavars.
nextStepsPrinter :: Command -> Response -> Maybe (Doc Style)
nextStepsPrinter cmd res =
  case (cmd, res) of
    (CommandPrepareTx opts, ResponsePrepared _ info _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    (CommandPrepareSendAll opts, ResponsePrepared _ info _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    (CommandBumpFee opts, ResponsePrepared _ info _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    -- The keys of a file at hand for the sweep are the ones to sign it with
    (CommandPrepareSweep opts, ResponseTx _ info)
      | Just shown <- shownOf info ->
          stepsPrinter
            [ ("Next, sign it with the private keys of the coins", hwLine "signsweep" (pendingHashWord shown : keysArgs opts.keys)),
              case opts.output of
                Nothing -> ("Or export it, to sign it where the keys are", hwLine "exporttx" [pendingHashWord shown, metavarWord "FILE"])
                Just file -> ("Or sign its file where the keys are", hwLine "signsweep" [optionWord "--input", fileWord file])
            ]
    -- A sweep outside the wallet is in its file only
    (CommandPrepareSweep opts, ResponseOutsideTx _ _)
      | Just file <- opts.output ->
          stepsPrinter [("Next, sign its file with the private keys of the coins", hwLine "signsweep" ([optionWord "--input", fileWord file] <> keysArgs opts.keys))]
    (CommandSignTx signing _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseOutsideTx _ info) -> afterSigning signing info
    (CommandSendTx {}, ResponseTx acc _) ->
      stepsPrinter [("Next, sync the account to follow the transaction", hwLine "sync" (accountArgs [acc]))]
    -- An account a descriptor added has the name recover gives one, since
    -- a descriptor carries none, and the file may label things the wallet
    -- does not have yet
    (CommandImportAcc _ _ file, ResponseImport report)
      | not (null steps) -> stepsPrinter steps
      where
        steps = renameSteps <> syncSteps
        renameSteps =
          [ ("Next, name the account, which the file did not", hwLine "renameacc" [pretty (shellWord (cs (dBAccountName acc))) `withStyle` Account, metavarWord "NAME"])
            | AccountCreated acc _ <- reportAccounts report,
              dBAccountName acc == recoveredName (accountKind acc) (accountNetwork acc) (accountIndex acc)
          ]
        syncSteps
          | reportNeedsSync report =
              [ ("Next, sync the wallet, for the addresses, transactions and coins the file labels", hwLine "sync" []),
                ("Then import the file again, for the labels it could not place", hwLine "importacc" [fileWord file])
              ]
          | otherwise = []
    -- A sync that synced no account has nothing new to see
    (CommandSync {}, ResponseSync results _)
      | not (null results) ->
          case filter ((> 0) . syncResConflicted) results of
            [] -> stepsPrinter [("Next, see the transactions", hwLine "txs" (accountArgs (syncResAccount <$> results)))]
            conflicted ->
              stepsPrinter
                [ ("Next, find the pending transaction in conflict, which can no longer be sent", hwLine "pendingtxs" (accountArgs (syncResAccount <$> conflicted))),
                  ("Then delete it, to free its coins", hwLine "deletetx" [metavarWord "HASH"])
                ]
    _ -> Nothing
  where
    -- The hash of a transaction in preparation, as its answer shows it
    shownOf info = case txInfoStatus info of
      Pending status -> Just (cs (txHashToHex (pendingShownHash status)))
      OnChain _ -> Nothing
    -- A payment of the wallet is signed with its mnemonic, here or on an
    -- offline computer
    afterPreparing output shown =
      stepsPrinter
        [ ("Next, sign it", hwLine "signtx" [pendingHashWord shown]),
          case output of
            Nothing -> ("Or export it, to sign it on an offline computer", hwLine "exporttx" [pendingHashWord shown, metavarWord "FILE"])
            Just file -> ("Or sign its file on an offline computer", hwLine "signtx" [optionWord "--input", fileWord file])
        ]
    -- A transaction signed in the wallet is sent by its id, one
    -- signed from a file by the file signed: the --output file, or the
    -- --input file it replaced
    afterSigning signing info
      | Just shown <- shownOf info, isJust signing.nosigHash =
          stepsPrinter [("Next, send it to the network", hwLine "sendtx" [pendingHashWord shown])]
      | Just file <- signing.output <|> signing.input =
          stepsPrinter [("Next, send its file from the online computer", hwLine "sendtx" [optionWord "--input", fileWord file])]
      | otherwise = Nothing
    -- Without a file, signsweep asks for the keys on the terminal
    keysArgs (Just (KeysInFile file)) = [optionWord "--key-file", fileWord file]
    keysArgs _ = []
    -- The account of the next command, named when there is one
    accountArgs [acc] = [optionWord "--account", styled Account (shellWord (cs (dBAccountName acc)))]
    accountArgs _ = [optionWord "--account", metavarWord "NAME"]

-- | Steps, each with what it does and the command that does it
stepsPrinter :: [(String, Doc Style)] -> Maybe (Doc Style)
stepsPrinter steps = Just $ vcat [vcat [styled Field (what <> ":"), indent 2 command] | (what, command) <- steps]

-- | A command line of hw, "hw signtx NOSIGHASH", the command in the colour
-- of its kind as the help shows it
hwLine :: String -> [Doc Style] -> Doc Style
hwLine name args = hsep ("hw" : commandWord name : args)

-- | A command of hw, in the colour of its kind, as the list of commands
-- shows it
commandWord :: String -> Doc Style
commandWord name = maybe (styled Command name) commandName (commandNamed program name)

optionWord :: String -> Doc Style
optionWord = styled Option

-- | What the user has to fill in, as the help shows it
metavarWord :: String -> Doc Style
metavarWord = styled Metavar

-- | The hash that names a pending transaction
pendingHashWord :: String -> Doc Style
pendingHashWord = styled PendingHash

fileWord :: String -> Doc Style
fileWord = styled UserText . shellWord

-- | A word of a command typed in a shell, quoted when the shell would read
-- it as more than one word or change it: an account name with a space, for
-- instance
shellWord :: String -> String
shellWord word
  | not (null word) && all plain word = word
  | otherwise = "'" <> concatMap quoted word <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./:@%+=," :: String)
    quoted '\'' = "'\\''"
    quoted c = [c]
