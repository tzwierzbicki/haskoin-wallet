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
import Haskoin.Wallet.Database.Address (CoinBalance (..), addrPath, chainPath, coinBalanceUnspendable, isIntPath, pathText)
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.Keys (KeySource (..), foreignKindDescription)
import Haskoin.Wallet.Labels (AccountImport (..), ImportReport (..), reportNeedsSync)
import Haskoin.Wallet.MessageSigning (signedMessageText)
import Haskoin.Wallet.Cli.Parser (BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..), View (..), program)
import Haskoin.Wallet.Cli.Parser.Help (commandName)
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Tx.BumpFee (PaidBy (..), Reduction (..), Sent, sentId)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SendAll (DustReport (..), inputFee, valueOf, worthSpendingUpTo)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, annotate, fillSep, hcat, hsep, indent, pretty, vcat, (<+>))
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

block :: Natural -> String -> String
block n str =
  case n `safeSubtract` fromIntegral (length str) of
    Just missing -> str <> replicate (fromIntegral missing) ' '
    _ -> str

rblock :: Natural -> String -> String
rblock n str =
  case n `safeSubtract` fromIntegral (length str) of
    Just missing -> replicate (fromIntegral missing) ' ' <> str
    _ -> str

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
-- words, or with --parts its parts only, a blank line between them. The
-- whole mnemonic is not shown with its parts: whoever reads the screen
-- would have the seed.
mnemonicDoc :: Text -> [Text] -> [[Text]] -> Doc Style
mnemonicDoc source ws parts =
  case parts of
    [] -> vcat [heading, wordsDoc ws]
    _ -> vcat . intersperse mempty $ heading : [vcat [text ("Part " <> show i), wordsDoc part] | (i, part) <- zip [1 :: Natural ..] parts]
  where
    heading = text ("Mnemonic of " <> show (length ws) <> " words" <> inParts <> ",") <+> styled Muted ("from " <> cs source)
    inParts = if null parts then "" else " in " <> show (length parts) <> " parts"

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
    unconfirmed = ["unconfirmed" <+> amountPrinter unit v | let v = dBAccountBalanceUnconfirmed acc, v > 0]
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
                  field 11 "Unconfirmed" <> amountPrinter unit (dBAccountBalanceUnconfirmed acc),
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
-- what the file labels that the wallet does not have yet, what hw keeps no
-- label for, and the lines that are not records
importDoc :: ImportReport -> Doc Style
importDoc report = vcat . intersperse mempty $ (accountImportDoc <$> reportAccounts report) <> [vcat summary]
  where
    summary =
      [labelsLine]
        <> [text ("Coins locked: " <> show n) | let n = reportCoinsLocked report, n > 0]
        <> [text ("Coins unlocked: " <> show n) | let n = reportCoinsUnlocked report, n > 0]
        <> ["Not in the wallet yet:" <+> commaList (text <$> missing) | not (null missing)]
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

-- | An account a file names, as accounts shows it, and what became of it.
-- Under a created one, the note on a kind taken on trust, if any, wrapped
-- at the width of the screen.
accountImportDoc :: AccountImport -> Doc Style
accountImportDoc = \case
  AccountCreated acc noteM -> vcat ([accountLine acc, indent 2 (styled Good "created")] <> [indent 2 (sentence (wordsOf (cs note))) | Just note <- [noteM]])
  AccountKept acc Nothing -> vcat [accountLine acc, indent 2 "already in the wallet"]
  AccountKept acc (Just why) -> vcat [accountLine acc, indent 2 ("already in the wallet, and keeps its name:" <+> pretty why)]
  AccountRenamed was acc -> vcat [accountLine acc, indent 2 ("renamed from" <+> pretty was `withStyle` Account)]
  AccountRefused name why -> vcat [pretty name `withStyle` Account, indent 2 (styled Failure "not imported:" <+> pretty why)]

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
                  field 12 "Unconfirmed" <> amountPrinter unit (dBAddressBalanceUnconfirmed addr),
                  field 12 "Spendable" <> amountPrinter unit spendable,
                  field 12 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr)),
                  field 12 "Created" <> text (secondText created)
                ]
        )

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
            "unspendable" <+> unspendable (coinBalanceUnspendable balance)
          ]
    ]
  where
    unspendable 0 = neutralAmount unit 0
    unspendable v = amountIn (Just Waiting) unit (fromIntegral v)

-- | Addresses as balances lists them, their paths aligned, a blank line
-- between them. preparetx --change-back shows the addresses it can send
-- the change back to this way.
coinBalancesPrinter :: AmountUnit -> [(DBAddress, CoinBalance)] -> Doc Style
coinBalancesPrinter unit addrs = vcat . intersperse mempty $ uncurry (coinBalanceDoc unit (pathWidth (fst <$> addrs))) <$> addrs

{- Coins -}

-- | A coin in three lines: its value with its state and its
-- confirmations, the address that holds it with its label, or marked as a
-- change address, and the coin itself, as lockcoin and --coin take it. A
-- coin that shows the label of its transaction has it on a line of its
-- own, below the first, as a transaction does.
coinDoc :: Network -> AmountUnit -> JsonCoin -> Doc Style
coinDoc net unit JsonCoin {..} =
  vcat $
    [columns [amountCell (Just Incoming) "" unit (fromIntegral jsonCoinValue), commaList [state, confirmations]]]
      <> [indent 2 (labelWord jsonCoinTxLabel) | not (Text.null jsonCoinTxLabel)]
      <> [ indent 2 (hsep $ ["at", text (addrToString net jsonCoinAddress)] <> [labelWord jsonCoinAddressLabel | not (Text.null jsonCoinAddressLabel)] <> [changeWord | jsonCoinChange]),
           indent 2 (styled Muted (coinText jsonCoinOutpoint))
         ]
  where
    state = case jsonCoinLock of
      Just LockedByTransaction -> styled Waiting "locked by a pending transaction"
      Just LockedByUser -> styled Waiting "locked by you"
      Nothing
        | jsonCoinSpendable -> styled Good "spendable"
        | otherwise -> styled Waiting "not spendable yet"
    confirmations
      | jsonCoinConfirmations == 0 = styled Waiting "unconfirmed"
      | otherwise = text (confirmationsText jsonCoinConfirmations)

{- Transactions -}

-- A transaction shows in one of two views. Both start with the same first
-- line: its time, its type, its amount and where it stands. The compact
-- view then gives whom it pays and its hash, in two more lines. The
-- verbose view gives every detail below the first line, as fields.

-- | A transaction as a listing shows it, in the view asked for, with its
-- time in the time zone of the computer. A listing of every account names
-- the account of each transaction.
listedTxDoc :: View -> Network -> AmountUnit -> Maybe DBAccount -> TxInfo -> IO (Doc Style)
listedTxDoc view net unit accountM info = do
  time <- localTxTime info
  return $ case view of
    Compact -> compactTxDoc net unit accountM time info
    Verbose -> verboseTxDoc net unit [accountField acc | Just acc <- [accountM]] time info

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
  return $ verboseTxDoc net unit named time info
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

-- | A time to the second, with its time zone: "31 Oct 2026 09:12:40 CET"
secondText :: ZonedTime -> String
secondText = formatTime defaultTimeLocale "%d %b %Y %H:%M:%S %Z"

-- | The first line of a transaction, the same in both views: its time,
-- its type, what it adds to the account or takes from it, fee included,
-- and where it stands. Its columns line up from one transaction to the
-- next.
headlineDoc :: AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
headlineDoc unit time (TxInfo summary status _ _) =
  hcat
    [ maybe (styled Muted (block minuteWidth "time unknown")) (text . minuteText) time,
      "  ",
      text (typeWord (summaryType summary)),
      "  ",
      amountColumn unit (summaryAmount summary),
      "  ",
      statusWords (summaryFeeByte summary) status
    ]

-- | The type of a transaction. The three words have the same length,
-- which keeps the amounts in a column. They hold before a transaction is
-- sent as well as after, unlike "sent" and "received".
typeWord :: TxType -> String
typeWord = \case
  TxDebit -> "Outgoing"
  TxCredit -> "Incoming"
  TxInternal -> "Internal"

-- | What a transaction adds to the account or takes from it, with its
-- sign, green when it comes in and red when it goes out
amountColumn :: AmountUnit -> Integer -> Doc Style
amountColumn unit amnt = amountCell style sign unit amnt
  where
    (style, sign)
      | amnt > 0 = (Just Incoming, "+")
      | amnt < 0 = (Just Outgoing, "")
      | otherwise = (Nothing, "")

-- | An amount in a column: its number after the sign given, right-aligned
-- in a column wide enough for 9 999 bitcoins in any unit, in the style if
-- any, then its unit padded to its plural, so that the amounts of a
-- listing line up. A larger amount pushes the rest of its line.
amountCell :: Maybe Style -> String -> AmountUnit -> Integer -> Doc Style
amountCell style sign unit amnt =
  maybe id annotate style (pretty (rblock 14 (sign <> cs (showAmount unit amnt))))
    <+> pretty (block unitWidth (cs (showUnit unit amnt)))
  where
    unitWidth = fromIntegral (Text.length (showUnit unit 2))

-- | Where a transaction stands, in a few words. One that waits shows its
-- fee rate, which tells how soon a block may take it: unconfirmed, or in
-- preparation. One in conflict can no longer be sent, and a dead one was
-- replaced or dropped from the network: their fee rates no longer matter.
statusWords :: Natural -> TxStatus -> Doc Style
statusWords feeByte = \case
  OnChain status
    | chainDead status -> styled Failure "dead"
    | chainConfirmations status == 0 -> styled Waiting "unconfirmed" <> rate
    | otherwise -> text (confirmationsText (chainConfirmations status))
  Pending status
    | pendingConflicted status -> styled Failure "conflicted"
    | pendingOnline status -> styled Good "sent" <> rate
    | pendingSigned status -> styled Waiting "signed, not sent" <> rate
    | otherwise -> styled Waiting "not signed" <> rate
  where
    rate = ", " <> text (show feeByte <> " sat/vB")

confirmationsText :: Natural -> String
confirmationsText n = counted n "confirmation" "confirmations"

-- | A transaction in three lines: its first line, whom it pays, and the
-- hash to name it by. A transaction the user labelled shows its label
-- below its first line, on a line of its own: the label says what the
-- transaction is for, better than any of its numbers. A replacement that
-- bumpfee prepared also names the transactions it replaces, before its
-- hash.
compactTxDoc :: Network -> AmountUnit -> Maybe DBAccount -> Maybe ZonedTime -> TxInfo -> Doc Style
compactTxDoc net unit accountM time info@(TxInfo summary status label _) =
  vcat $
    [headlineDoc unit time info]
      <> [indent 2 (labelWord label) | not (Text.null label)]
      <> (indent 2 <$> payeeLine net accountM summary)
      <> [indent 2 ("replaces" <+> text (cs (txHashToHex replaced))) | Pending p <- [status], replaced <- pendingReplaces p]
      <> [indent 2 (hashWord status)]

-- | Whom a transaction pays, in one line: the largest output of those it
-- sends to, with its label, and how many more there are. A payment the
-- wallet receives is paid "at" an address of the wallet, the others "to"
-- an address. The account comes first when the listing has every
-- account's. None for a transaction that pays no address and names no
-- account.
payeeLine :: Network -> Maybe DBAccount -> TxSummary -> [Doc Style]
payeeLine net accountM summary =
  [hcat (intersperse ", " parts) | not (null parts)]
  where
    parts = ["account" <+> pretty (dBAccountName acc) `withStyle` Account | Just acc <- [accountM]] <> payee
    payee = case sortOn (Down . txAddressValue) (fst (paidTo summary)) of
      [] -> []
      largest : others -> [hsep ([preposition, addressWords largest] <> more others)]
    preposition = if summaryType summary == TxCredit then "at" else "to"
    more [] = []
    more others = ["and", text (show (length others)), "more"]
    addressWords row =
      hsep $
        text (addrToString net (txAddress row))
          : [labelWord label | Just (_, label) <- [txAddressOwn row], not (Text.null label)]
          <> [changeWord | Just (path, _) <- [txAddressOwn row], isIntPath path]

-- | The label of an address or of a transaction, quoted, which sets it
-- apart on a line
labelWord :: Text -> Doc Style
labelWord label = styled UserText ("\"" <> cs label <> "\"")

-- | The hash to name a transaction by. For one on the chain, its txid,
-- faint, since the user rarely types it. For one in preparation, its
-- nosigHash until it is signed, then its txid, as the commands take it.
hashWord :: TxStatus -> Doc Style
hashWord (OnChain status) = styled Muted (cs (txHashToHex (chainTxid status)))
hashWord (Pending status) = styled PendingHash (cs (txHashToHex (pendingShownHash status)))

-- | A transaction with every detail: its first line, then its fields. The
-- lines given come first: its account in a listing of every account, or
-- its network in an answer. Its label, if any, follows them. Every address
-- it spends from or pays is on a line of its own, with its value, and those
-- of the wallet with their paths and labels, in columns. An address of the
-- wallet it spends from ends with the labels its coins show, if any. A
-- change address is marked as one, except under Change, which says it.
verboseTxDoc :: Network -> AmountUnit -> [Doc Style] -> Maybe ZonedTime -> TxInfo -> Doc Style
verboseTxDoc net unit named time info@(TxInfo summary status label inputLabels) =
  vcat $
    headlineDoc unit time info
      : ( indent 2
            <$> named
              <> [field 9 "Label" <> labelWord label | not (Text.null label)]
              <> statusFields time status
              <> [feeField unit summary]
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
    widest = maximum . (0 :) . map length

-- | A value of an address, as its column shows it: "0.00050000 btc"
valueText :: AmountUnit -> Natural -> String
valueText unit v = cs (showAmount unit (fromIntegral v) <> " " <> showUnit unit (fromIntegral v))

-- | An address of a transaction on one line: the address, its value
-- right-aligned, then for an address of the wallet, its path and its
-- label, or that it is a change address when marked, then the labels its
-- coins show, after the word "from". The value of an address of the
-- wallet is in the style given.
addressRow :: Network -> AmountUnit -> AddressColumns -> Maybe Style -> Bool -> TxAddress -> Doc Style
addressRow net unit widths ownStyle marked row =
  cellsLine $
    [(addressColumn widths, address, text address)]
      <> [(valueColumn widths, value, valueDoc value) | Just v <- [txAddressValue row], let value = valueText unit v]
      <> case txAddressOwn row of
        Just (path, label) ->
          (pathColumn widths, pathToStr path, styled Muted (pathToStr path))
            : [(0, "", labelWord label) | not (Text.null label)]
            <> [(0, "", changeWord) | marked, isIntPath path]
            <> [(0, "", styled Muted "from" <+> commaList (labelWord <$> spentLabels)) | let spentLabels = txAddressCoinLabels row, not (null spentLabels)]
        Nothing -> []
  where
    address = addrToString net (txAddress row)
    style = if isJust (txAddressOwn row) then ownStyle else Nothing
    -- Right-aligned, so that the points line up
    valueDoc value = pretty (replicate (valueColumn widths - length value) ' ') <> maybe id annotate style (pretty value)

-- | Cells side by side, two spaces apart, each with the width of its
-- column and its text. Each is padded to the width of its column, but the
-- last: a line ends without spaces. A right-aligned cell comes padded.
cellsLine :: [(Int, String, Doc Style)] -> Doc Style
cellsLine cells = hcat (intersperse "  " (padded cells))
  where
    padded [] = []
    padded [(_, _, doc)] = [doc]
    padded ((width, content, doc) : rest) = (doc <> pretty (replicate (width - length content) ' ')) : padded rest

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
    held addr path = columns ["at" <+> text (addrToString net addr), styled Muted (pathToStr path)]

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
-- view given.
prettyPrinter :: AmountUnit -> View -> Response -> IO ()
prettyPrinter unit view res = do
  responsePrinter unit view res
  when (responseFailed res) exitFailure

responsePrinter :: AmountUnit -> View -> Response -> IO ()
responsePrinter unit view answer =
  case answer of
    -- The listings, which the screen reads in chunks ('rowsPrinter')
    ResponseAccounts {} -> listing
    ResponseAddresses {} -> listing
    ResponseBalances {} -> listing
    ResponseTxs {} -> listing
    ResponsePendingTxs {} -> listing
    ResponseCoins {} -> listing
    ResponseError err -> renderIO $ errorDoc err
    ResponseMnemonic source ws parts -> renderIO $ mnemonicDoc source ws parts
    ResponseAccount acc spendable -> renderIO =<< listedAccountDoc Verbose unit acc spendable
    -- In green when the command did what it was asked, in red otherwise
    ResponseAccResult _ res txt -> renderIO $ styled (if res then Good else Failure) (cs txt)
    ResponseFile f -> renderIO $ "Written to" <+> styled UserText f
    ResponseImport report -> renderIO $ importDoc report
    ResponseDescriptors held -> renderIO $ vcat (intersperse mempty [descriptorDoc acc descriptor slip132 | (acc, descriptor, slip132) <- held])
    ResponseAddress _ addr spendable ->
      renderIO =<< listedAddressDoc Verbose unit (pathWidth [addr]) addr spendable
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
    -- | A document per row
    rowsDocs :: [Doc Style],
    -- | Whether a blank line sets the rows apart
    rowsApart :: Bool
  }

-- | The rows of an answer that lists, or Nothing for an answer that does
-- not, a blank line between them. The screen reads a listing in chunks,
-- so the paths of its addresses are padded to the longest path of the
-- account on the chains listed, the same for every chunk, rather than to
-- the longest one shown. The accounts, addresses and transactions show in
-- the view given, their times in the time zone of the computer.
rowsPrinter :: AmountUnit -> View -> Response -> IO (Maybe Rows)
rowsPrinter unit view = \case
  ResponseAccounts accs -> do
    docs <- mapM (uncurry (listedAccountDoc view unit)) accs
    return . Just $ Rows (text "There are no accounts to display") docs True
  ResponseAddresses acc addrs -> do
    let pad = longestPathWidth acc (nub (dBAddressInternal . fst <$> addrs))
    docs <- mapM (uncurry (listedAddressDoc view unit pad)) addrs
    return . Just $ Rows (text "There are no addresses to display") docs True
  ResponseBalances acc addrs ->
    let pad = longestPathWidth acc [False, True]
     in return . Just $ Rows (text "There are no addresses with coins to display") (uncurry (coinBalanceDoc unit pad) <$> addrs) True
  ResponseTxs acc txs -> do
    docs <- mapM (listedTxDoc view (accountNetwork acc) unit Nothing) txs
    return . Just $ Rows (text "There are no transactions to display") docs True
  -- Listing every account, each transaction names its own
  ResponsePendingTxs asked held -> do
    let none = case asked of
          Just acc -> "There are no pending transactions in the account " <> cs (dBAccountName acc)
          Nothing -> "There are no pending transactions"
        named acc = if isNothing asked then Just acc else Nothing
    docs <- mapM (\(acc, info) -> listedTxDoc view (accountNetwork acc) unit (named acc) info) held
    return . Just $ Rows (text none) docs True
  ResponseCoins acc coins ->
    return . Just $ Rows (text "There are no coins to display") (coinDoc (accountNetwork acc) unit <$> coins) True
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

-- | A whole listing: its rows, or what to say when it has none
listingDoc :: Rows -> Doc Style
listingDoc (Rows none [] _) = none
listingDoc (Rows _ docs apart) = vcat (if apart then intersperse mempty docs else docs)

-- | A chunk of a listing, the first one or a later one. The first chunk
-- of a listing without rows says so. A later chunk of a listing whose
-- rows are set apart starts with the blank line that sets it apart from
-- the chunk before. An answer that is not a listing is a failure here.
chunkDoc :: AmountUnit -> View -> Bool -> Response -> IO (Doc Style)
chunkDoc unit view first answer = do
  rowsM <- rowsPrinter unit view answer
  return $ case rowsM of
    Just rows
      | first -> listingDoc rows
      | otherwise -> vcat ([mempty | rowsApart rows] <> [listingDoc rows])
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

-- | A command of hw, in the colour of its kind: the colour of the offline
-- commands for one that asks for a mnemonic or private keys
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
