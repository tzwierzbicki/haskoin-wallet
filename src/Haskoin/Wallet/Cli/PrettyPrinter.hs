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
import Data.List (genericLength, intercalate, intersperse, nub, sortOn)
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
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..), PoolPath (..), foreignKindDescription, kindName, poolPathText, portfolioAccounts)
import Haskoin.Wallet.Bip329 (Record (..), RecordType (..), parseKeypath)
import Haskoin.Wallet.Labels (AccountImport (..), FileShown (..), ImportReport (..), PortfolioImport (..), WalletImport (..), importCreated, reportNeedsSync)
import Haskoin.Wallet.MessageSigning (signedMessageText)
import Haskoin.Wallet.Cli.Listing (ChunkAt (..))
import Haskoin.Wallet.Cli.Parser (AddressCode (..), BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..), TxShown (..), program)
import Haskoin.Wallet.Cli.Parser.Help (commandName)
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Cli.QrCode (paymentUri, qrRows)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.ShortId (shortCoinText, shortHashText)
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Tx.BumpFee (PaidBy (..), Reduction (..), Sent, sentId)
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SendAll (DustReport (..), inputFee, valueOf, worthSpendingUpTo)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, annotate, fillSep, flatAlt, group, hcat, hsep, indent, line, pretty, punctuate, vcat, (<+>))
import Text.DocLayout (realLength)
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
-- a text out of its column, so every padding counts columns. A character of ambiguous width counts one, as most terminals
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

{- Where an answer is -}

-- | A portfolio as an answer names it: portfolio0, and its wallet after
-- it when that is not wallet0, as in "portfolio0 of wallet1". A user of
-- one mnemonic has wallet0 alone and never reads its name. The listing of
-- the portfolios names the wallet of each on its own.
portfolioWords :: PortfolioRef -> String
portfolioWords ref =
  cs (portfolioName (refPortfolio ref))
    <> (if refWallet ref == 0 then "" else " of " <> cs (walletName (refWallet ref)))

-- | The line that says where the rows under it are: the portfolio, then
-- the network
placeLine :: Place -> Doc Style
placeLine place =
  columns
    [ text (portfolioWords (placePortfolio place)) `withStyle` Account,
      text (placeNetwork place).name
    ]

-- | The line that names an account: its portfolio, its network and the
-- type of its addresses, as --network and --type pick it
accountRefLine :: AccountRef -> Doc Style
accountRefLine ref =
  columns
    [ text (portfolioWords (placePortfolio (accountRefPlace ref))) `withStyle` Account,
      text (accountRefNetwork ref).name,
      text (cs (kindName (accountRefKind ref)))
    ]

-- | The fields that say where a transaction is, first in its verbose
-- view: its portfolio and its network, each padded to the width given
placeFields :: Natural -> Place -> [Doc Style]
placeFields pad place =
  [ field pad "Portfolio" <> (text (portfolioWords (placePortfolio place)) `withStyle` Account),
    field pad "Network" <> text (placeNetwork place).name
  ]

{- Portfolios -}

-- | A portfolio as a listing shows it, in the view asked for, the time it
-- was created in the time zone of the computer
listedPortfolioDoc :: View -> AmountUnit -> PortfolioView -> IO (Doc Style)
listedPortfolioDoc view unit portfolio = do
  created <- utcToLocalZonedTime (viewCreated portfolio)
  return $ case view of
    Verbose -> verbosePortfolioDoc unit created portfolio
    -- The table is laid out by rowsPrinter ('portfolioRowDocs')
    _ -> compactPortfolioDoc unit created portfolio

-- | The first line of a portfolio: the mark of the active one, its name,
-- its wallet, and its description when it has one. The listing of the
-- portfolios always names the wallet, since it is where the wallets are
-- told apart. The mark is a column of its own before the name, a * for the
-- active portfolio and a space for the others, so that the names line up
-- and the eye finds the active one at the edge of the listing. The lines
-- under the first one are indented past the mark.
portfolioLine :: PortfolioView -> Doc Style
portfolioLine portfolio =
  activeMark portfolio
    <+> columns
      ( [ pretty (portfolioName (refPortfolio (viewRef portfolio))) `withStyle` Account,
          pretty (walletName (refWallet (viewRef portfolio)))
        ]
          <> [labelWord description | let description = viewDescription portfolio, not (Text.null description)]
      )

-- | The mark of the active portfolio, the one the commands work in: a *
-- in the colour of what is as it should be, or a space for another
-- portfolio
activeMark :: PortfolioView -> Doc Style
activeMark portfolio
  | viewActive portfolio = styled Good "*"
  | otherwise = " "

-- | The networks of a portfolio that a short view shows: those it has an
-- address or a coin on. A whole portfolio has an account on every network,
-- most of which a user never touches.
activeNetworks :: PortfolioView -> [NetworkView]
activeNetworks portfolio = [net | net <- viewNetworks portfolio, networkViewAddresses net > 0 || networkViewCoins net > 0]

-- | What a portfolio holds on a network, in one line: what it can spend
-- now, what is unconfirmed when anything is, and how many coins and
-- addresses it has
networkFigures :: AmountUnit -> NetworkView -> Doc Style
networkFigures unit net = commaList (spendable : unconfirmed <> [coins, addresses])
  where
    spendable = "spendable" <+> amountPrinter unit (networkViewSpendable net)
    -- An unconfirmed payment from another wallet cannot be spent yet
    unconfirmed = ["unconfirmed" <+> waitingAmount unit v | let v = networkViewUnconfirmed net, v > 0]
    coins = text (counted (fromIntegral (networkViewCoins net)) "coin" "coins")
    addresses = text (counted (networkViewAddresses net) "address" "addresses")

-- | A portfolio in a few lines: its first line, with the mark of the
-- active portfolio, the description of its wallet when it has one, the
-- date it was created, then a line for each network it has addresses or
-- coins on, with what it holds there
compactPortfolioDoc :: AmountUnit -> ZonedTime -> PortfolioView -> Doc Style
compactPortfolioDoc unit created portfolio =
  vcat $
    portfolioLine portfolio
      : (indent 4 <$> (ofWallet <> [("created" <+> text (dayText created))] <> networks))
  where
    -- What the user said of the wallet, which its name does not tell
    ofWallet = [pretty (walletName (refWallet (viewRef portfolio))) <+> "is" <+> labelWord description | let description = viewWalletDescription portfolio, not (Text.null description)]
    active = activeNetworks portfolio
    pad = fromIntegral (maximum (0 : (length . (.name) . networkViewNetwork <$> active)))
    networks
      | null active = [styled Muted "no addresses yet"]
      | otherwise = [text (block pad (networkViewNetwork net).name) <> "  " <> networkFigures unit net | net <- active]

-- | A portfolio with every detail: its first line, the fields of the
-- portfolio and of its wallet, then every network it has an account on,
-- with each of its balances, and under each network its accounts, each
-- with its path and the keys another wallet takes it by. The output
-- descriptor comes whole, with its origin, the fingerprint of the row
-- Fingerprint and the path of the account. Each account adds its key in
-- the form of SLIP-132, a zpub or a vpub for a bip84 account, for the
-- wallets that take only that form.
--
-- A descriptor row takes 148 columns or more, and a zpub row 126. The
-- terminal wraps such a row, which goes on at the first column of the
-- screen. The portfolios listing has the pager wrap its lines in this
-- view, so that the user can copy a descriptor whole
-- ('Haskoin.Wallet.Cli.Parser.wrapsLines').
verbosePortfolioDoc :: AmountUnit -> ZonedTime -> PortfolioView -> Doc Style
verbosePortfolioDoc unit created portfolio =
  vcat $
    portfolioLine portfolio
      : ( indent 4
            <$> ( [ field 11 "Fingerprint" <> pretty (fingerprintToText (viewFingerprint portfolio)),
                    field 11 "Created" <> text (secondText created)
                  ]
                    <> [field 11 "Wallet" <> labelWord description | let description = viewWalletDescription portfolio, not (Text.null description)]
                    <> concatMap networkBlock (viewNetworks portfolio)
                )
        )
  where
    networkBlock net =
      styled Field (networkViewNetwork net).name
        : ( indent 2
              <$> ( [ field 11 "Confirmed" <> amountPrinter unit (networkViewConfirmed net),
                      field 11 "Unconfirmed" <> waitingAmount unit (networkViewUnconfirmed net),
                      field 11 "Spendable" <> amountPrinter unit (networkViewSpendable net),
                      field 11 "Coins" <> countPrinter (fromIntegral (networkViewCoins net)),
                      field 11 "Addresses" <> countPrinter (networkViewAddresses net)
                    ]
                      <> concatMap accountBlock (networkViewAccounts net)
                  )
          )
    accountBlock keys =
      columns [text (cs (kindName (keysKind keys))), text (pathToStr (keysPath keys))]
        : ( indent 2
              <$> ( [field 10 "Descriptor" <> text (cs (keysDescriptor keys))]
                      <> [field 10 "SLIP-132" <> text (cs key) | Just key <- [keysSlip132 keys]]
                  )
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

-- | Names in a sentence, "or" before the last: "btc", "btc or bch",
-- "btc, bch or btctest4"
orList :: [String] -> String
orList names = case reverse names of
  [] -> ""
  [name] -> name
  lastName : before -> intercalate ", " (reverse before) <> " or " <> lastName

-- | A count with its noun: "no coins", "1 coin", "3 coins"
counted :: Natural -> String -> String -> String
counted 0 _ plural = "no " <> plural
counted 1 singular _ = "1 " <> singular
counted n _ plural = show n <> " " <> plural

{- Syncs -}

-- | Whether a sync is worth a block of its own: the portfolio holds
-- something on the network, or the sync changed something. sync goes over
-- every network of every portfolio, and most of them hold nothing.
syncWorthShowing :: SyncRes -> Bool
syncWorthShowing result =
  syncResCoins result > 0
    || syncResConfirmed result > 0
    || syncResUnconfirmed result > 0
    || syncResTxUpdates result > 0
    || syncResCoinUpdates result > 0
    || syncResRebroadcast result > 0
    || syncResConflicted result > 0
    || syncResNewAddrs result > 0

-- | The syncs worth showing, each with its results, a blank line between
-- them, then one line for each portfolio that names its networks where
-- nothing is held and nothing changed
syncResultsPrinter :: AmountUnit -> [SyncRes] -> Doc Style
syncResultsPrinter unit results =
  vcat . intersperse mempty $ (syncResultDoc unit <$> shown) <> [vcat quietLines | not (null quietLines)]
  where
    shown = filter syncWorthShowing results
    quiet = filter (not . syncWorthShowing) results
    quietLines =
      [ (text (portfolioWords ref) `withStyle` Account) <> "  " <> styled Muted ("nothing on " <> andList [(placeNetwork (syncResPlace r)).name | r <- quiet, placePortfolio (syncResPlace r) == ref])
        | ref <- nub (placePortfolio . syncResPlace <$> quiet)
      ]

-- | Names in a sentence, "and" before the last: "btc", "btc and bch",
-- "btc, bch and btctest4"
andList :: [String] -> String
andList names = case reverse names of
  [] -> ""
  [name] -> name
  lastName : before -> intercalate ", " (reverse before) <> " and " <> lastName

-- | A portfolio synced on a network: where it is, what it holds there,
-- the block it is synced to, and what changed, one kind of change a
-- line. What did not change is left out, and a sync that changed nothing
-- says so.
syncResultDoc :: AmountUnit -> SyncRes -> Doc Style
syncResultDoc unit result =
  vcat $ placeLine (syncResPlace result) : (indent 2 <$> holds : synced : changes)
  where
    holds = commaList (spendable : unconfirmed <> [coins])
    spendable = "spendable" <+> amountPrinter unit (syncResSpendable result)
    unconfirmed = ["unconfirmed" <+> waitingAmount unit v | let v = syncResUnconfirmed result, v > 0]
    coins = text (counted (fromIntegral (syncResCoins result)) "coin" "coins")
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

-- | The networks the host does not serve, in one line: no failure, since
-- no host serves every network
notServedDoc :: [Network] -> Doc Style
notServedDoc nets = styled Muted ("Not served by the store: " <> intercalate ", " ((.name) <$> nets))

-- | The networks a sync or a search left out because their store is down,
-- a line for each with the failure of its store. The word says what was
-- not done there: "not synced", or "left out" for recover, whose
-- network may fail during the search or during the syncs after it.
failedNetworksDoc :: String -> [(Network, Text)] -> Doc Style
failedNetworksDoc notDone nets =
  vcat [text net.name <> "  " <> styled Failure (notDone <> ":") <+> pretty failure | (net, failure) <- nets]

-- | The funds of a seed at accounts hw does not hold, as recover shows
-- them: a line for each account, with its network, its path, its kind, how
-- many of its addresses were used and what they hold, then what to do with
-- the funds. The network comes first, as on the line of an account. The
-- path alone does not tell it, since m/44'/0' is searched on Bitcoin Cash
-- too.
foreignFundsDoc :: AmountUnit -> [(Network, ForeignFunds)] -> Doc Style
foreignFundsDoc unit elsewhere =
  vcat $
    "The seed was paid at paths hw cannot spend from:"
      : (indent 2 . fundsLine <$> elsewhere)
      <> [indent 2 "A wallet that holds such accounts can move the funds to an address of this wallet."]
  where
    netOf (net, _) = net.name
    pathOf (net, funds) = pathToStr (foreignPath net funds)
    -- The widest of the column, so that the next one lines up
    widthOf shown = fromIntegral $ maximum (0 : (length . shown <$> elsewhere))
    fundsLine entry@(_, funds) =
      text (block (widthOf netOf) (netOf entry))
        <> "  "
        <> text (block (widthOf pathOf) (pathOf entry))
        <> "  "
        <> text (foreignKindDescription (foreignKind funds) <> ": " <> counted (foreignUsed funds) "address" "addresses" <> " used, ")
        <> left (foreignBalance funds)
    left 0 = "nothing left"
    left balance = amountPrinter unit balance <+> "left"

{- Imports -}

-- | What importwallet did with a file: each wallet the file names, with
-- what became of it and of its portfolios, then which portfolio is active
-- when the import created one ('activeDoc'), then the keys left aside, a
-- blank line between them, then the labels and locks set, what the file
-- labels that the wallet does not have yet, with the addresses among them
-- that hw would not derive from their key path, what hw keeps no label
-- for, and the lines that are not records
importDoc :: ImportReport -> Maybe ActiveAfter -> Doc Style
importDoc report activeM =
  vcat . intersperse mempty $
    (walletImportDoc False <$> reportWallets report)
      <> [activeDoc active | Just active <- [activeM], not (null (importCreated report))]
      <> [leftAsideDoc "not imported:" name why | AccountLeftAside name why <- reportAccounts report]
      <> [vcat summary]
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
        <> [counted n "record" "records" <> " of the portfolios --portfolio leaves out" | let n = reportOtherPortfolios report, n > 0]
        <> [counted n "label" "labels" <> " of change addresses" | let n = reportChangeAddresses report, n > 0]
        <> [counted n "record" "records" <> " hw keeps no label for" | let n = reportNotKept report, n > 0]
    -- A count with its noun, when there is anything to count
    countedIf n singular plural = [counted n singular plural | n > 0]
    -- Why hw derived none of these addresses from their key path: a file
    -- names any path, and hw generates no further than the scan limit
    -- past the last used address of a chain
    tooFar n = "Of them, " <> counted n "address lies" "addresses lie" <> " over a thousand past the last used address of " <> (if n == 1 then "its" else "their") <> " chain, further than hw generates on the word of a file."

-- | A wallet a file names, and what became of it, or what would with the
-- flag: its name and the fingerprint of its seed, whether it was created
-- or found, then a line for each portfolio the file names, with whether
-- it was created with its accounts or found whole
walletImportDoc :: Bool -> WalletImport -> Doc Style
walletImportDoc dryRun wallet =
  vcat $
    columns
      [ pretty (walletName (walletImportIndex wallet)) `withStyle` Account,
        pretty (fingerprintToText (walletImportFingerprint wallet)),
        status
      ]
      : (indent 2 . portfolioDoc <$> walletImportPortfolios wallet)
  where
    status
      | walletImportCreated wallet = if dryRun then "not in the database, and importwallet creates it" else styled Good "created"
      | otherwise = if dryRun then "in the database" else "already in the database"
    -- A portfolio created gets every account at once, one it has gets
    -- none
    portfolioDoc p =
      columns
        [ pretty (portfolioName (portfolioImportIndex p)) `withStyle` Account,
          commaList (text <$> if portfolioImportCreated p then [created, added (genericLength portfolioAccounts)] else [added 0])
        ]
    created = if dryRun then "importwallet creates it" else "created"
    added 0 = "nothing to add"
    added n = counted n "account" "accounts" <> (if dryRun then " to add" else " added")

-- | A key of a file that is not one of an account of a wallet of hw: the
-- start of the key, and why it is left aside
leftAsideDoc :: String -> Text -> Text -> Doc Style
leftAsideDoc notDone name why =
  vcat [pretty (visibleText name) `withStyle` Account, indent 2 (styled Failure notDone <+> pretty why)]

{- Files of a wallet -}

-- | A file of a wallet as show shows it, a blank line between its parts.
-- Each wallet comes first, with what importwallet would do with it and
-- with its portfolios. Then each account comes with the labels and locks
-- of its records, those that have any, and the keys importwallet would
-- leave aside. Then come the labels and locks of no account of the file,
-- and last what hw keeps no label for and the lines that are not records,
-- in the words of importwallet.
walletFileDoc :: FileShown -> Doc Style
walletFileDoc shown =
  vcat . intersperse mempty $
    (walletImportDoc True <$> shownWallets shown)
      <> [doc | (outcome, records) <- shownAccounts shown, doc <- fileAccountDocs outcome records]
      <> [vcat ("Labels without an account in the file" : (indent 2 <$> recordsDocs others)) | let others = shownOthers shown, not (null others)]
      <> [vcat rest | not (null rest)]
  where
    rest =
      ["Left aside:" <+> commaList (text <$> aside) | not (null aside)]
        <> [styled Failure "Lines that are not records:" <+> text (intercalate ", " (show <$> unreadable)) | let unreadable = shownUnreadable shown, not (null unreadable)]
    aside =
      [counted n "record" "records" <> " of the portfolios --portfolio leaves out" | let n = shownOtherPortfolios shown, n > 0]
        <> [counted n "label" "labels" <> " of change addresses" | let n = shownChangeLabels shown, n > 0]
        <> [counted n "record" "records" <> " hw keeps no label for" | let n = shownNotKept shown, n > 0]

-- | An account of a file with the labels and locks of its records, when
-- it has any: a whole portfolio has twelve accounts, and most carry no
-- label. A key importwallet would leave aside comes with the reason.
fileAccountDocs :: AccountImport -> [Record] -> [Doc Style]
fileAccountDocs outcome records = case outcome of
  AccountAdded ref -> withRecords ref
  AccountHeld ref -> withRecords ref
  AccountLeftAside name why -> [leftAsideDoc "importwallet leaves it aside:" name why]
  where
    withRecords ref = [vcat (accountRefLine ref : (indent 2 <$> recordsDocs records)) | not (null records)]

-- | The labels and locks of records, under a heading for each kind. An
-- address comes after its key path, when its record gives one, the paths
-- padded to the longest. A public key comes in place of an address, in
-- hexadecimal. A transaction comes by its txid and a coin by its
-- outpoint, in full: the wallet may hold neither, so a short id could
-- name nothing. What comes from the file is written out, never as an
-- order to the terminal.
recordsDocs :: [Record] -> [Doc Style]
recordsDocs records =
  section "Address labels" (labelledAddressRow <$> ofTypes [AddrRecord, PubkeyRecord])
    <> section "Transaction labels" (txRow <$> ofTypes [TxRecord])
    <> section "Coins" (coinRow <$> ofTypes [OutputRecord])
  where
    ofTypes kinds = filter ((`elem` kinds) . recordType) records
    section :: String -> [Doc Style] -> [Doc Style]
    section title rows = [vcat (styled Field title : (indent 2 <$> rows)) | not (null rows)]
    keypathOf r = maybe "" pathToStr (parseKeypath =<< recordKeypath r)
    keypathWidth = fromIntegral (maximum (0 : (length . keypathOf <$> ofTypes [AddrRecord, PubkeyRecord])))
    reference r = pretty (visibleText (recordRef r))
    labelOf r = [labelWord label | Just label <- [recordLabel r], not (Text.null label)]
    labelledAddressRow r = columns ([text (block keypathWidth (keypathOf r)) | keypathWidth > 0] <> [reference r] <> labelOf r)
    txRow r = columns (reference r : labelOf r)
    coinRow r = columns [reference r, if recordSpendable r == Just False then "locked" else "unlocked"]

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

-- | An address as an answer shows it, with where it is, and with its QR
-- code when the command gives the address out and someone can scan the
-- screen
addressAnswerDoc :: AddressCode -> AmountUnit -> AccountRef -> DBAddress -> Word64 -> IO (Doc Style)
addressAnswerDoc code unit ref addr spendable = do
  details <- listedAddressDoc Verbose unit (pathWidth [addr]) addr spendable
  terminal <- hIsTerminalDevice stdout
  let rows
        | code == WithCode && terminal = fromMaybe [] (qrRows (paymentUri (accountRefNetwork ref) (dBAddressAddress addr)))
        | otherwise = []
  return (withCodeDoc (vcat [details, indent 2 (accountRefField 12 ref)]) rows)

-- | The field that says the account of an address: its portfolio, its
-- network and the type of its addresses
accountRefField :: Natural -> AccountRef -> Doc Style
accountRefField pad ref =
  field pad "Account"
    <> columns
      [ text (portfolioWords (placePortfolio (accountRefPlace ref))) `withStyle` Account,
        text (accountRefNetwork ref).name,
        text (cs (kindName (accountRefKind ref)))
      ]

-- | Text, then a QR code drawn in the rows given. The code goes under the
-- text and never beside it: a terminal wraps its long lines again when it
-- is resized, which would mix the text into the code.
withCodeDoc :: Doc Style -> [Text] -> Doc Style
withCodeDoc details rows = vcat (details : (annotate QrCode . pretty <$> rows))

-- | The address at a path of the account that show derives, when the
-- wallet does not hold it: its path and the address cut, as the first
-- line of addresses, then its account, then what the cut form is for. The wallet holds no row for it, so it has no label and no
-- balance. The user compares its ends with the address the online
-- computer showed, and the note says why the middle is missing, which a
-- user would otherwise take for a fault ("Haskoin.Wallet.CutAddress").
derivedAddrDoc :: AccountRef -> SoftPath -> Text -> Doc Style
derivedAddrDoc ref path cut =
  vcat
    [ columns [text (cs (pathText path)), pretty cut],
      indent 2 (accountRefField 9 ref),
      mempty,
      sentence (wordsOf (cutAddressNote path))
    ]

-- | How to check an address with its cut form, and why its middle is
-- hidden. The user reads it in the middle of a payment, about to give the
-- address out, so it says what to do in each case and alarms only when
-- the ends differ. Why the address must not be paid from this screen, that
-- it may lie past the gap, is in the help and the README instead.
cutAddressNote :: SoftPath -> String
cutAddressNote path =
  "Compare both ends with the address that receive gave out at "
    <> pathToStr path
    <> " on the online computer."
    <> " If they match, that address is of this account, and you can give it out."
    <> " If they differ, the online computer may have been tampered with, and you should not give the address out."
    <> " Nobody can make another address with the same ends."
    <> " This screen is for checking only, so the middle of the address is hidden."
    <> " Give out the address as receive showed it."

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

-- | The first line of a coin in the verbose view: the time of the
-- transaction that paid it, its value, then its state and its
-- confirmations, written by the function given, in the words of
-- 'coinLineCells'
coinLine :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinLine countText unit time coin = columns (coinLineCells "locked by a pending transaction" countText unit time coin)

-- | What the first line of a coin gives: the time of the transaction that
-- paid it, its value, then its state, with the words given for a coin a
-- pending transaction spends, and its confirmations, written by the
-- function given. The time and the value keep the columns of the first
-- line of a transaction, so that the values of a listing line up.
coinLineCells :: String -> (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> [Doc Style]
coinLineCells lockedByTransaction countText unit time coin =
  [ minuteWord time,
    amountCell (Just Incoming) "" unit (fromIntegral (jsonCoinValue coin)),
    commaList [coinState lockedByTransaction coin, confirmations]
  ]
  where
    confirmations
      | jsonCoinConfirmations coin == 0 = styled Waiting "unconfirmed"
      | otherwise = text (countText (jsonCoinConfirmations coin))

-- | Whether a coin can be spent now, in the colour of its state
-- ('coinStateText'): green when it is spendable, yellow when it waits
coinState :: String -> JsonCoin -> Doc Style
coinState lockedByTransaction coin = styled style (coinStateText lockedByTransaction coin)
  where
    style = if isNothing (jsonCoinLock coin) && jsonCoinSpendable coin then Good else Waiting

-- | Whether a coin can be spent now, in words: spendable, locked by a
-- pending transaction or by the user, or not spendable yet, as it waits
-- for a confirmation. The words for a coin a pending transaction spends
-- are given: the compact view and a CSV file spell them out, and the table
-- has little room.
coinStateText :: String -> JsonCoin -> String
coinStateText lockedByTransaction coin = case jsonCoinLock coin of
  Just LockedByTransaction -> lockedByTransaction
  Just LockedByUser -> "locked by you"
  Nothing
    | jsonCoinSpendable coin -> "spendable"
    | otherwise -> "not spendable yet"

-- | The address that holds a coin, as the verbose view gives it: in full,
-- then its path, with its label, or the mark of a change address, on the
-- line below, as every view that gives an address in full does. The
-- compact view keeps them on the line of the address instead
-- ('compactCoinDoc').
coinAddressDoc :: Network -> JsonCoin -> Doc Style
coinAddressDoc net coin =
  withNotesBelow
    (fullAddressWords net (jsonCoinAddress coin) (pathInAccount <$> jsonCoinPath coin))
    (ownAddressNoteBelow (jsonCoinAddressLabel coin) (jsonCoinChange coin))

-- | A coin in two lines, as the compact view of a transaction is laid
-- out. The first gives the short id of the coin, which lockcoin and
-- --coin take ("Haskoin.Wallet.ShortId"), first, as in the table, then
-- the time of the transaction that paid it, its value, its state and its
-- confirmations, counted up to 999+. A coin a pending transaction spends
-- is "locked by tx", in the words of the table, so that the line keeps to
-- 80 columns. The second line gives the address that holds the coin in
-- full, then its path, then its label, or the mark of a change address. It
-- is the one compact view that gives an address of the wallet in full:
-- what tells a coin apart is the address that holds it, which another
-- wallet or a block explorer shows, more than its path. The label of the
-- address stays, since a user may sort coins by the labels of their
-- addresses. It stays on the line of the address too, so that every coin
-- takes two lines: the one exception to the rule that puts the label of
-- an address in full on the line below it ('withNotesBelow'). A label
-- ends the line, so a long one pushes nothing, and it is whole, as on a
-- row of a table. The label of the transaction that paid the coin says
-- less of the coin, and only the verbose view gives it.
compactCoinDoc :: Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
compactCoinDoc net unit time coin =
  vcat
    [ columns (shortCoinWord coin : coinLineCells "locked by tx" cappedConfirmations unit time coin),
      indent 2 ("at" <+> columns (fullAddressWords net (jsonCoinAddress coin) (pathInAccount <$> jsonCoinPath coin) : ownAddressNote (jsonCoinAddressLabel coin) (jsonCoinChange coin)))
    ]

-- | The short id of a coin at the start of a line, faint, padded to the
-- width of a coin at one of the first ten outputs of its transaction,
-- a1075db5:1. A coin further down pushes the rest of its line.
shortCoinWord :: JsonCoin -> Doc Style
shortCoinWord coin = styled Muted (block shortCoinWidth (shortCoinText (jsonCoinOutpoint coin)))

-- | The width of the short id of a coin at one of the first ten outputs of
-- its transaction: 8 digits, a colon and one digit
shortCoinWidth :: Natural
shortCoinWidth = 10

-- | A coin with every detail: its first line, with the full count of its
-- confirmations, then its fields. The label of its transaction comes
-- first, as in the verbose view of a transaction, then the coin in full
-- and its short id, the block that holds it, the time of its transaction to the second with its
-- time zone, and its address in full with its path. The label of the
-- address, or the mark of a change address, is on the line below the
-- address, as in the verbose view of a transaction. An unconfirmed coin
-- is in no block yet, and shows none.
verboseCoinDoc :: Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
verboseCoinDoc net unit time coin =
  vcat $
    coinLine fullConfirmations unit time coin
      : ( indent 2
            <$> [field 8 "Label" <> labelWord label | let label = jsonCoinTxLabel coin, not (Text.null label)]
              <> [ field 8 "Coin" <> text (coinText (jsonCoinOutpoint coin)),
                   field 8 "Short id" <> text (shortCoinText (jsonCoinOutpoint coin))
                 ]
              <> [field 8 "Block" <> text (show height) | Store.BlockRef height _ <- [jsonCoinBlock coin]]
              <> [field 8 "Time" <> text (secondText t) | Just t <- [time]]
              <> [field 8 "Address" <> coinAddressDoc net coin]
        )

{- Transactions -}

-- A transaction shows in one of three views. The compact and the verbose
-- views give what changed and where it stands on their first line. The
-- compact view takes two lines. The first starts with the short id, the
-- first 8 digits of the hash, then gives the time, the amount and where
-- the transaction stands, the confirmations counted up to 999+. The second
-- gives the balance after it, in txs, then what it is for, in the words of
-- the table ('purposeCell'), which names an address of the wallet by its
-- path. The verbose view starts with the time, the type, the amount and
-- the full count of the confirmations, then gives every detail as fields:
-- every address in full, and the hash in full with its short id. The
-- table gives a row of columns under a header: the short id, the time,
-- the type, the amount, the balance, the confirmations, and what the
-- transaction is for.

-- | A transaction as a listing shows it, in the view asked for, with its
-- time in the time zone of the computer. A listing of every portfolio
-- names the portfolio of each transaction. The balance after it is the
-- one txs gives: the other listings have none.
listedTxDoc :: View -> Network -> AmountUnit -> Maybe PortfolioRef -> Maybe Integer -> TxInfo -> IO (Doc Style)
listedTxDoc view net unit portfolioM balanceM info = do
  time <- localTxTime info
  return $ case view of
    Compact -> compactTxDoc net unit portfolioM balanceM time info
    Verbose -> verboseTxDoc net unit (whereFields net portfolioM info) balanceM time info
    Table -> txRowDoc net unit balanceM time info

-- | The fields that say where a transaction is, first in its verbose
-- view: its portfolio, when the view names it, its network, and the types
-- of the portfolio's addresses it involves, when it involves any. A
-- portfolio holds coins of both types on Bitcoin, and the types tell
-- which of them a transaction touched.
whereFields :: Network -> Maybe PortfolioRef -> TxInfo -> [Doc Style]
whereFields net portfolioM info =
  [field 9 "Portfolio" <> (text (portfolioWords ref) `withStyle` Account) | Just ref <- [portfolioM]]
    <> [field 9 "Network" <> text net.name]
    <> [field 9 "Types" <> text (intercalate ", " (cs . kindName <$> kinds)) | let kinds = summaryKinds (txInfoSummary info), not (null kinds)]

-- | The one transaction a command answers with, with every detail, naming
-- its portfolio when it is one of the wallet, and its network. The user
-- sees whose coins it moves, and which coins: private keys, coins and the
-- legacy addresses of Bitcoin Cash read the same on Bitcoin, and a sweep
-- outside the wallet is on the network of its file.
txAnswerDoc :: Network -> Maybe PortfolioRef -> AmountUnit -> TxInfo -> IO (Doc Style)
txAnswerDoc net portfolioM unit info = do
  time <- localTxTime info
  return $ verboseTxDoc net unit (whereFields net portfolioM info) Nothing time info

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

-- | A time to the day, as the date a portfolio was created shows
dayText :: ZonedTime -> String
dayText = formatTime defaultTimeLocale "%d %b %Y"

-- | The first line of the verbose view of a transaction: its time, its
-- type, what it adds to the account or takes from it, fee included, and
-- where it stands. Its columns line up from one transaction to the next.
-- The function given writes the confirmations: 'fullConfirmations' in the
-- verbose view, 'cappedConfirmations' where a count that grows without end
-- would push the rest of a line.
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

-- | A transaction in two lines, for a listing that the eye runs down.
-- The first line gives the short id, the time, what the transaction adds
-- to the account or takes from it, with its sign and its colour, and
-- where it stands ('compactHeadline'). The short id comes first, as in
-- the first column of the table: the eye finds it there to copy it, and
-- each transaction starts where its id does.
--
-- The second line gives the details, a comma between them: the account,
-- in a listing of every account; the balance after the transaction, in
-- txs; the transactions a replacement that bumpfee prepared replaces; and
-- last what the transaction is for, in the words of the last column of
-- the table ('purposeCell'). The label of a payment says what it is for
-- better than the address it pays, and takes its place. What it is for
-- has no width to fix in advance, so it ends the line: a long address or
-- label pushes nothing after it. The verbose view gives every address in
-- full, and the type of the transaction, which the sign of the amount and
-- the words "at" and "to" tell here. A transaction with no detail, which
-- pays no address and has no label, takes one line.
compactTxDoc :: Network -> AmountUnit -> Maybe PortfolioRef -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
compactTxDoc net unit portfolioM balanceM time info@(TxInfo summary status label _) =
  vcat $
    compactHeadline unit time info
      : [indent 2 (hcat (intersperse ", " details)) | not (null details)]
  where
    details =
      [(text (portfolioWords ref) `withStyle` Account) <+> text net.name | Just ref <- [portfolioM]]
        <> ["balance" <+> neutralAmount unit balance | Just balance <- [balanceM]]
        <> ["replaces" <+> shortIdList replaced | Pending p <- [status], let replaced = pendingReplaces p, not (null replaced)]
        <> [purpose | not (null (renderPlain purpose))]
    purpose = purposeCell net label summary

-- | The first line of the compact view of a transaction: its short id,
-- faint for one on the chain ('hashWord'), its time to the minute, what
-- it adds to the account or takes from it, fee included, and where it
-- stands, the confirmations counted up to 999+. Its columns line up from
-- one transaction to the next. It leaves out the type: the sign and the
-- colour of the amount tell what comes in and what goes out, so the line
-- keeps to 80 columns.
compactHeadline :: AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
compactHeadline unit time (TxInfo summary status _ _) =
  columns
    [ hashWord status,
      minuteWord time,
      amountColumn unit (summaryAmount summary),
      statusWords cappedConfirmations (summaryFeeByte summary) status
    ]

-- | Short ids one after the other, as English lists them: "a1075db5",
-- "a1075db5 and b2d4f6a8", "a1075db5, b2d4f6a8 and 3c5e7a90"
shortIdList :: [TxHash] -> Doc Style
shortIdList hashes = case reverse (text . shortHashText <$> hashes) of
  [] -> mempty
  [only] -> only
  lastOne : others -> hcat (intersperse ", " (reverse others)) <+> "and" <+> lastOne

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

-- | Whom a transaction pays: "at /0/3 \"Rent\"", "to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 2 more". An address of the
-- wallet is named by its path, then by what the function given says of
-- its path and its label: 'purposeCell' gives the label of the
-- transaction there, or else that of the address, or else the mark of a
-- change address. The address of another wallet is in full.
payeeWordsNoted :: Network -> (PoolPath -> Text -> [Doc Style]) -> Payee -> Doc Style
payeeWordsNoted net noteOf (Payee preposition row others) =
  hsep $ [text (cs preposition), target] <> note <> more
  where
    target = case txAddressOwn row of
      Just (path, _) -> ownPathWord path
      Nothing -> text (addrToString net (txAddress row))
    -- A path is short: its label stays on its line
    note = case txAddressOwn row of
      Just (path, label) -> noteOf path label
      Nothing -> []
    more = if others > 0 then ["and", text (show others), "more"] else []

{- The addresses of the wallet -}

-- An address of the wallet is blue, with its label ('OwnAddress'), so
-- that it stands apart from the addresses of others in a transaction, and
-- so that a label on the line below its address is seen to go with it.
-- The compact views and the table name it by its type and its derivation
-- path, which is short and says which address of the portfolio it is. The verbose views
-- give it in full, with its path: the address is what another wallet or a
-- block explorer shows. The compact view of a coin does too, since the
-- address is what tells a coin apart ('compactCoinDoc').

-- | The path of an address of the wallet, in its colour, with the type
-- of its account in front, as in bip84/0/3. A portfolio has an address at
-- /0/3 in each of its accounts on a network, and the type tells them
-- apart where the address itself is not written out.
ownPathWord :: PoolPath -> Doc Style
ownPathWord path = styled OwnAddress (cs (poolPathText path))

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
-- path is short ('purposeCell'). So does the compact view of a coin, the
-- one exception among the views that give an address in full, so that
-- each coin takes two lines ('compactCoinDoc').
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

-- | The short id to name a transaction by, the first 8 digits of its hash
-- ("Haskoin.Wallet.ShortId"). For one on the chain, that of its txid,
-- faint, since the user rarely types it. For one in preparation, that of
-- its nosigHash until it is signed, then that of its txid, as the
-- commands take it.
hashWord :: TxStatus -> Doc Style
hashWord (OnChain status) = styled Muted (shortHashText (chainTxid status))
hashWord (Pending status) = styled PendingHash (shortHashText (pendingShownHash status))

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
-- which ends the row and pushes nothing. Nothing in a table is cut: a
-- label, an address and the name of an account show whole, however long.
-- On a terminal, less keeps each row on one line and scrolls it sideways
-- ("Haskoin.Wallet.Cli.Pager"), so a long cell makes its row wider
-- without breaking the columns. A cut would save room on the screen, but
-- hide what tells two labels apart, and an address read in part is how a
-- lookalike passes for the right one.
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
-- which says more to the user. Nothing in the cell is cut.
--
-- The first column gives the short id of the transaction, the first 8
-- digits of its txid, which the commands take ("Haskoin.Wallet.ShortId"),
-- as the first line of the compact view starts with it. The 64 digits of
-- the txid would make a row too wide for most screens. The verbose view
-- gives them.

-- | The columns of the table of txs
txColumns :: AmountUnit -> [Column]
txColumns unit =
  [ column "Txid" shortIdWidth LeftSide,
    column "Time" minuteWidth LeftSide,
    column "Type" typeWidth LeftSide,
    column (amountHeading "Amount" unit) amountWidth RightSide,
    column (amountHeading "Balance" unit) amountWidth RightSide,
    -- Its cells take four characters at most, "999+" and "dead": the
    -- heading sets its width
    column "Confs" 4 RightSide,
    column "Description" 0 LeftSide
  ]

-- | The width of a column of short ids: 8 digits
shortIdWidth :: Natural
shortIdWidth = 8

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
-- view, as short as they go ('pendingStatusText'), in the colour of where
-- it stands: red in conflict, green once sent, yellow before
pendingStatusWord :: PendingStatus -> Doc Style
pendingStatusWord status = styled style (pendingStatusText status)
  where
    style
      | pendingConflicted status = Failure
      | pendingOnline status = Good
      | otherwise = Waiting

-- | How far a transaction in preparation got, in words: not signed,
-- signed, sent, or conflicted, which it stays once it is, sent or not
pendingStatusText :: PendingStatus -> String
pendingStatusText status
  | pendingConflicted status = "conflicted"
  | pendingOnline status = "sent"
  | pendingSigned status = "signed"
  | otherwise = "not signed"

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

-- | A transaction on one row of the table of txs: its short id, its time,
-- its type, its amount, the balance after it and its confirmations, each
-- in its column, then what it is for ('purposeCell'). The amount has its sign and its
-- colour, as on the first line of the compact view. A dead transaction leaves its balance empty, since
-- the balance does not count it. The table lists one account, so no row
-- names its account.
txRowDoc :: Network -> AmountUnit -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
txRowDoc net unit balanceM time (TxInfo summary status label _) =
  tableLine (zip (txColumns unit) cells)
  where
    cells =
      [ hashWord status,
        timeCell time,
        text (typeWord (summaryType summary)),
        signedAmountCell unit (summaryAmount summary),
        maybe mempty (text . cs . showAmount unit) balanceM,
        tableConfirmations status,
        purposeCell net label summary
      ]

-- | What a transaction is for, as the last cell of the table of txs and
-- the end of the compact view say it. A payment received, or one between the addresses of the wallet, gives the path of
-- the address it pays the most, then the label of the transaction, or
-- else the label of that address, or else that it is a change address:
-- "at /0/3 \"Salary\"". A payment gives its label alone, or else the
-- address of the other wallet it pays the most: "to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU". Either tells how many other
-- addresses it pays. A label and an address are whole, however long.
purposeCell :: Network -> Text -> TxSummary -> Doc Style
purposeCell net label summary = case mainPayee summary of
  Just payee
    | isJust (txAddressOwn (payeeRow payee)) || Text.null label -> payeeWordsNoted net ownNote payee
  _ -> if Text.null label then mempty else labelWord label
  where
    -- The label of the transaction says what it is for better than the
    -- label of its address
    ownNote path ownLabel
      | Text.null label = ownAddressNote ownLabel (isIntPath (pathInAccount path))
      | otherwise = [labelWord label]

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
addressTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Bool -> [Column]
addressTableColumns unit net kinds pad internal =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth net kinds) LeftSide,
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
addressWidth :: Network -> [AccountKind] -> Natural
addressWidth net kinds = fromIntegral (maximum (0 : [length (addrToString net (widest kind)) | kind <- kinds]))
  where
    widest kind = case kind of
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
      | otherwise = labelWord (dBAddressLabel addr)

-- | An amount that waits to be spent, as a number in a column of a table:
-- yellow, unless it is zero, as 'waitingAmount'
waitingAmountCell :: AmountUnit -> Word64 -> Doc Style
waitingAmountCell unit amnt = (if amnt > 0 then annotate Waiting else id) (text (cs (showAmount unit (fromIntegral amnt))))

-- | An amount the wallet holds, as a number in a column of a table: green,
-- unless it is zero
heldAmountCell :: AmountUnit -> Word64 -> Doc Style
heldAmountCell unit amnt = (if amnt > 0 then annotate Incoming else id) (text (cs (showAmount unit (fromIntegral amnt))))

{- The table of portfolios -}

-- The table of portfolios gives a row to each network of each portfolio
-- that has addresses or coins, as the compact view gives it a line. A
-- portfolio without any gets one row with no network, so that it shows.
-- The listing is read whole, so its first columns are as wide as their
-- widest cells.

-- | The columns of the table of portfolios, for the portfolios listed. The
-- first one, without a heading, marks the active portfolio, as the other
-- views do.
portfolioTableColumns :: AmountUnit -> [PortfolioView] -> [Column]
portfolioTableColumns unit portfolios =
  [ column "" 1 LeftSide,
    column "Wallet" (widestCell (cs . walletName . refWallet . viewRef) portfolios) LeftSide,
    column "Portfolio" (widestCell (cs . portfolioName . refPortfolio . viewRef) portfolios) LeftSide,
    column "Network" (widestCell (\net -> (networkViewNetwork net).name) (concatMap activeNetworks portfolios)) LeftSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Unconfirmed" unit) amountWidth RightSide,
    -- The headings set the widths: 99 999 coins, 999 999 999 addresses
    column "Coins" 1 RightSide,
    column "Addresses" 1 RightSide,
    column "Description" 0 LeftSide
  ]

-- | The width of the widest cell of a column, which a listing read whole
-- can size its columns to
widestCell :: (a -> String) -> [a] -> Natural
widestCell cell elements = fromIntegral (maximum (0 : [shownWidth (cell e) | e <- elements]))

-- | A portfolio on the rows of the table of portfolios: one for each
-- network it has addresses or coins on, or one without a network
portfolioRowDocs :: AmountUnit -> [Column] -> PortfolioView -> [Doc Style]
portfolioRowDocs unit headed portfolio =
  case activeNetworks portfolio of
    [] -> [tableLine (zip headed (named <> [mempty, mempty, mempty, mempty, mempty, describedCell]))]
    active ->
      [ tableLine . zip headed $
          named
            <> [ text (networkViewNetwork net).name,
                 heldAmountCell unit (networkViewSpendable net),
                 waitingAmountCell unit (networkViewUnconfirmed net),
                 text (show (networkViewCoins net)),
                 text (show (networkViewAddresses net)),
                 describedCell
               ]
        | net <- active
      ]
  where
    named =
      [ activeMark portfolio,
        pretty (walletName (refWallet (viewRef portfolio))),
        pretty (portfolioName (refPortfolio (viewRef portfolio))) `withStyle` Account
      ]
    describedCell =
      hsep [labelWord description | let description = viewDescription portfolio, not (Text.null description)]

{- The table of coins -}

-- The table of coins gives what the compact view gives: the short id of
-- the coin first, the time of the transaction that paid it, its value,
-- its state, its confirmations, the address that holds it in full with
-- its path, and the label of the address. The coin in full, TXID:INDEX,
-- takes 66 characters at least, which would make a row too wide. The
-- verbose view gives it.

-- | The columns of the table of coins. The address takes the width of
-- the widest address the account can have ('addressWidth'), and the path
-- the width given, that of the longest path of the account on its two
-- chains, since coins lists the coins of both.
coinTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> [Column]
coinTableColumns unit net kinds pad =
  [ column "Coin" coinIdWidth LeftSide,
    column "Time" minuteWidth LeftSide,
    column (amountHeading "Value" unit) amountWidth RightSide,
    column "State" coinStateWidth LeftSide,
    column "Confs" 4 RightSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column "Path" pad LeftSide,
    column "Label" 0 LeftSide
  ]

-- | The width of the column of the short ids of coins: 8 digits, a colon
-- and an index of up to three digits. A coin past the thousandth output of
-- its transaction pushes the rest of its row.
coinIdWidth :: Natural
coinIdWidth = 12

-- | The width of the column of the states of coins: that of "not
-- spendable yet", the longest
coinStateWidth :: Natural
coinStateWidth = 17

-- | A coin on one row of its table, under the columns given: its short
-- id, faint, the time of the transaction that paid it, its value in
-- green, its state, with "locked by tx" for a coin a pending transaction
-- spends, its confirmations, the address in the colour of the wallet's
-- addresses, its path, faint, then the label of the address in that
-- colour, or that it is a change address. The address and its label share a colour, as in
-- the compact view.
coinRowDoc :: Network -> AmountUnit -> [Column] -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinRowDoc net unit headed time coin =
  tableLine $
    zip
      headed
      [ styled Muted (shortCoinText (jsonCoinOutpoint coin)),
        timeCell time,
        heldAmountCell unit (jsonCoinValue coin),
        coinState "locked by tx" coin,
        confsCell (jsonCoinConfirmations coin),
        ownAddressWord net (jsonCoinAddress coin),
        maybe mempty (trailingPathWord . pathInAccount) (jsonCoinPath coin),
        -- The label has a column of its own
        hsep (ownAddressNote (jsonCoinAddressLabel coin) (jsonCoinChange coin))
      ]

{- The table of balances -}

-- The table of balances gives what the compact view gives: the path, the
-- address, its three amounts, and its label, or that it is a change
-- address. balances lists the addresses of both chains together.

-- | The columns of the table of balances. The path takes the width given,
-- that of the longest path of the account on its two chains, and the
-- address that of the widest address the account can have.
balanceTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> [Column]
balanceTableColumns unit net kinds pad =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth net kinds) LeftSide,
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
      | not (Text.null (dBAddressLabel addr)) = labelWord (dBAddressLabel addr)
      | dBAddressInternal addr = changeWord
      | otherwise = mempty

{- The table of pending transactions -}

-- The table of pending transactions gives what the compact view gives:
-- the short id of the hash the other commands take first, the time it was
-- created, its type, its amount, how far it got, its fee rate, and what
-- it is for, as the table of txs says it. A listing of every account
-- names the account of each in a column after the short id. The listing
-- is read whole ('listingWhole'), so that column is as wide as the
-- longest name it shows, as in the table of accounts. The hash in full
-- would make a row too wide. The verbose view gives it.

-- | The columns of the table of pending transactions: the short id first,
-- then a column of accounts when the listing has every account's, those
-- given, whose longest name sets its width
pendingTableColumns :: AmountUnit -> [Place] -> [Column]
pendingTableColumns unit places =
  [ column "Hash" shortIdWidth LeftSide,
    column "Portfolio" (widestCell (portfolioWords . placePortfolio) places) LeftSide,
    column "Network" (widestCell (\place -> (placeNetwork place).name) places) LeftSide
  ]
    <> [ column "Time" minuteWidth LeftSide,
         column "Type" typeWidth LeftSide,
         column (amountHeading "Amount" unit) amountWidth RightSide,
         column "Status" pendingStatusWidth LeftSide,
         -- The fee rate has a few digits: the heading sets the width
         column "sat/vB" 1 RightSide,
         column "Description" 0 LeftSide
       ]

-- | The width of the column of the statuses of pending transactions: that
-- of "not signed" and "conflicted", the longest
pendingStatusWidth :: Natural
pendingStatusWidth = 10

-- | A pending transaction on one row of its table, under the columns
-- given: its short id, its account when given, the time it was created,
-- its type, its amount, how far it got, its fee rate, and what it is for
-- ('purposeCell'). One in conflict can no longer be sent: its fee rate no
-- longer matters, and its cell is empty.
pendingRowDoc :: AmountUnit -> [Column] -> Place -> Maybe ZonedTime -> TxInfo -> Doc Style
pendingRowDoc unit headed place time (TxInfo summary status label _) =
  tableLine . zip headed $
    [ hashWord status,
      text (portfolioWords (placePortfolio place)) `withStyle` Account,
      text net.name
    ]
      <> [ timeCell time,
           text (typeWord (summaryType summary)),
           signedAmountCell unit (summaryAmount summary),
           statusCell,
           rateCell,
           purposeCell net label summary
         ]
  where
    net = placeNetwork place
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
-- by, in full and by its short id, its time to the second with its time
-- zone, and what its first line does not explain
statusFields :: Maybe ZonedTime -> TxStatus -> [Doc Style]
statusFields time (OnChain status) =
  hashFields Nothing "Txid" (chainTxid status)
    <> [field 9 "Time" <> text (secondText t) | Just t <- [time]]
    <> [field 9 "Status" <> "replaced, or dropped from the network" | chainDead status]
-- A transaction in preparation shows one hash, the one to name it by: its
-- nosigHash until it is signed, then its id, which the network will know.
statusFields time (Pending status) =
  hashFields (Just PendingHash) (if pendingSigned status then "Txid" else "NoSigHash") (pendingShownHash status)
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

-- | A hash under its name, in full, then its short id on a field of its
-- own, as the verbose views give both: the full hash for a block explorer
-- or another wallet, the short id for a command. Both are in the style
-- given, if any. On one line, the two would reach 86 columns.
hashFields :: Maybe Style -> Text -> TxHash -> [Doc Style]
hashFields styleM name hash =
  [ field 9 name <> shown (cs (txHashToHex hash)),
    field 9 "Short id" <> shown (shortHashText hash)
  ]
  where
    shown = maybe text styled styleM

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
    -- | The path and the label of an address of the wallet
    txAddressOwn :: !(Maybe (PoolPath, Text)),
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
      pathColumn = widest [pathToStr (pathInAccount p) | Just (p, _) <- txAddressOwn <$> rows]
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
        <> [(pathColumn widths, pathToStr soft, trailingPathWord soft) | Just (path, _) <- [txAddressOwn row], let soft = pathInAccount path]
    notes = case txAddressOwn row of
      Just (path, label) ->
        ownAddressNoteBelow label (marked && isIntPath (pathInAccount path))
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
    text (counted (fromIntegral (length left)) "coin stays" "coins stay" <> " in the portfolio:")
      : [questionCoinDoc unit coin.leftCoinOutpoint (fromIntegral coin.leftCoinValue) coin.leftCoinTxLabel (text (leftReasonText coin.leftCoinReason)) | coin <- left]

{- The questions of preparesendall and bumpfee -}

-- The reports the commands show on stderr before they ask. A sentence
-- wraps at the width of the screen, as the help does. A coin takes two
-- lines, as in a listing: its value with what is said of it, then the
-- short id of the coin, as --coin takes it. The amounts are in the unit
-- asked for.

-- | A sentence of words and amounts, wrapped at the width of the screen
sentence :: [Doc Style] -> Doc Style
sentence = fillSep

-- | The words of a text, for a sentence
wordsOf :: String -> [Doc Style]
wordsOf = map pretty . words

-- | A coin in a question: its value, then what is said of it, then the
-- label it shows if any, then the short id of the coin, as --coin and
-- lockcoin take it
questionCoinDoc :: AmountUnit -> OutPoint -> Natural -> Text -> Doc Style -> Doc Style
questionCoinDoc unit outpoint value label said =
  vcat $
    [columns [amountCell (Just Incoming) "" unit (fromIntegral value), said]]
      <> [indent 2 (labelWord label) | not (Text.null label)]
      <> [indent 2 (styled Muted (shortCoinText outpoint))]

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
    -- The fee of an input depends on the type of its coin. The header
    -- gives it when the dust coins all cost the same, and each coin
    -- gives its own otherwise.
    fees = nub (inputFee report.rate <$> report.dustCoins)
    header = case fees of
      [fee]
        | one -> sentence $ wordsOf "1 coin costs more to spend than it is worth at" <> [rateDoc report.rate <> ",", amount fee <> ":"]
        | otherwise -> sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ",", amount fee] <> wordsOf "each:"
      _ -> sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ":"]
    dustCoinDoc coin = questionCoinDoc unit coin.outpoint (fromIntegral coin.value) (Map.findWithDefault "" coin.outpoint.hash labels) (worth coin)
    worth coin = hsep (ownFee coin <> [worthAt coin])
    ownFee coin = ["costs" <+> amount (inputFee report.rate coin) <> "," | length fees > 1]
    worthAt coin = case worthSpendingUpTo coin of
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
          <> wordsOf ("less" <> if report.othersStay then "." else ", and the portfolio has no coin left on the network.")
    left =
      sentence $
        wordsOf ("Leaving " <> them <> ", the portfolio keeps")
          <> [amount dustValue <> "."]
          <> wordsOf "Once the other coins are gone,"
          <> later
    later = case report.laterRate of
      Just upTo
        | one -> wordsOf "it can only be spent at" <> [rateDoc upTo] <> wordsOf "or less."
        | otherwise -> wordsOf "they can only be spent together, at" <> [rateDoc upTo] <> wordsOf "or less."
      Nothing -> wordsOf ((if one then "it" else "they") <> " can never be spent: any fee would leave less than") <> [amount report.dustLimit <> "."]

-- | What bumpfee says of the transactions that spend the change of the one
-- to replace, before it asks to replace them all: their short ids, as txs
-- shows them
childrenDoc :: [Sent] -> Doc Style
childrenDoc children =
  vcat $
    sentence (wordsOf "Replacing the transaction removes those of the portfolio that spend its change:")
      : [indent 2 (styled Muted (shortHashText (sentId child))) | child <- children]

-- | What bumpfee says of the coins to add, before it asks to add them: each
-- with the address that holds it and its path, and the label it shows
-- among those given
addedCoinsDoc :: Network -> AmountUnit -> Map.Map TxHash Text -> Natural -> [(Store.Unspent, Address, PoolPath)] -> Doc Style
addedCoinsDoc net unit labels feeByte added =
  vcat . intersperse mempty $
    header : [questionCoinDoc unit coin.outpoint (fromIntegral coin.value) (Map.findWithDefault "" coin.outpoint.hash labels) (held addr path) | (coin, addr, path) <- added]
  where
    them = if length added == 1 then "this coin" else "these coins"
    header = sentence $ wordsOf "The coins of the transaction cannot pay its fee at" <> [rateDoc feeByte <> ","] <> wordsOf ("and " <> them <> " of the portfolio would:")
    held addr path = "at" <+> fullAddressWords net addr (Just (pathInAccount path))

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
          <> wordsOf "and no coin of the portfolio is added."
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

-- | Print the answer of a command, as 'prettyPrinter' does, without the
-- exit status of a failure
responsePrinter :: AmountUnit -> View -> AddressCode -> TxShown -> Response -> IO ()
responsePrinter unit view code shown answer =
  case answer of
    -- A transaction just signed, shown in full before the secret was asked
    ResponseTx _ info | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    ResponseOutsideTx _ info | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    -- The listings, which the screen reads in chunks ('sectionsPrinter')
    ResponsePortfolios {} -> listing
    ResponseAddresses {} -> listing
    ResponseBalances {} -> listing
    ResponseTxs {} -> listing
    ResponsePendingTxs {} -> listing
    ResponseCoins {} -> listing
    ResponseError err -> renderIO $ errorDoc err
    ResponseNewPortfolio secretM portfolio active ->
      renderIO . vcat . intersperse mempty $
        [secretDoc secret | Just secret <- [secretM]]
          <> [newPortfolioDoc portfolio active]
    ResponsePortfolio portfolio -> renderIO =<< listedPortfolioDoc Compact unit portfolio
    -- The outcome first, then the portfolio as portfolios shows it
    ResponseSwitched portfolio -> do
      portfolioDoc <- listedPortfolioDoc Compact unit portfolio
      renderIO $ vcat [text ("The active portfolio is now " <> refText (viewRef portfolio) <> "."), portfolioDoc]
    -- In green when the mnemonic matches, in red otherwise
    ResponseChecked _ matches said -> renderIO $ styled (if matches then Good else Failure) (cs said)
    ResponseDeleted said -> renderIO $ styled Good (cs said)
    ResponseDefaults defaults file ->
      renderIO . vcat $
        [field pad key <> pretty value | (key, value) <- defaults]
          <> ["Written to" <+> styled UserText file]
      where
        pad = fromIntegral (maximum (0 : (Text.length . fst <$> defaults)))
    ResponseFile f -> renderIO $ "Written to" <+> styled UserText f
    ResponseImport report active -> renderIO $ importDoc report active
    ResponseWalletFile file -> renderIO $ walletFileDoc file
    ResponseAddress ref addr spendable ->
      renderIO =<< addressAnswerDoc code unit ref addr spendable
    ResponseDerivedAddr ref path addr -> renderIO $ derivedAddrDoc ref path addr
    ResponseTx place txInfo ->
      renderIO =<< txAnswerDoc (placeNetwork place) (Just (placePortfolio place)) unit txInfo
    -- Each portfolio's view of the transaction, a blank line between them
    ResponseLabelledTxs held -> renderIO =<< heldTxsDoc unit held
    ResponseHeldTxs held -> renderIO =<< heldTxsDoc unit held
    -- The coins left in the portfolio below the transaction, apart from it
    ResponsePrepared place txInfo left -> do
      renderIO =<< txAnswerDoc (placeNetwork place) (Just (placePortfolio place)) unit txInfo
      unless (null left) $ putStrLn "" >> renderIO (leftCoinsDoc unit left)
    ResponseOutsideTx net txInfo -> renderIO =<< txAnswerDoc net Nothing unit txInfo
    ResponseSentTx net h ->
      renderIO $
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
        [syncResultsPrinter unit results | not (null results)]
          <> [notServedDoc notServed | not (null notServed)]
          <> [failedNetworksDoc "not synced" notSynced | not (null notSynced)]
    ResponseRecover searched gap walletM created results elsewhere notServed notSearched activeM ->
      renderIO . vcat . intersperse mempty $
        [vcat (recoveredDoc searched gap walletM created : [activeDoc active | Just active <- [activeM], not (null created)])]
          <> [syncResultsPrinter unit results | not (null results)]
          <> [foreignFundsDoc unit elsewhere | not (null elsewhere)]
          <> [notServedDoc notServed | not (null notServed)]
          <> [failedNetworksDoc "left out" notSearched | not (null notSearched)]
    ResponseFees net rates -> do
      now <- getCurrentTime
      renderIO $ feeRatesDoc net now rates
  where
    -- An answer of several groups says where each is
    listing = mapM_ (renderIO . listingDoc) =<< sectionsPrinter unit view (severalGroups answer) answer

-- | The secret newportfolio made, which it shows once
secretDoc :: NewSecret -> Doc Style
secretDoc (NewMnemonic source ws) = mnemonicDoc source ws
secretDoc (NewShares source threshold shares) = sharesDoc source threshold shares

-- | The portfolio newportfolio created: its name and its wallet, the
-- fingerprint of the wallet, which names the mnemonic from then on, and
-- the accounts it has, then which portfolio is active ('activeDoc'). A new
-- portfolio has every account, so the line only counts them and names
-- their networks.
newPortfolioDoc :: PortfolioView -> ActiveAfter -> Doc Style
newPortfolioDoc portfolio active =
  vcat $
    (styled Good "Created" <+> (pretty (portfolioName (refPortfolio ref)) `withStyle` Account) <+> "of" <+> pretty (walletName (refWallet ref)))
      : ( indent 2
            <$> [ field 11 "Fingerprint" <> pretty (fingerprintToText (viewFingerprint portfolio)),
                  field 11 "Accounts" <> text (show accounts <> ", on " <> andList networks)
                ]
        )
      <> [activeDoc active]
  where
    ref = viewRef portfolio
    accounts = sum (length . networkViewAccounts <$> viewNetworks portfolio)
    networks = (.name) . networkViewNetwork <$> viewNetworks portfolio

-- | A portfolio as a sentence names it, with its wallet always:
-- "portfolio1 of wallet0"
refText :: PortfolioRef -> String
refText ref = cs (portfolioName (refPortfolio ref)) <> " of " <> cs (walletName (refWallet ref))

-- | What a command that created portfolios says of the active one, the
-- one the commands work in: that it made the first portfolio of the
-- database the active one, or which one is still active. switch, which a
-- next step names, makes a new one active ('switchStep').
activeDoc :: ActiveAfter -> Doc Style
activeDoc active
  | active.activeAfterActivated = text (refText ref <> " is the active portfolio: the commands work in it.")
  | otherwise = text ("The active portfolio is still " <> refText ref <> ".")
  where
    ref = active.activeAfterRef

-- | What recover found, above the syncs of the portfolios: the portfolios
-- it created, or that the search found none
recoveredDoc :: [Network] -> Natural -> Maybe Natural -> [Natural] -> Doc Style
recoveredDoc searched gap walletM created =
  case walletM of
    Nothing -> text ("No portfolio of the mnemonic was paid on " <> orList ((.name) <$> searched) <> ". " <> ended)
    Just wallet -> case created of
      [] -> text ("No new portfolio of the mnemonic was paid on " <> orList ((.name) <$> searched) <> ". " <> ended)
      _ -> styled Good "Recovered" <+> (text (andList (cs . portfolioName <$> created)) `withStyle` Account) <+> "of" <+> pretty (walletName wallet)
  where
    ended = "The search ended after " <> counted gap "portfolio" "portfolios in a row" <> " without a payment."

-- | A transaction as each portfolio that holds it sees it, in full, a
-- blank line between the portfolios: what label and show answer
heldTxsDoc :: AmountUnit -> [(Place, TxInfo)] -> IO (Doc Style)
heldTxsDoc unit held =
  vcat . intersperse mempty <$> mapM (\(place, txInfo) -> txAnswerDoc (placeNetwork place) (Just (placePortfolio place)) unit txInfo) held

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
-- preparetx, preparesendall, preparesweep, signtx, signsweep, broadcast,
-- importwallet or sync, or after a command that created a portfolio that
-- is not the active one, below its answer. In the pretty output only: a
-- script reading the JSON answer knows its next step
-- ('nextStepsPrinter').
printNextSteps :: Command -> Response -> IO ()
printNextSteps cmd res =
  forM_ (nextStepsPrinter cmd res) $ \steps -> putStrLn "" >> renderIO steps

-- | A failure, as an answer or a listing reports it
errorDoc :: Text -> Doc Style
errorDoc err = styled Failure "Error:" <+> pretty err

{- Listings -}

-- | The rows of a section of a listing, as the screen shows them. A
-- listing over a portfolio has a section for each network, or for each
-- account for the addresses. Any other listing is one section.
data Rows = Rows
  { -- | What to say when the listing has no row at all
    rowsNone :: Doc Style,
    -- | The lines that say where the rows are, above the section: none
    -- in a listing of one place, which the command line already names
    rowsTitle :: [Doc Style],
    -- | The lines above the first row: the header of a table. Only the
    -- first chunk of a section shows them.
    rowsHeader :: [Doc Style],
    -- | A document per row
    rowsDocs :: [Doc Style],
    -- | Whether a blank line sets the rows apart
    rowsApart :: Bool
  }

-- | The rows of a listing whose elements take a few lines each: no
-- title, no header, and a blank line between two elements
apartRows :: Doc Style -> [Doc Style] -> Rows
apartRows none docs = Rows none [] [] docs True

-- | Whether an answer that lists comes in several groups, each of which
-- then says where it is
severalGroups :: Response -> Bool
severalGroups = \case
  ResponseAddresses groups -> length groups > 1
  ResponseBalances groups -> length groups > 1
  ResponseTxs groups -> length groups > 1
  ResponseCoins groups -> length groups > 1
  _ -> False

-- | The sections of an answer that lists, or Nothing for an answer that
-- does not: one for each group of the answer, or one alone for the
-- portfolios and the pending transactions, which come in no group. An
-- answer without a group gives one section without rows, which says what
-- to say then.
--
-- The rows of a section have a blank line between them. A table has a
-- header instead, and a row under the other. The screen reads a listing
-- in chunks, so the paths of its addresses are padded to the longest path
-- of the accounts of the section on the chains listed, the same for every
-- chunk, rather than to the longest one shown. The columns of a table
-- have fixed widths for the same reason. The portfolios, addresses,
-- transactions and coins show in the view given, their times in the time
-- zone of the computer.
--
-- With the flag, each section says where it is in a line above its rows:
-- its portfolio and its network, and its account for the addresses. The
-- callers set it for a listing that may cover several places. The
-- sections of balances always have that line, which gives the total of
-- the network: the specification of the listing asks for a total for
-- each network, and a listing narrowed to one network has one too.
sectionsPrinter :: AmountUnit -> View -> Bool -> Response -> IO (Maybe [Rows])
sectionsPrinter unit view titled = \case
  ResponsePortfolios portfolios -> do
    let none = text "There are no portfolios to display"
        -- The listing is read whole: these are all the portfolios
        headed = portfolioTableColumns unit portfolios
    if view == Table
      then return $ Just [Rows none [] (tableHeader headed) (concatMap (portfolioRowDocs unit headed) portfolios) False]
      else Just . pure . apartRows none <$> mapM (listedPortfolioDoc view unit) portfolios
  ResponseAddresses groups ->
    sections (text "There are no addresses to display") groups $ \none g -> do
      let net = placeNetwork (groupPlace g)
          addrs = groupRows g
          pad = longestPathWidth (groupAccounts g) (nub (dBAddressInternal . fst <$> addrs))
          -- addresses lists the addresses of one chain
          internal = any (dBAddressInternal . fst) addrs
          headed = addressTableColumns unit net (countsKind <$> groupAccounts g) pad internal
          title = [maybe (placeLine (groupPlace g)) accountRefLine (groupAccountRef g) | titled]
      if view == Table
        then return $ Rows none title (tableHeader headed) (uncurry (addressRowDoc unit headed) <$> addrs) False
        else do
          docs <- mapM (uncurry (listedAddressDoc view unit pad)) addrs
          return $ Rows none title [] docs True
  ResponseBalances groups ->
    sections (text "There are no addresses with coins to display") groups $ \none (g, total) -> do
      let net = placeNetwork (groupPlace g)
          addrs = groupRows g
          pad = longestPathWidth (groupAccounts g) [False, True]
          headed = balanceTableColumns unit net (countsKind <$> groupAccounts g) pad
          title = [balanceTitle unit (groupPlace g) total]
      return $
        if view == Table
          then Rows none title (tableHeader headed) (uncurry (balanceRowDoc unit headed) <$> addrs) False
          else Rows none title [] (uncurry (listedBalanceDoc view unit pad) <$> addrs) True
  ResponseTxs groups ->
    sections (text "There are no transactions to display") groups $ \none g -> do
      let net = placeNetwork (groupPlace g)
          title = [placeLine (groupPlace g) | titled]
      docs <- mapM (\(ListedTx info balanceM) -> listedTxDoc view net unit Nothing balanceM info) (groupRows g)
      return $
        if view == Table
          then Rows none title (txTableHeader unit) docs False
          else Rows none title [] docs True
  -- The listing covers every network of the active portfolio, or every
  -- portfolio in the tab of exportsheet, so each transaction says where
  -- it is
  ResponsePendingTxs held -> do
    let none = text "There are no pending transactions"
        -- The listing is read whole: these are all its places
        headed = pendingTableColumns unit (fst <$> held)
    if view == Table
      then do
        rows <- mapM (\(place, info) -> pendingRowDoc unit headed place <$> localTxTime info <*> pure info) held
        return $ Just [Rows none [] (tableHeader headed) rows False]
      else do
        docs <- mapM (\(place, info) -> listedTxDoc view (placeNetwork place) unit (Just (placePortfolio place)) Nothing info) held
        return $ Just [apartRows none docs]
  ResponseCoins groups ->
    sections (text "There are no coins to display") groups $ \none g -> do
      let net = placeNetwork (groupPlace g)
          coins = groupRows g
          headed = coinTableColumns unit net (countsKind <$> groupAccounts g) (longestPathWidth (groupAccounts g) [False, True])
          title = [placeLine (groupPlace g) | titled]
      if view == Table
        then do
          rows <- mapM (\coin -> coinRowDoc net unit headed <$> traverse utcToLocalZonedTime (jsonCoinTime coin) <*> pure coin) coins
          return $ Rows none title (tableHeader headed) rows False
        else do
          docs <- mapM (listedCoinDoc view net unit) coins
          return $ Rows none title [] docs True
  _ -> return Nothing
  where
    -- A section for each group, or one without rows for no group
    sections :: Doc Style -> [group] -> (Doc Style -> group -> IO Rows) -> IO (Maybe [Rows])
    sections none [] _ = return $ Just [apartRows none []]
    sections none groups rowsOf = Just <$> mapM (rowsOf none) groups

-- | The line above the addresses balances lists on a network: where they
-- are, then the value of every coin of the portfolio there, what it can
-- spend now, and the rest, which waits. The figures count every coin the
-- listing covers on the network, whatever part of them a page shows.
balanceTitle :: AmountUnit -> Place -> CoinBalance -> Doc Style
balanceTitle unit place total =
  columns
    [ placeLine place,
      commaList
        [ "total" <+> amountPrinter unit (coinBalanceTotal total),
          "spendable" <+> amountPrinter unit (coinBalanceSpendable total),
          "unspendable" <+> waitingAmount unit (coinBalanceUnspendable total)
        ]
    ]

-- | The width of the longest derivation path of the accounts on the
-- chains, False for the receiving one and True for the change one: that
-- of the newest address there
longestPathWidth :: [AccountCounts] -> [Bool] -> Natural
longestPathWidth accounts chains = fromIntegral $ maximum (0 : [width account internal | account <- accounts, internal <- chains])
  where
    width account internal =
      let count = if internal then countsInternal account else countsExternal account
       in if count == 0 then 0 else Text.length (pathText (chainPath internal (fromIntegral (count - 1))))

-- | A whole listing: its sections that have rows, a blank line between
-- them, or what to say when none has a row
listingDoc :: [Rows] -> Doc Style
listingDoc sections = case filter (not . null . rowsDocs) sections of
  [] -> maybe mempty rowsNone (listToMaybe sections)
  shown -> vcat (intersperse mempty (sectionDoc <$> shown))

-- | A section from its start: its title, set apart by a blank line when
-- its rows are, its header and its rows
sectionDoc :: Rows -> Doc Style
sectionDoc rows =
  vcat (rowsTitle rows <> [mempty | rowsApart rows, not (null (rowsTitle rows))] <> rowsHeader rows <> [rowsBody rows])

-- | The rows of a section alone, without its title and its header
rowsBody :: Rows -> Doc Style
rowsBody rows = vcat (if rowsApart rows then intersperse mempty (rowsDocs rows) else rowsDocs rows)

-- | A chunk of a listing, where the listing says it is ('ChunkAt'). The
-- first chunk of a listing without rows says so. A chunk that starts a
-- section has the title of the section, if any, and the header of its
-- table, after a blank line that sets it apart from the section before.
-- A chunk that goes on in its section comes under the rows before it,
-- after the blank line that sets them apart if one does. An answer that
-- is not a listing is a failure here.
chunkDoc :: AmountUnit -> View -> ChunkAt -> Response -> IO (Doc Style)
chunkDoc unit view at answer = do
  sectionsM <- sectionsPrinter unit view (chunkTitled at) answer
  return $ case sectionsM of
    Just sections
      | all (null . rowsDocs) sections -> listingDoc sections
      | chunkStartsSection at -> vcat ([mempty | not (chunkFirst at)] <> [listingDoc sections])
      | otherwise -> vcat [vcat ([mempty | rowsApart rows] <> [rowsBody rows]) | rows <- sections]
    Nothing -> case answer of
      ResponseError err -> errorDoc err
      _ -> errorDoc "The answer is not a listing"

-- | Whether the first lines of the first chunk of a listing are the
-- header of a table, which the pager can then keep on the screen: a
-- table whose sections have no title. A title comes above the header, and
-- the pager keeps the first lines it is given.
startsWithHeader :: View -> ChunkAt -> Response -> Bool
startsWithHeader view at answer = view == Table && not (chunkTitled at) && not balances
  where
    balances = case answer of
      ResponseBalances _ -> True
      _ -> False

-- | The steps the user may take next, after a command of a payment or a
-- sweep, if any. Each step is a command line, styled as the help shows
-- commands, options and metavars.
--
-- A step names no portfolio: the next command works in the active one, or
-- finds its portfolio by itself. newportfolio, recover and importwallet
-- leave the active portfolio as it was when the database had one, and
-- their step is then the switch to the first portfolio they created
-- ('switchStep'). A sync covers every portfolio, so the pending
-- transaction in conflict it found may be in another portfolio than the
-- active one, which pendingtxs lists alone: the steps then start with
-- the switch to that portfolio ('conflictSteps').
nextStepsPrinter :: Command -> Response -> Maybe (Doc Style)
nextStepsPrinter cmd res =
  case (cmd, res) of
    (CommandNewPortfolio _, ResponseNewPortfolio _ view active)
      | not active.activeAfterActivated -> stepsPrinter [switchStep (viewRef view)]
    (CommandRecover _, ResponseRecover _ _ (Just wallet) created@(_ : _) _ _ _ _ (Just active))
      | not active.activeAfterActivated -> stepsPrinter [switchStep (PortfolioRef wallet (minimum created))]
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
    (CommandSignTx signing, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseTx _ info) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseOutsideTx _ info) -> afterSigning signing info
    (CommandBroadcast {}, ResponseTx _ _) ->
      stepsPrinter [("Next, sync the wallet to follow the transaction", hwLine "sync" [])]
    -- The file may label things the wallet does not have yet, and it may
    -- bring portfolios that the active one leaves aside
    (CommandImportWallet _ file, ResponseImport report activeM)
      | not (null steps) -> stepsPrinter steps
      where
        steps =
          concat
            [ [ ("Next, sync the wallet, for the addresses, transactions and coins the file labels", hwLine "sync" []),
                ("Then import the file again, for the labels it could not place", hwLine "importwallet" [fileWord file])
              ]
              | reportNeedsSync report
            ]
            <> [ switchStep first
                 | Just active <- [activeM],
                   not active.activeAfterActivated,
                   first : _ <- [importCreated report]
               ]
    -- A sync that synced nothing has nothing new to see
    (CommandSync {}, ResponseSync results _ _ activeM)
      | not (null results) ->
          case nub [placePortfolio (syncResPlace result) | result <- results, syncResConflicted result > 0] of
            [] -> stepsPrinter (txsSteps activeM (nub [placePortfolio (syncResPlace result) | result <- results, syncResTxUpdates result > 0]))
            conflicts -> stepsPrinter (conflictSteps activeM conflicts)
    _ -> Nothing
  where
    -- The short id of a transaction in preparation, as its answer shows
    -- it, to type in the next command
    shownOf info = case txInfoStatus info of
      Pending status -> Just (shortHashText (pendingShownHash status))
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
          stepsPrinter [("Next, send it to the network", hwLine "broadcast" [pendingHashWord shown])]
      | Just file <- signing.output <|> signing.input =
          stepsPrinter [("Next, send its file from the online computer", hwLine "broadcast" [optionWord "--input", fileWord file])]
      | otherwise = Nothing
    -- Without a file, signsweep asks for the keys on the terminal
    keysArgs (Just (KeysInFile file)) = [optionWord "--key-file", fileWord file]
    keysArgs _ = []

-- | The step that makes a portfolio a command created the active one, to
-- work in it: the switch to it, which names its wallet always, since the
-- active portfolio may be of another wallet
switchStep :: PortfolioRef -> (String, Doc Style)
switchStep ref = ("To work in " <> refText ref <> ", make it the active one", switchLine ref)

-- | The command line that makes a portfolio the active one, which names
-- its wallet always
switchLine :: PortfolioRef -> Doc Style
switchLine ref = hwLine "switch" [optionWord "--wallet", text (show (refWallet ref)), optionWord "--portfolio", text (show (refPortfolio ref))]

-- | The steps after a sync that found pending transactions in conflict,
-- given the active portfolio and the portfolios that hold them, each
-- once. pendingtxs lists the active portfolio alone, and deletetx finds
-- the transaction by itself. So the steps are pendingtxs and deletetx
-- when the active portfolio holds every conflict. Otherwise they start
-- with the switch to the portfolio of the conflict, or to each of the
-- portfolios in turn when there are several, the active one among them
-- if it holds one: a pendingtxs typed without a switch would not show
-- the transaction.
conflictSteps :: Maybe PortfolioRef -> [PortfolioRef] -> [(String, Doc Style)]
conflictSteps activeM conflicts = case conflicts of
  [only]
    | Just only == activeM ->
        [ ("Next, find the pending transaction in conflict, which can no longer be sent", hwLine "pendingtxs" []),
          ("Then delete it, to free its coins", hwLine "deletetx" [metavarWord "HASH"])
        ]
    | otherwise ->
        [ ("Next, switch to " <> refText only <> ", where a pending transaction is in conflict", switchLine only),
          ("Then find the pending transaction in conflict, which can no longer be sent", hwLine "pendingtxs" []),
          ("Then delete it, to free its coins", hwLine "deletetx" [metavarWord "HASH"])
        ]
  several ->
    [ ("Next, switch in turn to each portfolio where a pending transaction is in conflict", vcat (switchLine <$> several)),
      ("In each, find the pending transactions in conflict, which can no longer be sent", hwLine "pendingtxs" []),
      ("Then delete them, to free their coins", hwLine "deletetx" [metavarWord "HASH"])
    ]

-- | The steps after a sync that found no conflict, given the active
-- portfolio and the portfolios whose transactions it updated, each once.
-- txs lists the active portfolio alone. So the step is txs when the sync
-- updated the active portfolio, or none. When it updated other portfolios
-- alone, the steps start with the switch to the one, or to each in turn:
-- a txs typed without a switch would show none of what the sync found.
txsSteps :: Maybe PortfolioRef -> [PortfolioRef] -> [(String, Doc Style)]
txsSteps activeM updated
  | null updated || maybe False (`elem` updated) activeM = [("Next, see the transactions", hwLine "txs" [])]
  | otherwise = case updated of
      [only] ->
        [ ("Next, switch to " <> refText only <> ", where the sync found new transactions", switchLine only),
          ("Then see the transactions", hwLine "txs" [])
        ]
      several ->
        [ ("Next, switch in turn to each portfolio where the sync found new transactions", vcat (switchLine <$> several)),
          ("In each, see the transactions", hwLine "txs" [])
        ]

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
-- it as more than one word or change it: a file name with a space, for
-- instance
shellWord :: String -> String
shellWord word
  | not (null word) && all plain word = word
  | otherwise = "'" <> concatMap quoted word <> "'"
  where
    plain c = isAlphaNum c || c `elem` ("-_./:@%+=," :: String)
    quoted '\'' = "'\\''"
    quoted c = [c]
