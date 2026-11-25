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
import Data.List (genericLength, intercalate, intersperse, nub, partition, sort, sortOn)
import Data.List.Split (chunksOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
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
import Haskoin.Wallet.Database.Address (AddressFunds (..), CoinBalance (..), addrPath, addrPoolPath, chainPath, coinBalanceNotSpendableYet, coinBalanceUnspendable, isIntPath, pathText)
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.PendingTx (Retag (..), intercalateAnd)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Backup (BackupsShown (..), CopiesShown (..))
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), PortfolioShown (..), WalletShown (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..), levelName, rateAt)
import Haskoin.Wallet.Tx.Mixing (SpentGroup (..))
import Haskoin.Wallet.Keys (AccountKind (..), KeySource (..), PoolPath (..), foreignKindDescription, kindName, poolPathText, portfolioAccounts, portfolioNetworks)
import Haskoin.Wallet.Bip329 (Record (..), RecordType (..), parseKeypath)
import Haskoin.Wallet.Labels (AccountImport (..), FileRecord (..), FileShown (..), FileTag (..), ImportReport (..), ImportScope (..), LabelImport (..), PortfolioImport (..), RecordFate (..), WalletImport (..), importCreated, isContactRecord, recordFileTags, reportNeedsSync)
import Haskoin.Wallet.MessageSigning (signedMessageText)
import Haskoin.Wallet.Cli.Listing (ChunkAt (..), KeptTags (..), keptTagsWords)
import Haskoin.Wallet.Cli.Parser (AddressCode (..), BumpFee (..), Command (..), PrepareSendAll (..), PrepareSweep (..), PrepareTx (..), Signing (..), TxShown (..), WordsLayout (..), program)
import Haskoin.Wallet.Cli.Parser.Help (commandName)
import Haskoin.Wallet.Cli.Parser.Run (commandNamed)
import Haskoin.Wallet.Cli.QrCode (paymentUri, qrRows)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.ShortId (shortCoinText, shortHashText)
import Haskoin.Wallet.Contacts (Contact (..), inFours)
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse (..), TagWidths (..))
import Haskoin.Wallet.Tags (ShownTag (..), Tag, colorName, tagText)
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Tx.BumpFee (PaidBy (..), Reduction (..), Sent, sentId)
import Haskoin.Wallet.Tx.CoinSelection (FeePayer (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.Dust (DustPlace (..), DustReport (..), SizedCoin (..), inputFee, valueOf, worthSpendingUpTo)
import Haskoin.Wallet.Tx.Sweep (UnsweptCoin (..), UnsweptReason (..))
import Haskoin.Wallet.Tx.Size (PaidRate, paidRateText)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, annotate, fillSep, flatAlt, group, hcat, hsep, indent, line, pretty, punctuate, vcat, (<+>))
import Text.DocLayout (realLength)
import System.Exit
import System.IO
import System.FilePath (takeFileName)

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

-- | The keys of the configuration file with their values, a key on each
-- line, padded to the longest one, as setdefault and showdefault answer.
-- The keys are the names of the file, so that the user finds them there.
keyRowsDoc :: [(Text, Text)] -> [Doc Style]
keyRowsDoc defaults = [field pad key <> pretty value | (key, value) <- defaults]
  where
    pad = fromIntegral (maximum (0 : (Text.length . fst <$> defaults)))

-- | What setdefault says of the journal hw keeps for ledger, once it
-- turned it on or off: where the file is, for the user to include it in
-- a journal of their own, or that hw removes it. A journal left behind
-- would go on being read, and would say less and less of the wallet.
journalSwitchDoc :: JournalSwitch -> Doc Style
journalSwitchDoc switched
  | journalKept switched = "hw now keeps a journal for ledger at" <+> styled UserText (journalPath switched)
  | otherwise = "hw no longer keeps a journal, and removes" <+> styled UserText (journalPath switched)

-- | What showdefault says of the active portfolio, in a sentence. A
-- sentence always names the wallet, as "portfolio1 of wallet0", whatever
-- the number of wallets: the user asked which one it is. Nothing when
-- neither --portfolio nor --wallet asked for it.
activeShownDoc :: ActiveShown -> Maybe (Doc Style)
activeShownDoc = \case
  ActiveNotAsked -> Nothing
  ActivePortfolioShown (Just ref) -> Just (text ("The active portfolio is " <> refText ref <> "."))
  ActiveWalletShown (Just index) -> Just (text ("The active wallet is " <> cs (walletName index) <> "."))
  ActivePortfolioShown Nothing -> Just noPortfolioDoc
  ActiveWalletShown Nothing -> Just noPortfolioDoc
  where
    -- The refusal of the commands that need a portfolio says how to make
    -- one, which the user who has none needs to know here too
    noPortfolioDoc = sentence (wordsOf (noWallets <> "."))

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
-- words in the layout given
mnemonicDoc :: WordsLayout -> Text -> [Text] -> Doc Style
mnemonicDoc layout source ws = vcat [heading, wordsDoc layout ws]
  where
    heading = text ("Mnemonic of " <> show (length ws) <> " words,") <+> styled Muted ("from " <> cs source)

-- | The shares of a new backup: how many they are, of how many words, how
-- many of them rebuild the seed and where their entropy came from, then
-- each share under its number, a blank line between them, its words in
-- the layout given
sharesDoc :: WordsLayout -> Text -> Natural -> [[Text]] -> Doc Style
sharesDoc layout source threshold shares =
  vcat . intersperse mempty $ heading : [vcat [text ("Share " <> show i), wordsDoc layout share] | (i, share) <- zip [1 :: Natural ..] shares]
  where
    heading = text ("Backup in " <> counted (fromIntegral (length shares)) "share" "shares" <> " of " <> show (maybe 0 length (listToMaybe shares)) <> " words, " <> needed <> ",") <+> styled Muted ("from " <> cs source)
    needed
      | length shares == 1 = "which is the whole backup"
      | otherwise = "any " <> show threshold <> " of them rebuild the seed"

-- | The words of a mnemonic or of a share in the layout given
-- ('WordsLayout').
--
-- In numbered columns, four words are on a line, each after its number. A
-- word of the BIP39 list has at most 8 letters, and so has one of the
-- SLIP-39 list.
--
-- On one line, the words have a space between them and nothing else, not
-- even the indent of the columns: a line selected whole on the terminal
-- holds the words alone. The line is one text, which the layout never
-- breaks at 80 columns. A break would make two lines to copy. 24 words are
-- wider than most terminals, which wrap the line on the screen only, and
-- copy it as one.
wordsDoc :: WordsLayout -> [Text] -> Doc Style
wordsDoc OneLine ws = styled Secret (cs (Text.unwords ws))
wordsDoc NumberedColumns ws = indent 2 . vcat $ cellsLine . map cell <$> chunksOf 4 (zip [1 :: Natural ..] ws)
  where
    cell (i, w) =
      let number = rblock 2 (show i) <> "."
       in (12, number <> " " <> cs w, styled Field number <+> styled Secret (cs w))

{- Where an answer is -}

-- | A portfolio as a view names it: "portfolio0" in a database of one
-- wallet, and with its wallet in a database of several, wallet0 too, as
-- in "portfolio0 of wallet0" ('WalletNaming'). Every line of a view names
-- a portfolio this way: the field of an answer, the title of a listing,
-- and a portfolio after a path. So a portfolio reads the same wherever
-- the user meets it, and a portfolio named alone is always of the one
-- wallet of the database. The listing of the portfolios names the wallet
-- of each on its own, and a sentence names both always ('refText').
portfolioWords :: WalletNaming -> PortfolioRef -> String
portfolioWords naming ref = cs (portfolioRefName naming ref)

-- | The line that says where the rows under it are: the portfolio, named
-- as the views name one, then the network
placeLine :: WalletNaming -> Place -> Doc Style
placeLine naming place =
  columns
    [ text (portfolioWords naming (placePortfolio place)) `withStyle` Account,
      text (placeNetwork place).name
    ]

-- | The line that names an account: its portfolio, named as the views
-- name one, its network and the type of its addresses, as --network and
-- --type pick it
accountRefLine :: WalletNaming -> AccountRef -> Doc Style
accountRefLine naming ref =
  columns
    [ text (portfolioWords naming (placePortfolio (accountRefPlace ref))) `withStyle` Account,
      text (accountRefNetwork ref).name,
      text (cs (kindName (accountRefKind ref)))
    ]

-- | The portfolio an answer about one thing shows it from, and the active
-- portfolio, if the database has one. A command given an address or a
-- transaction finds its portfolio by itself, whichever is active: a
-- transfer between two portfolios is of the one that pays it, which
-- signs it. So the answer may show the paths and the labels of another
-- portfolio than the active one, and it then says so
-- ('seenPortfolioDoc'). The alternative, showing a transfer as the
-- active portfolio sees it, was turned down: the view before signtx would
-- then show a payment received while the mnemonic signs one sent.
data SeenFrom = SeenFrom
  { seenPortfolio :: !PortfolioRef,
    seenActive :: !(Maybe PortfolioRef)
  }
  deriving (Eq, Show)

-- | The portfolio an answer is seen from, as its field names it:
-- "portfolio0", or "portfolio0, not the active portfolio1" when another
-- portfolio is active. Both are named as the views name a portfolio
-- ('portfolioWords'), so with several wallets the field reads "portfolio1
-- of wallet0, not the active portfolio0 of wallet1". The note is yellow.
-- In the view of a transaction it is in the first field, so the user
-- reads it before the paths. An answer that does not know the active
-- portfolio, as one read back from JSON, names none.
seenPortfolioDoc :: WalletNaming -> SeenFrom -> Doc Style
seenPortfolioDoc naming seen =
  (text (portfolioWords naming seen.seenPortfolio) `withStyle` Account) <> note
  where
    note = case seen.seenActive of
      Just active
        | seenFromAnother seen ->
            ", " <> styled OtherPortfolio ("not the active " <> portfolioWords naming active)
      _ -> mempty

-- | Whether an answer is seen from another portfolio than the active one.
-- An answer that does not know the active portfolio is not.
seenFromAnother :: SeenFrom -> Bool
seenFromAnother seen = maybe False (/= seen.seenPortfolio) seen.seenActive

{- The paths of another portfolio -}

-- | How a view writes the path of an address of the wallet. A path is
-- written alone, as in bip84\/0\/3, or with the portfolio that holds the
-- address after it, as in bip84\/0\/3 of portfolio1. That portfolio is
-- named as every line of the view names one ('portfolioWords'): with its
-- wallet in a database of several wallets, as in bip84\/0\/3 of
-- portfolio1 of wallet0.
--
-- A path alone is of the active portfolio, in a view of the active
-- portfolio. The commands read a path the same way. show, label and
-- --spend-from take a path in the active portfolio. So a path alone on the
-- screen is one the user can type back.
--
-- Every other path names its portfolio. Nobody then takes it for a path
-- to type, or for an address of the portfolio they work in. There are two
-- cases.
--
-- * A view of the active portfolio names the portfolio of an address of
--   another portfolio. A transaction pays such an address, or spends from
--   it (the type t'HeldElsewhere').
--
-- * A view of another portfolio than the active one names the portfolio
--   after every path. That covers the paths of its own portfolio, and
--   those of any other portfolio, the active one too. Such a view shows
--   paths of two portfolios. A path alone could be read as of either.
--
-- A view may not know the active portfolio, as an answer read back from
-- JSON. It writes the paths of its own portfolio alone.
--
-- The alternative was to keep the paths of the portfolio of the view
-- alone, whichever portfolio is active, as hw wrote them before. The
-- first line of such a view says its portfolio is not the active one
-- ('seenPortfolioDoc'). But a path is read and copied several lines
-- below that line.
data PathNaming = PathNaming
  { -- | The portfolio of the view, when its own paths name it: that of a
    -- view of another portfolio than the active one
    namingOwn :: !(Maybe PortfolioRef),
    -- | Whether the view names the wallet of a portfolio after a path,
    -- as its other lines do
    namingWallets :: !WalletNaming
  }

-- | How a view of the active portfolio writes its paths: its own alone.
-- The listings of one portfolio, addresses, balances, coins and txs, are
-- of the active one, and so are the reports of the commands that work in
-- it. A transaction of txs may pay an address of another portfolio, whose
-- path names it, with its wallet or not as given.
activePaths :: WalletNaming -> PathNaming
activePaths = PathNaming Nothing

-- | How a view that shows addresses of the active portfolio alone writes
-- their paths: each alone. addresses, balances and coins list such
-- addresses, and the report on the dust of a send-all shows such coins.
-- No path there is of another portfolio, so these views name no
-- portfolio and need not know how the wallets are named. A view that may
-- show an address of another portfolio, as that of a transaction, takes
-- 'activePaths' instead.
ownPathsAlone :: PathNaming
ownPathsAlone = activePaths WithoutWallets

-- | How an answer about one thing writes its paths, given how the views
-- name the wallets, the portfolio it is seen from and the active one: its
-- own alone, unless it is seen from another portfolio than the active one
pathsSeenFrom :: WalletNaming -> SeenFrom -> PathNaming
pathsSeenFrom naming seen = PathNaming (if seenFromAnother seen then Just seen.seenPortfolio else Nothing) naming

-- | How a transaction of a listing of every portfolio writes its paths,
-- given how the views name the wallets, the active portfolio, and the
-- portfolio of the transaction
listedPaths :: WalletNaming -> Maybe PortfolioRef -> PortfolioRef -> PathNaming
listedPaths naming activeM ref = pathsSeenFrom naming (SeenFrom ref activeM)

-- | A portfolio after a path: " of portfolio1", or " of portfolio1 of
-- wallet0" where the views name the wallets
ofPortfolio :: PathNaming -> PortfolioRef -> String
ofPortfolio paths ref = " of " <> portfolioWords paths.namingWallets ref

-- | What follows a path of the portfolio of the view: its portfolio when
-- its paths name it, or nothing
ownSuffix :: PathNaming -> String
ownSuffix paths = maybe "" (ofPortfolio paths) paths.namingOwn

-- | The path of an address of the portfolio of the view, as the view
-- writes it: with the type of its account in front, as in bip84\/0\/3
-- ('poolPathText'), and its portfolio after it when the paths of the view
-- name it
ownPathText :: PathNaming -> PoolPath -> String
ownPathText paths path = cs (poolPathText path) <> ownSuffix paths

{- Portfolios -}

-- | A portfolio as a listing shows it, in the view asked for, the time it
-- was created in the time zone of the computer
listedPortfolioDoc :: View -> AmountUnit -> PortfolioView -> IO (Doc Style)
listedPortfolioDoc view unit portfolio = do
  created <- utcToLocalZonedTime (viewCreated portfolio)
  return $ case view of
    Verbose -> verbosePortfolioDoc unit created portfolio
    -- The table is laid out by sectionsPrinter ('portfolioRowDocs')
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
activeMark = activeMarkDoc . viewActive

-- | The mark of a portfolio, given whether it is the active one
activeMarkDoc :: Bool -> Doc Style
activeMarkDoc active
  | active = styled Good "*"
  | otherwise = " "

-- | The networks of a portfolio that a short view shows: those it has an
-- address or a coin on. A whole portfolio has an account on every network,
-- most of which a user never touches.
activeNetworks :: PortfolioView -> [NetworkView]
activeNetworks portfolio = filter networkInUse (viewNetworks portfolio)

-- | What a portfolio holds on a network, in one line: what its coins
-- are worth and what it can spend now ('shortBalanceFigures'), and how
-- many coins and addresses it has.
--
-- On a network where it never synced, the line says so in place of the
-- amounts and the coins ('notSyncedWords'), then gives the addresses:
-- "total 0.00000000 btc, no coins" would say that the portfolio holds
-- nothing there, and hw asked no server.
networkFigures :: AmountUnit -> NetworkView -> Doc Style
networkFigures unit net
  | networkViewSynced net = commaList (shortBalanceFigures unit (networkViewTotal net) (networkViewSpendable net) <> [coins, addresses])
  | otherwise = commaList [notSyncedWords, addresses]
  where
    coins = text (counted (fromIntegral (networkViewCoins net)) "coin" "coins")
    addresses = text (counted (networkViewAddresses net) "address" "addresses")

-- | What coins are worth, in the three amounts of every short view:
-- all of them, those that can be spent now, and the rest, which waits or
-- is locked. The first is the sum of the two others, so no reader can
-- add them wrongly or take a part for the whole. balances --verbose says
-- why the rest cannot be spent.
--
-- The amounts follow what can be spent, and not what is confirmed: an
-- unconfirmed amount would be a part of the total that overlaps both
-- others, since the wallet can spend the unconfirmed change of its own
-- payments.
balanceFigures :: AmountUnit -> Word64 -> Word64 -> [Doc Style]
balanceFigures unit total spendable =
  [ totalFigure unit total,
    "spendable" <+> amountPrinter unit spendable,
    unspendableFigure unit (total - spendable)
  ]

-- | The first of the 'balanceFigures': what coins are worth
totalFigure :: AmountUnit -> Word64 -> Doc Style
totalFigure unit total = "total" <+> amountPrinter unit total

-- | The last of the 'balanceFigures': what cannot be spent now, in the
-- colour of what waits
unspendableFigure :: AmountUnit -> Word64 -> Doc Style
unspendableFigure unit rest = "unspendable" <+> waitingAmount unit rest

-- | 'balanceFigures' on a line that has more to say after them, as that
-- of a portfolio and that of a sync: when every coin can be spent, the
-- rest is left out. The line always starts with the total and what can
-- be spent of it.
shortBalanceFigures :: AmountUnit -> Word64 -> Word64 -> [Doc Style]
shortBalanceFigures unit total spendable
  | total == spendable = take 2 figures
  | otherwise = figures
  where
    figures = balanceFigures unit total spendable

-- | A portfolio in a few lines: its first line, with the mark of the
-- active portfolio, the description of its wallet when it has one, the
-- date it was created, then a line for each network it has addresses or
-- coins on, with what it holds there.
--
-- A portfolio without such a network has one line in their place. It has
-- no addresses yet, or it has some on hidden networks alone
-- ('viewUsedOnHidden'): it then has nothing on the shown networks, which
-- is true and names no hidden network.
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
      | null active && viewUsedOnHidden portfolio = [styled Muted "nothing on the shown networks"]
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
              <$> ( holdings net
                      <> [field 11 "Addresses" <> countPrinter (networkViewAddresses net)]
                      <> concatMap accountBlock (networkViewAccounts net)
                  )
          )
    -- One row stands for the amounts and the coins of a network that
    -- never synced, as in the verbose view of an address
    holdings net
      | networkViewSynced net =
          [ field 11 "Total" <> amountPrinter unit (networkViewTotal net),
            field 11 "Spendable" <> amountPrinter unit (networkViewSpendable net),
            field 11 "Unspendable" <> waitingAmount unit (networkViewUnspendable net),
            field 11 "Confirmed" <> amountPrinter unit (networkViewConfirmed net),
            field 11 "Unconfirmed" <> waitingAmount unit (networkViewUnconfirmed net),
            field 11 "Coins" <> countPrinter (fromIntegral (networkViewCoins net))
          ]
      | otherwise = [field 11 "Balance" <> notSyncedWords]
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
    || not (null (syncResNotPublished result))
    || syncResConflicted result > 0
    || syncResNewAddrs result > 0

-- | The syncs worth showing, each with its results, a blank line between
-- them, then one line for each portfolio that names its networks where
-- nothing is held and nothing changed. A sync goes over the portfolios of
-- every wallet, and each is named as the views name one
-- ('portfolioWords').
syncResultsPrinter :: WalletNaming -> AmountUnit -> [SyncRes] -> Doc Style
syncResultsPrinter naming unit results =
  vcat . intersperse mempty $ (syncResultDoc naming unit <$> shown) <> [vcat quietLines | not (null quietLines)]
  where
    shown = filter syncWorthShowing results
    quiet = filter (not . syncWorthShowing) results
    quietLines =
      [ (text (portfolioWords naming ref) `withStyle` Account) <> "  " <> styled Muted ("nothing on " <> andList [(placeNetwork (syncResPlace r)).name | r <- quiet, placePortfolio (syncResPlace r) == ref])
        | ref <- nub (placePortfolio . syncResPlace <$> quiet)
      ]

-- | Names in a sentence, "and" before the last: "btc", "btc and bch",
-- "btc, bch and btctest4"
andList :: [String] -> String
andList names = case reverse names of
  [] -> ""
  [name] -> name
  lastName : before -> intercalate ", " (reverse before) <> " and " <> lastName

-- | Names as 'andList' writes them, as the words of a sentence that wraps
-- ('sentence'). Each name is one word, so a line breaks between two names
-- and never inside one, even a name with spaces such as
-- "btc (bip44 and bip84)". The "and" goes with the last name, so that a
-- line does not end with it.
andListWords :: [String] -> [Doc Style]
andListWords names = case reverse names of
  [] -> []
  [name] -> [text name]
  lastName : nextToLast : earlier -> (text . (<> ",") <$> reverse earlier) <> [text nextToLast, text ("and " <> lastName)]

-- | A portfolio synced on a network: where it is, what it holds there,
-- the block it is synced to, and what changed, one kind of change a
-- line. What did not change is left out, and a sync that changed nothing
-- says so.
syncResultDoc :: WalletNaming -> AmountUnit -> SyncRes -> Doc Style
syncResultDoc naming unit result =
  vcat $ placeLine naming (syncResPlace result) : (indent 2 <$> syncHoldsDoc unit result : synced : changes)
  where
    synced
      | null changes = "synced at block" <+> height <> ", nothing new"
      | otherwise = "synced at block" <+> height
    height = text (show (syncResBlockHeight result))
    changes = updated <> syncNewsDocs unit result
    updated = case (syncResTxUpdates result, syncResCoinUpdates result) of
      (0, 0) -> []
      (t, 0) -> [text (counted t "transaction" "transactions") <+> "updated"]
      (0, c) -> [text (counted c "coin" "coins") <+> "updated"]
      (t, c) -> [text (counted t "transaction" "transactions" <> " and " <> counted c "coin" "coins") <+> "updated"]

-- | A portfolio as the sync that follows a sending leaves it
-- ('syncAfterDocs'): where it is and what it holds, then what the sync
-- has to tell ('syncNewsDocs'). The block of a sync
-- ('syncResultDoc') has two more lines, the block reached and the count
-- of what was updated. They say that a sync was done, which the user of
-- send did not ask about, and the payment itself always updates
-- something.
--
-- The news stays. A sync tells each of them once: a lock that moved to
-- the coin of a replacement just sent, or a coin with tokens that came
-- since the last sync, would otherwise never be told.
syncedAfterDoc :: WalletNaming -> AmountUnit -> SyncRes -> Doc Style
syncedAfterDoc naming unit result =
  vcat $ placeLine naming (syncResPlace result) : (indent 2 <$> syncHoldsDoc unit result : syncNewsDocs unit result)

-- | What a portfolio holds on a network once synced: what its coins are
-- worth and what it can spend ('shortBalanceFigures'), and how many
-- coins
syncHoldsDoc :: AmountUnit -> SyncRes -> Doc Style
syncHoldsDoc unit result = commaList (shortBalanceFigures unit (syncResTotal result) (syncResSpendable result) <> [coins])
  where
    coins = text (counted (fromIntegral (syncResCoins result)) "coin" "coins")

-- | What a sync has to tell beyond its counts, a line or two for each:
-- the locks that moved, the coins with tokens that came, the
-- transactions sent again and those that could not be, the pending
-- transactions found in conflict, and the addresses found
syncNewsDocs :: AmountUnit -> SyncRes -> [Doc Style]
syncNewsDocs unit result = locksMoved <> withTokens <> sentAgain <> notPublished <> conflicted <> found
  where
    -- A coin the user locked is gone, its payment replaced, and the coin
    -- of the replacement is locked: both short ids, since unlockcoin now
    -- takes the second
    locksMoved =
      [ text ("lock moved from " <> shortCoinText old <> " to " <> shortCoinText heir <> ", the coin that replaced it")
        | (old, heir) <- syncResLocksMoved result
      ]
    -- A coin that holds tokens of Bitcoin Cash arrived. hw locks it for
    -- good, so the lines say how many came, what they hold, and what
    -- moves them: no command of hw does.
    withTokens = case syncResTokenCoins result of
      [] -> []
      values ->
        [ text (counted (fromIntegral (length values)) "new coin holds" "new coins hold" <> " tokens (CashTokens):")
            <+> amountPrinter unit (sum values)
            <> ", which hw never spends",
          indent 2 . text $ "a wallet that handles tokens moves " <> (if length values == 1 then "it" else "them") <> ", with the same mnemonic"
        ]
    sentAgain = [text (counted r "transaction" "transactions") <+> "sent again" | let r = syncResRebroadcast result, r > 0]
    -- A transaction the store did not publish stays pending: its short
    -- id, to name it in deletetx, then what the store said, whole
    notPublished =
      [ styled Failure (shortHashText txid <> " not sent again") <> ":" <+> text why
        | (txid, why) <- syncResNotPublished result
      ]
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
--
-- A failure is wrapped as an error is ('wrappedAfter'), after the network
-- and the word: its sentences break between words, and a command to copy
-- that it gives on a line of its own stays whole.
failedNetworksDoc :: String -> [(Network, Text)] -> Doc Style
failedNetworksDoc notDone nets =
  vcat [wrappedAfter (text net.name <> "  " <> styled Failure (notDone <> ":")) (net.name <> "  " <> notDone <> ":") failure | (net, failure) <- nets]

-- | The funds of a seed at accounts hw does not hold, as recover shows
-- them: a line for each account, with its network, its path, its kind, how
-- many of its addresses were used and what they hold, then what to do with
-- the funds. The network comes first, as on the line of an account. The
-- path alone does not tell it, since m\/44'\/0' is searched on Bitcoin Cash
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
-- blank line between them. Then come the counts of the import
-- ('importCountsDoc').
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
-- file with the same lines ('walletFileParts'): the user reads before
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
-- file brings it ('fileDescriptionDocs'). The answer of importwallet
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
-- description ('shownWalletDoc'): the label of the file, between quotes.
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
-- counts under the records ('importCountsDoc'), which say how many such
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

{- Files show tells by their kind -}

-- | The first lines of a file show shows by what it holds: the kind of
-- the file in bold, with the file as the user named it, then a line that
-- says what the file is and what follows. show takes two files that both
-- hold wallets, a BIP 329 file of labels and a copy of the database, and
-- a backup has one of each, with the same time in their names. Their
-- views both start with wallets and portfolios. So each view names its
-- kind first, in the same place, in the words the user knows the format
-- by: "BIP 329 file" and "SQLite database".
fileHeadingDoc :: String -> FilePath -> Doc Style -> Doc Style
fileHeadingDoc kind file about =
  vcat [columns [styled Title kind, styled UserText file], indent 2 about]

{- Database files -}

-- | A database file as show shows it, the dates in the time zone of the
-- computer ('databaseFileDoc')
databaseFileAnswerDoc :: FilePath -> DatabaseShown -> IO (Doc Style)
databaseFileAnswerDoc file shown = do
  created <- mapM (mapM (utcToLocalZonedTime . portfolioShownCreated) . walletShownPortfolios) (databaseShownWallets shown)
  return (databaseFileDoc file shown created)

-- | A database file as show shows it: its heading, which says that it is
-- an SQLite database, its format version and how many wallets and
-- portfolios it holds, then each wallet with its portfolios, a blank line
-- between the wallets. The dates the portfolios were created are given
-- apart, in a time zone, in the order of the wallets and of the
-- portfolios of each.
--
-- The view is the one of the wallets of a file of a wallet
-- ('walletImportDoc'): a wallet, its fingerprint, and its portfolios
-- under it. The user reads both the same way, and the heading tells
-- which file it is.
databaseFileDoc :: FilePath -> DatabaseShown -> [[ZonedTime]] -> Doc Style
databaseFileDoc file shown created =
  vcat . intersperse mempty $
    fileHeadingDoc "SQLite database" file about
      : zipWith shownWalletDoc wallets created
  where
    wallets = databaseShownWallets shown
    portfolioCount = genericLength (concatMap walletShownPortfolios wallets) :: Natural
    about =
      text $
        "A database of hw at format version "
          <> show (databaseShownVersion shown)
          <> ", with "
          <> holding (genericLength wallets) portfolioCount
    holding :: Natural -> Natural -> String
    holding 0 _ = "no wallet"
    holding walletCount count = counted walletCount "wallet" "wallets" <> " and " <> counted count "portfolio" "portfolios"

-- | A wallet of a database file: its name, the fingerprint of its seed
-- and its description when it has one, then a line for each of its
-- portfolios, given the date each was created. The line of a portfolio
-- starts with the mark of the active one ('activeMarkDoc'), as in the
-- listing of the portfolios. Its name is padded to the longest name of
-- the wallet, so that the days line up. The description comes last, since
-- not every portfolio has one.
shownWalletDoc :: WalletShown -> [ZonedTime] -> Doc Style
shownWalletDoc wallet created =
  vcat $
    columns
      ( [ pretty (walletName (walletShownIndex wallet)) `withStyle` Account,
          pretty (fingerprintToText (walletShownFingerprint wallet))
        ]
          <> [labelWord description | let description = walletShownDescription wallet, not (Text.null description)]
      )
      : (indent 2 <$> zipWith portfolioDoc portfolios created)
  where
    portfolios = walletShownPortfolios wallet
    nameOf = cs . portfolioName . portfolioShownIndex :: PortfolioShown -> String
    pad = fromIntegral (maximum (0 : (length . nameOf <$> portfolios)))
    portfolioDoc p day =
      activeMarkDoc (portfolioShownActive p)
        <+> columns
          ( [ text (block pad (nameOf p)) `withStyle` Account,
              "created" <+> text (dayText day)
            ]
              <> [labelWord description | let description = portfolioShownDescription p, not (Text.null description)]
          )

{- Backups -}

-- | The two files of a backup hw made before a command lost what the
-- database held, a line each, under the answer of that command. The text
-- given first says of the copy of the database when it was made, and
-- the file follows. The user reads there where what was lost still is.
--
-- The line of the labels says where they are, not that they were
-- written: hw writes no second copy of labels that the newest copy holds
-- already, and names that one ("Haskoin.Wallet.Backup").
backupFilesDocs :: String -> BackupFiles -> [Doc Style]
backupFilesDocs copied files =
  [ text copied <+> styled UserText (backupDatabase files),
    "Its labels are in" <+> styled UserText (backupLabels files)
  ]

-- | What restoredatabase did, the dates in the time zone of the computer
-- ('restoredDoc')
restoredAnswerDoc :: FilePath -> DatabaseShown -> Maybe BackupFiles -> Maybe FilePath -> IO (Doc Style)
restoredAnswerDoc file shown backupM damagedM = do
  created <- mapM (mapM (utcToLocalZonedTime . portfolioShownCreated) . walletShownPortfolios) (databaseShownWallets shown)
  return (restoredDoc file shown created backupM damagedM)

-- | What restoredatabase did: the copy the database was restored from,
-- then each wallet the database holds now with its portfolios, as show
-- lists those of a database file ('shownWalletDoc'), then the backup hw
-- made of the database it replaced, or where hw moved that database when
-- it was damaged. A blank line sets the three apart.
-- The dates the portfolios were created are given apart, in a time zone,
-- in the order of the wallets and of the portfolios of each.
restoredDoc :: FilePath -> DatabaseShown -> [[ZonedTime]] -> Maybe BackupFiles -> Maybe FilePath -> Doc Style
restoredDoc file shown created backupM damagedM =
  vcat . intersperse mempty $
    [styled Good "The database was restored from" <+> styled UserText file]
      <> holding
      <> [vcat (backupFilesDocs "The database it replaced was copied to" backup) | Just backup <- [backupM]]
      <> ["The damaged database it replaced was moved to" <+> styled UserText damaged | Just damaged <- [damagedM]]
  where
    wallets = databaseShownWallets shown
    holding
      | null wallets = ["It holds no wallet"]
      | otherwise = zipWith shownWalletDoc wallets created

-- | The copies hw keeps, as showbackups shows them, given the time now:
-- the automatic backups, then the backups of the user, a blank line
-- between. Each starts with a line that names it, with its directory,
-- as the views of a file start. Under it comes a row for each kind of
-- copy.
backupsDoc :: UTCTime -> BackupsShown -> Doc Style
backupsDoc now shown =
  vcat . intersperse mempty $
    [ directoryDoc
        "Automatic backups"
        (backupsAutoDirectory shown)
        [ copiesDoc now "Database" (backupsDatabase shown) (turnedOff "autobackup-database" (backupsDatabaseOn shown)),
          copiesDoc now "Labels" (backupsLabels shown) (turnedOff "autobackup-labels" (backupsLabelsOn shown))
        ],
      directoryDoc "User backups" (backupsUserDirectory shown) [copiesDoc now "Database" (backupsUser shown) Nothing]
    ]
  where
    directoryDoc title directory rows =
      vcat (columns [styled Title title, styled UserText directory] : (indent 2 <$> rows))
    turnedOff key on
      | on = Nothing
      | otherwise = Just ("Turned off: the key " <> key <> " of the configuration is false")

-- | The copies of one kind in a directory, under the name of the kind:
-- how many there are and how long ago the newest was made, then the name
-- of that file, under the count. A kind that the configuration turns off
-- says so on a line of its own, in the colour of what waits for the
-- user, under the copies made while it was on. The names are padded to
-- \"Database\", the longest.
copiesDoc :: UTCTime -> Text -> CopiesShown -> Maybe String -> Doc Style
copiesDoc now kind copies offM =
  field 8 kind <> align (vcat (counting <> [styled Waiting off | Just off <- [offM]]))
  where
    counting = case (copiesNewest copies, copiesCount copies) of
      -- A kind that is off says so alone
      (Nothing, _) -> ["no copy yet" | isNothing offM]
      (Just (file, time), 1) -> [text ("1 copy, made " <> agoText now time), styled UserText (takeFileName file)]
      (Just (file, time), count) ->
        [ text (show count <> " copies, the newest made " <> agoText now time),
          styled UserText (takeFileName file)
        ]

{- Files of a wallet -}

-- | A file of a wallet as show answers with it: its heading, which says
-- that it is a BIP 329 file, then what importwallet would make of it
-- ('walletFileParts'), a blank line between the parts
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
-- tags that nothing has ('fileTagsDoc'), and last the counts importwallet
-- would end its answer with ('importCountsDoc'): what it would set, what
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
-- would not ('recordFateDocs'). What comes from
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
-- would replace ('replacedLabelsDocs'). A line ends with why importwallet
-- would set nothing of its record ('recordFateDocs'). What comes from the
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

{- Addresses -}

-- | An address of the network as addresses lists it, in the view asked
-- for, the time it was created in the time zone of the computer. Its path
-- is padded to the given width, that of the longest path of its section
-- ('longestPathWidth').
listedAddressDoc :: View -> Network -> AmountUnit -> Natural -> DBAddress -> AddressFunds -> [ShownTag] -> IO (Doc Style)
listedAddressDoc view net unit pad addr funds tags = do
  created <- utcToLocalZonedTime (dBAddressCreated addr)
  return $ case view of
    Compact -> compactAddressDoc net unit pad addr funds tags
    -- A listing is of the active portfolio
    Verbose -> verboseAddressDoc ownPathsAlone net unit pad created addr funds tags
    -- The table needs the account for the width of its addresses:
    -- rowsPrinter lays it out ('addressRowDoc')
    Table -> compactAddressDoc net unit pad addr funds tags

-- | The first line of an address of the network: its type and its
-- derivation path ('rowPathText'), which tell which address of the
-- portfolio it is, receiving or change, padded to the width given, then
-- the address and its label. The path names its portfolio when the paths
-- of the view do (the type t'PathNaming'): in the answer about an address of
-- another portfolio than the active one. In a listing of addresses of
-- both chains, a change address says so in place of a label, which it
-- never has. The tags of the address end the line, each between
-- brackets ('tagMarks'), in the compact view and in the verbose one
-- alike: they are few and short, and say which groups the address is in
-- as the label says what it is for.
addressLine :: PathNaming -> Network -> Bool -> Natural -> DBAddress -> [ShownTag] -> Doc Style
addressLine paths net mixed pad addr tags =
  columns $
    [text (block pad (cs (rowPathText net addr) <> ownSuffix paths)), pretty (dBAddressAddress addr)]
      <> [labelWord label | let label = dBAddressLabel addr, not (Text.null label)]
      <> [changeWord | mixed, dBAddressInternal addr]
      <> [hsep (tagMarks tags) | not (null tags)]

-- | The path of an address of the wallet on the network, as every view
-- writes it: the type of its account in front, as in bip84\/0\/3
-- ('poolPathText'). A portfolio has an address at \/0\/3 in each of its
-- accounts on a network, so the path alone would name two. The type
-- costs five columns, and saves the user from reading the address to
-- tell them apart. The path is alone for an address that does not read on
-- the network, which hw never stores.
rowPathText :: Network -> DBAddress -> Text
rowPathText net addr = maybe (pathText (addrPath addr)) poolPathText (addrPoolPath net addr)

-- | What marks a change address where a label would go: the user cannot
-- label one, and hw knows which addresses are change
changeWord :: Doc Style
changeWord = styled Muted "change address"

-- | An address in two lines: its first line, then what it holds, what
-- it received and how many transactions it has, or that it is unused.
-- addresses lists one kind of address at a time, so none is marked.
--
-- An address of a pool that never synced is neither: hw asked no server
-- about it, so its second line says that it is not synced
-- ('notSyncedWords'), and not that it is unused, which is a statement
-- about the chain (the type t'AddressFunds').
--
-- What it holds is the total of its coins, then what it cannot spend of
-- them now, when there is any (the type t'CoinTotals'). The line started
-- with the spendable amount alone: an address whose coin was locked, or
-- waited for a confirmation, read \"spendable 0\" as an address that a
-- payment had emptied. The spendable amount is not written out, as the
-- line of a portfolio writes it ('shortBalanceFigures'): it is the total
-- on nearly every address, and the three amounts with what the address
-- received would make a line of 117 columns. It is the total without the
-- unspendable amount, and balances gives the three.
--
-- The count is of every transaction of the address, those that spent from
-- it as well as those that paid it: the store gives no other. So it
-- stands apart, after a comma. It followed the amount received as "in 2
-- transactions", which said of an address paid once, then emptied, that
-- two payments had made up its amount.
compactAddressDoc :: Network -> AmountUnit -> Natural -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
compactAddressDoc = compactAddressDocOf ownPathsAlone

-- | 'compactAddressDoc', with the path of the address written as the
-- paths of the view are: an answer may be about another portfolio than
-- the active one, and its paths then name theirs
compactAddressDocOf :: PathNaming -> Network -> AmountUnit -> Natural -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
compactAddressDocOf paths net unit pad addr funds tags =
  vcat [addressLine paths net False pad addr tags, indent 2 activity]
  where
    txs = fromIntegral (dBAddressBalanceTxs addr)
    activity = case funds of
      NotSynced -> notSyncedWords
      Holds totals
        | txs == 0 -> styled Muted "unused"
        | otherwise ->
            commaList $
              [totalFigure unit totals.coinsTotal]
                <> [unspendableFigure unit rest | let rest = coinsUnspendable totals, rest > 0]
                <> [ "received" <+> amountPrinter unit (dBAddressBalanceReceived addr),
                     text (counted txs "transaction" "transactions")
                   ]

-- | What a view writes in place of the amounts and the counts of an
-- address of a pool that never synced (the type t'AddressFunds'), and
-- in place of those of a portfolio on such a network, as faint as
-- \"unused\": the words sync uses for a network it did not sync
notSyncedWords :: Doc Style
notSyncedWords = styled Muted "not synced"

-- | An address with every detail: its first line, its path written as
-- the paths of the view are, then its fields. What it holds comes in
-- the two splits of the verbose view of a portfolio
-- ('verbosePortfolioDoc'), in the same order: the total of its coins with
-- what it can spend of them and the rest, then its confirmed and
-- unconfirmed balances.
--
-- An address of a pool that never synced has one field in place of
-- these eight, which says that it is not synced ('notSyncedWords'):
-- eight zeros would say that nobody paid it, and hw never asked (the
-- type t'AddressFunds').
verboseAddressDoc :: PathNaming -> Network -> AmountUnit -> Natural -> ZonedTime -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
verboseAddressDoc paths net unit pad created addr funds tags =
  vcat (addressLine paths net False pad addr tags : (indent 2 <$> (held <> [field 12 "Created" <> text (secondText created)])))
  where
    held = case funds of
      NotSynced -> [field 12 "Balance" <> notSyncedWords]
      Holds totals ->
        [ field 12 "Transactions" <> countPrinter (fromIntegral (dBAddressBalanceTxs addr)),
          field 12 "Received" <> amountPrinter unit (dBAddressBalanceReceived addr),
          field 12 "Total" <> amountPrinter unit totals.coinsTotal,
          field 12 "Spendable" <> amountPrinter unit totals.coinsSpendable,
          field 12 "Unspendable" <> waitingAmount unit (coinsUnspendable totals),
          field 12 "Confirmed" <> amountPrinter unit (dBAddressBalanceConfirmed addr),
          field 12 "Unconfirmed" <> waitingAmount unit (dBAddressBalanceUnconfirmed addr),
          field 12 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr))
        ]

-- | An address as an answer shows it, with where it is, and with its QR
-- code when the command gives the address out and someone can scan the
-- screen. The active portfolio given, if any, is named when the address is
-- of another one ('seenPortfolioDoc'), and the path of the address then
-- names its portfolio (the type t'PathNaming'): label and signmsg would
-- read the path alone in the active portfolio, at another address. Each
-- portfolio is named with its wallet or not, as given.
--
-- With --with-change, tag and untag end the answer with the change
-- addresses that followed the address, after an empty line
-- ('changeFollowedDoc').
addressAnswerDoc :: WalletNaming -> AddressCode -> AmountUnit -> Maybe PortfolioRef -> AccountRef -> DBAddress -> AddressFunds -> [ShownTag] -> Maybe ChangeFollowed -> IO (Doc Style)
addressAnswerDoc naming code unit activeM ref addr funds tags followedM = do
  created <- utcToLocalZonedTime (dBAddressCreated addr)
  let net = accountRefNetwork ref
      paths = pathsSeenFrom naming (SeenFrom (placePortfolio (accountRefPlace ref)) activeM)
      -- One address: its path needs no padding
      details = verboseAddressDoc paths net unit 0 created addr funds tags
  terminal <- hIsTerminalDevice stdout
  let rows
        | code == WithCode && terminal = fromMaybe [] (qrRows (paymentUri net (dBAddressAddress addr)))
        | otherwise = []
      answered = withCodeDoc (vcat [details, indent 2 (accountRefField naming 12 activeM ref)]) rows
  return (vcat (answered : concat [[mempty, changeFollowedDoc paths net unit followed] | Just followed <- [followedM]]))

-- | The change addresses that followed the address tag or untag was
-- given with --with-change: a line that says what was done to them and
-- why, then each as addresses lists it in its compact view, with its
-- tags as they are now and what it holds. The command changed
-- addresses the user did not name, so the answer names every one of
-- them. What each holds shows where the coins of the group are now:
-- the address tagged is most often empty, and the end of its chain holds
-- them. When none followed, one line says so: the user asked for them.
--
-- The change addresses where the tags stopped come after them, apart
-- (@changeStopsDoc@).
changeFollowedDoc :: PathNaming -> Network -> AmountUnit -> ChangeFollowed -> Doc Style
changeFollowedDoc paths net unit followed =
  vcat . intersperse mempty $
    vcat (said : [compactAddressDocOf paths net unit pad addr funds tags | (addr, funds, tags) <- followed.followedRows])
      : [changeStopsDoc paths net unit followed.followedStops | not (null followed.followedStops)]
  where
    pad = pathPad paths net [addr | (addr, _, _) <- followed.followedRows]
    count = counted (fromIntegral (length followed.followedRows)) "change address" "change addresses"
    said = case (followed.followedRetag, null followed.followedRows) of
      (AddTags, False) -> "Tagged with it, down the chain of its payments:" <+> text count
      (AddTags, True) -> "No change address down the chain of its payments was tagged with it"
      (RemoveTags, False) -> "Untagged with it, down the chain of its payments:" <+> text count
      (RemoveTags, True) -> "No change address down the chain of its payments was untagged with it"

-- | The width that lines up the paths of the addresses, as in a listing
pathPad :: PathNaming -> Network -> [DBAddress] -> Natural
pathPad paths net addrs = maximum (0 : [fromIntegral (Text.length (rowPathText net addr) + length (ownSuffix paths)) | addr <- addrs])

-- | The change addresses where the tags of tag --with-change stopped:
-- a line that counts them, then each as 'changeFollowedDoc' lists an
-- address, with why it took no tag below it. The reason names the
-- payment, by the short id every command takes, and the address of the
-- coin from outside the group, by its path: tagging that address is what
-- lets the tags go further, and hw does not decide it for the user.
changeStopsDoc :: PathNaming -> Network -> AmountUnit -> [ChangeStop] -> Doc Style
changeStopsDoc paths net unit stops =
  vcat (text ("Not tagged, where the chain stops: " <> count) : concatMap stopDoc stops)
  where
    pad = pathPad paths net [addr | ChangeStop {changeStopAt = (addr, _, _)} <- stops]
    count = counted (fromIntegral (length stops)) "change address" "change addresses"
    stopDoc stop =
      let (addr, funds, tags) = stop.changeStopAt
       in [compactAddressDocOf paths net unit pad addr funds tags, indent 2 (sentence (wordsOf (why stop)))]
    why stop =
      ("The payment " <> shortHashText stop.changeStopPayment <> " also spends a coin ")
        <> case stop.changeStopSource of
          Just (source, _, _) -> "at " <> cs (rowPathText net source) <> ownSuffix paths <> ", without " <> lacked stop
          Nothing -> "that is not of the portfolio, and has no tag"
    lacked stop = case cs . tagText . shownTag <$> stop.changeStopTags of
      [tag] -> "the tag " <> tag
      tags -> "the tags " <> intercalateAnd tags

-- | The field that says the account of an address: its portfolio, its
-- network and the type of its addresses. The portfolio says when it is not
-- the active one given ('seenPortfolioDoc'), and is named with its wallet
-- or not, as given.
accountRefField :: WalletNaming -> Natural -> Maybe PortfolioRef -> AccountRef -> Doc Style
accountRefField naming pad activeM ref =
  field pad "Account"
    <> columns
      [ seenPortfolioDoc naming (SeenFrom (placePortfolio (accountRefPlace ref)) activeM),
        text (accountRefNetwork ref).name,
        text (cs (kindName (accountRefKind ref)))
      ]

-- | Text, then a QR code drawn in the rows given. The code goes under the
-- text and never beside it: a terminal wraps its long lines again when it
-- is resized, which would mix the text into the code.
withCodeDoc :: Doc Style -> [Text] -> Doc Style
withCodeDoc details rows = vcat (details : (annotate QrCode . pretty <$> rows))

-- | The address at a path of the account that show derives, when the
-- wallet does not hold it: its type and its path, and the address cut, as
-- the first line of addresses, then its account, then what the cut form
-- is for. The wallet holds no row for it, so it has no label and no
-- balance. The user compares its ends with the address the online
-- computer showed, and the note says why the middle is missing, which a
-- user would otherwise take for a fault ("Haskoin.Wallet.CutAddress").
derivedAddrDoc :: WalletNaming -> AccountRef -> SoftPath -> Text -> Doc Style
derivedAddrDoc naming ref path cut =
  vcat
    [ columns [text (cs (poolPathText typed)), pretty cut],
      -- show derives a path in the active portfolio, so there is no other
      -- one to name
      indent 2 (accountRefField naming 9 Nothing ref),
      mempty,
      cutAddressNote typed
    ]
  where
    typed = PoolPath (accountRefKind ref) path

-- | How to check an address with its cut form, and why its middle is
-- hidden. The user reads it in the middle of a payment, about to give the
-- address out, so it says what to do in each case and alarms only when
-- the ends differ. Why the address must not be paid from this screen, that
-- it may lie past the gap, is in the help and the README instead. The
-- word receive is in the colour of a command, as in the list of commands
-- ('commandWord'), so that it reads as the name of one.
cutAddressNote :: PoolPath -> Doc Style
cutAddressNote path =
  sentence $
    wordsOf "Compare both ends with the address that"
      <> [commandWord "receive"]
      <> wordsOf ("gave out at " <> cs (poolPathText path) <> " on the online computer.")
      <> wordsOf "If they match, that address is of this account, and you can give it out."
      <> wordsOf "If they differ, the online computer may have been tampered with, and you should not give the address out."
      <> wordsOf "Nobody can make another address with the same ends."
      <> wordsOf "This screen is for checking only, so the middle of the address is hidden."
      <> wordsOf "Give out the address as"
      <> [commandWord "receive"]
      <> wordsOf "showed it."

-- | An address as balances lists it, in the view asked for. The table
-- needs the account for the widths of its columns: rowsPrinter lays it
-- out ('balanceRowDoc').
listedBalanceDoc :: View -> Network -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
listedBalanceDoc view net unit pad addr balance tags = case view of
  Verbose -> verboseBalanceDoc net unit pad addr balance tags
  _ -> coinBalanceDoc net unit pad addr balance tags

-- | An address as balances shows it: its first line, with a change
-- address marked as one among the receiving addresses, then the value of
-- its coins, what it can spend now, and the rest, which waits
coinBalanceDoc :: Network -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
coinBalanceDoc net unit pad addr balance tags =
  vcat
    [ addressLine ownPathsAlone net True pad addr tags,
      indent 2 (commaList (balanceFigures unit (coinBalanceTotal balance) (coinBalanceSpendable balance)))
    ]

-- | An address as balances --verbose shows it: its first line, then its
-- fields. The value of its coins and how many there are, then what it can
-- spend now, and what it cannot, split as coins tells the state of each
-- coin: locked by a pending transaction, locked by the user, locked by
-- a tag, or not spendable yet. The compact view gives the sum of the
-- four; this one tells which to wait for, and which to unlock.
--
-- The coins that hold tokens of Bitcoin Cash have a line too, only at an
-- address that has some: no other network has tokens, and few addresses
-- of Bitcoin Cash do, so the line would be a zero nearly everywhere.
verboseBalanceDoc :: Network -> AmountUnit -> Natural -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
verboseBalanceDoc net unit pad addr balance tags =
  vcat $
    addressLine ownPathsAlone net True pad addr tags
      : ( indent 2
            <$> [ field 17 "Total" <> amountPrinter unit (coinBalanceTotal balance),
                  field 17 "Coins" <> countPrinter (fromIntegral (dBAddressBalanceCoins addr)),
                  field 17 "Spendable" <> amountPrinter unit (coinBalanceSpendable balance),
                  field 17 "Locked by tx" <> waitingAmount unit (coinBalanceLockedByTx balance),
                  field 17 "Locked by you" <> waitingAmount unit (coinBalanceLockedByUser balance),
                  field 17 "Locked by tag" <> waitingAmount unit (coinBalanceLockedByTag balance)
                ]
              <> [field 17 "Locked by tokens" <> waitingAmount unit (coinBalanceLockedByTokens balance) | coinBalanceLockedByTokens balance > 0]
              <> [field 17 "Not spendable yet" <> waitingAmount unit (coinBalanceNotSpendableYet balance)]
        )

-- | An amount that waits to be spent: in the colour of what waits, unless
-- it is zero. Unconfirmed amounts take it too: what another wallet pays
-- cannot be spent before it confirms, and the wallet does not tell its
-- own unconfirmed coins apart in a balance.
waitingAmount :: AmountUnit -> Word64 -> Doc Style
waitingAmount unit 0 = neutralAmount unit 0
waitingAmount unit v = amountIn (Just Waiting) unit (fromIntegral v)

{- Coins -}

-- A coin shows in one of three views. The compact and the verbose views
-- start with the same first line: the time of the transaction that paid
-- it, its value, its state and its confirmations, as a transaction starts
-- with its time. Only the count of the confirmations differs: the compact
-- view stops it at 999+, and the verbose view gives it in full. The table
-- gives a coin one row ('coinRowDoc').

-- | A coin as coins lists it, in the view asked for, with the time of
-- its transaction in the time zone of the computer. coins lists the
-- active portfolio, so the paths are alone ('ownPathsAlone'). lockcoin and
-- unlockcoin answer with the verbose view, as the commands that answer
-- with one element do ('coinAnswerDoc').
listedCoinDoc :: View -> Network -> AmountUnit -> JsonCoin -> IO (Doc Style)
listedCoinDoc view net unit coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  return $ case view of
    Compact -> compactCoinDoc ownPathsAlone net unit time coin
    Verbose -> verboseCoinDoc ownPathsAlone net unit time coin
    -- The table needs the account for the width of its paths: rowsPrinter
    -- lays it out ('coinRowDoc')
    Table -> compactCoinDoc ownPathsAlone net unit time coin

-- | The first line of a coin in the verbose view: the time of the
-- transaction that paid it, its value, then its state and its
-- confirmations, written by the function given, in the words of
-- 'coinLineCells'
coinLine :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
coinLine countText unit time coin = columns (coinLineCells FullLockWords countText unit time coin)

-- | What the first line of a coin gives: the time of the transaction that
-- paid it, its value, then its state, in the words asked for a coin a
-- pending transaction holds, and its confirmations, written by the
-- function given. The time and the value keep the columns of the first
-- line of a transaction, so that the values of a listing line up.
coinLineCells :: LockWords -> (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> [Doc Style]
coinLineCells wording countText unit time coin =
  [ minuteWord time,
    amountCell (Just Incoming) "" unit (fromIntegral (jsonCoinValue coin)),
    commaList [coinState wording coin, confirmations]
  ]
  where
    confirmations
      | jsonCoinConfirmations coin == 0 = styled Waiting "unconfirmed"
      | otherwise = text (countText (jsonCoinConfirmations coin))

-- | Whether a coin can be spent now, in the colour of its state
-- ('coinStateText'): green when it is spendable, yellow when it waits
coinState :: LockWords -> JsonCoin -> Doc Style
coinState wording coin = styled style (coinStateText wording coin)
  where
    style = if isNothing (jsonCoinLock coin) && jsonCoinSpendable coin then Good else Waiting

-- | How a view words the lock of a pending transaction on a coin: in the
-- few words a line of 80 columns and a table have room for, or in full,
-- as the verbose view and a CSV file spell it out
data LockWords = FewLockWords | FullLockWords
  deriving (Eq, Show)

-- | Whether a coin can be spent now, in words: spendable, locked by a
-- pending transaction, by the user or by a tag of its address, or not
-- spendable yet, as it waits for a confirmation.
--
-- A pending transaction holds the coins it spends, and a replacement
-- also those of the transactions it replaces, which go once it is sent.
-- In few words both are "locked by tx": either way a pending transaction
-- holds the coin, and deletetx frees it. In full, the words say which of
-- the two it is.
coinStateText :: LockWords -> JsonCoin -> String
coinStateText wording coin = case jsonCoinLock coin of
  Just LockedByTransaction -> inWords "locked by a pending transaction"
  Just LockedByReplacement -> inWords "locked by a pending replacement"
  Just LockedByTokens -> "locked by tokens"
  Just LockedByUser -> "locked by you"
  Just LockedByTag -> "locked by tag"
  Nothing
    | jsonCoinSpendable coin -> "spendable"
    | otherwise -> "not spendable yet"
  where
    inWords full = case wording of
      FewLockWords -> "locked by tx"
      FullLockWords -> full

-- | The address that holds a coin, as the verbose view gives it: in full,
-- then its path, with its label, or the mark of a change address, on the
-- line below, as every view that gives an address in full does. The
-- compact view keeps them on the line of the address instead
-- ('compactCoinDoc').
coinAddressDoc :: PathNaming -> Network -> JsonCoin -> Doc Style
coinAddressDoc paths net coin =
  withNotesBelow
    (fullAddressWords paths net (jsonCoinAddress coin) (jsonCoinPath coin))
    (ownAddressNoteBelow (jsonCoinAddressLabel coin) (jsonCoinChange coin) <> [hsep (tagMarks tags) | let tags = jsonCoinAddressTags coin, not (null tags)])

-- | A coin in two lines, as the compact view of a transaction is laid
-- out. The first gives the short id of the coin, which lockcoin and
-- --coin take ("Haskoin.Wallet.ShortId"), first, as in the table, then
-- the time of the transaction that paid it, its value, its state and its
-- confirmations, counted up to 999+. A coin a pending transaction holds
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
-- less of the coin, and only the verbose view gives it. The path is
-- written as the paths of the view are (the type t'PathNaming'): it names its
-- portfolio in a report on the coins of another portfolio than the active
-- one.
compactCoinDoc :: PathNaming -> Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
compactCoinDoc paths net unit time coin =
  vcat
    [ columns (shortCoinWord coin : coinLineCells FewLockWords cappedConfirmations unit time coin),
      indent 2 ("at" <+> columns (fullAddressWords paths net (jsonCoinAddress coin) (jsonCoinPath coin) : ownAddressNote (jsonCoinAddressLabel coin) (jsonCoinChange coin) <> addressTagMarks))
    ]
  where
    -- The tags of the address, after its label: a coin is in the groups
    -- of its address
    addressTagMarks = [hsep (tagMarks tags) | let tags = jsonCoinAddressTags coin, not (null tags)]

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
-- is in no block yet, and shows none. The path is written as the paths of
-- the view are (the type t'PathNaming').
verboseCoinDoc :: PathNaming -> Network -> AmountUnit -> Maybe ZonedTime -> JsonCoin -> Doc Style
verboseCoinDoc paths net unit time coin =
  vcat $
    coinLine fullConfirmations unit time coin
      : ( indent 2
            <$> [field 8 "Label" <> labelWord label | let label = jsonCoinTxLabel coin, not (Text.null label)]
              <> [ field 8 "Coin" <> text (coinText (jsonCoinOutpoint coin)),
                   field 8 "Short id" <> text (shortCoinText (jsonCoinOutpoint coin))
                 ]
              <> [field 8 "Block" <> text (show height) | Store.BlockRef height _ <- [jsonCoinBlock coin]]
              <> [field 8 "Time" <> text (secondText t) | Just t <- [time]]
              <> [field 8 "Address" <> coinAddressDoc paths net coin]
        )

-- | A coin as an answer shows it: with every detail, as coins --verbose
-- does, then the account that holds it, as the answer of an address ends.
-- The account says when it is not of the active portfolio given
-- ('seenPortfolioDoc'), and the path of the address of the coin then names
-- its portfolio (the type t'PathNaming'). Each portfolio is named with its
-- wallet or not, as given. The time is in the time zone of the computer.
coinAnswerDoc :: WalletNaming -> AmountUnit -> Maybe PortfolioRef -> AccountRef -> JsonCoin -> IO (Doc Style)
coinAnswerDoc naming unit activeM ref coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  let paths = pathsSeenFrom naming (SeenFrom (placePortfolio (accountRefPlace ref)) activeM)
  return $ vcat [verboseCoinDoc paths (accountRefNetwork ref) unit time coin, indent 2 (accountRefField naming 8 activeM ref)]

{- Transactions -}

-- A transaction shows in one of three views. The compact and the verbose
-- views give what changed and where it stands on their first line. The
-- compact view takes two lines. The first starts with the short id, the
-- first 8 digits of the hash, then gives the time, the amount and where
-- the transaction stands, the confirmations counted up to 999+. The second
-- gives the balance after it, in txs, then what it is for, in the words of
-- the table ('purposeCell'), which names an address of the wallet by its
-- path. A listing that keeps the transactions at tagged addresses adds a
-- line for each other address that has such a tag ('otherKeptAddresses').
-- The verbose view starts with the time, the type, the amount and
-- the full count of the confirmations, then gives every detail as fields:
-- every address in full, and the hash in full with its short id. The
-- table gives a row of columns under a header: the short id, the time,
-- the type, the amount, the balance, the confirmations, and what the
-- transaction is for.

-- | A transaction as a listing shows it, in the view asked for, with its
-- time in the time zone of the computer. A listing of every portfolio
-- names the portfolio of each transaction, with its wallet or not as the
-- views name the wallets ('portfolioWords'). The balance after
-- it is the one txs gives: the other listings have none. The paths are
-- written as given (the type t'PathNaming'): txs lists the active portfolio
-- ('activePaths'), and a listing of every portfolio has transactions of
-- other ones ('listedPaths').
listedTxDoc :: PathNaming -> View -> Network -> AmountUnit -> Maybe PortfolioRef -> Maybe Integer -> TxInfo -> IO (Doc Style)
listedTxDoc paths view net unit portfolioM balanceM info = do
  time <- localTxTime info
  return $ case view of
    Compact -> compactTxDoc paths net unit portfolioM balanceM time info
    Verbose -> verboseTxDoc paths net unit (whereFields net (listedWhere <$> portfolioM) info) balanceM time info
    -- The table needs the width of its column of tags: sectionsPrinter
    -- lays it out ('txRowDoc', 'pendingRowDoc')
    Table -> compactTxDoc paths net unit portfolioM balanceM time info
  where
    listedWhere ref = text (portfolioWords paths.namingWallets ref) `withStyle` Account

-- | The fields that say where a transaction is, first in its verbose
-- view: its portfolio, as the view names it, when it names it, its
-- network, and the types of the portfolio's addresses it involves, when
-- it involves any. A portfolio holds coins of both types on Bitcoin, and
-- the types tell which of them a transaction touched.
whereFields :: Network -> Maybe (Doc Style) -> TxInfo -> [Doc Style]
whereFields net portfolioM info =
  [field 9 "Portfolio" <> named | Just named <- [portfolioM]]
    <> [field 9 "Network" <> text net.name]
    <> [field 9 "Types" <> text (intercalate ", " (cs . kindName <$> kinds)) | let kinds = summaryKinds (txInfoSummary info), not (null kinds)]

-- | The one transaction a command answers with, with every detail, naming
-- its portfolio when it is one of the wallet, and its network. The user
-- sees whose coins it moves, and which coins: private keys, coins and the
-- legacy addresses of Bitcoin Cash read the same on Bitcoin, and a sweep
-- outside the wallet is on the network of its file. The portfolio says
-- when it is not the active one ('seenPortfolioDoc'), and every path then
-- names its portfolio ('pathsSeenFrom'). Each portfolio is named with its
-- wallet or not, as given.
txAnswerDoc :: WalletNaming -> Network -> Maybe SeenFrom -> AmountUnit -> TxInfo -> IO (Doc Style)
txAnswerDoc naming net seenM unit info = do
  time <- localTxTime info
  let paths = maybe (activePaths naming) (pathsSeenFrom naming) seenM
  return $ verboseTxDoc paths net unit (whereFields net (seenPortfolioDoc naming <$> seenM) info) Nothing time info

-- | The time a transaction shows: the time the store gives it
-- ('chainTime'), for one on the chain, or when the wallet started holding
-- it, for one in preparation. None when hw does not know it, as for a
-- transaction read from a file.
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

-- | The width of a time to the minute, in characters
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
--
-- An external transaction adds nothing to a portfolio and takes nothing
-- from it. Its amount is all that it spends, fee included, as the amount
-- of a payment is: without a sign or a colour, since it is neither in
-- nor out ('spentOfOthers').
headlineDoc :: (Natural -> String) -> AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
headlineDoc countText unit time (TxInfo summary status _ _ _ _ _) =
  hcat
    [ headlineTime status time,
      "  ",
      text (typeWord (summaryType summary)),
      "  ",
      amount,
      "  ",
      statusWords countText (summaryFeeRate summary) status
    ]
  where
    amount = case summaryType summary of
      TxExternal -> amountCell Nothing "" unit (toInteger (spentOfOthers summary))
      _ -> amountColumn unit (summaryAmount summary)

-- | The time a first line starts with ('minuteWord'). A transaction in
-- preparation without one is not held by the wallet, which read it from a
-- file: "from a file" says so, where "time unknown" read as a fault. A
-- file holds no time.
headlineTime :: TxStatus -> Maybe ZonedTime -> Doc Style
headlineTime (Pending _) Nothing = styled Muted (block minuteWidth "from a file")
headlineTime _ time = minuteWord time

-- | The type of a transaction. The four words have the same length,
-- which keeps the amounts in a column. They hold before a transaction is
-- sent as well as after, unlike "sent" and "received".
typeWord :: TxType -> String
typeWord = \case
  TxDebit -> "Outgoing"
  TxCredit -> "Incoming"
  TxInternal -> "Internal"
  TxExternal -> "External"

-- | The length of the four words of 'typeWord'
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
statusWords :: (Natural -> String) -> PaidRate -> TxStatus -> Doc Style
statusWords countText paid = \case
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
    rate = ", " <> paidRateDoc paid

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
-- The second line gives the details, a comma between them: the
-- portfolio and the network, in a listing of every portfolio, with the
-- wallet when the views name the wallets ('portfolioWords'); the
-- balance after the transaction, in
-- txs; the transactions a replacement that bumpfee prepared replaces, in
-- yellow ('replacesNote'); the transactions that replaced a dead one
-- ('replacedByNote'); and last what the transaction is for, in the
-- words of the last column of the table ('purposeCell'). The label of a payment says what it is for
-- better than the address it pays, and takes its place. What it is for
-- has no width to fix in advance, so it ends the line: a long address or
-- label pushes nothing after it. The verbose view gives every address in
-- full, and the type of the transaction, which the sign of the amount and
-- the words "at" and "to" tell here. A transaction with no detail, which
-- pays no address and has no label, takes one line. A path is written as
-- the paths of the view are (the type t'PathNaming').
--
-- A listing that keeps the transactions at tagged addresses
-- (--address-tag) adds lines below the second one. Each gives one
-- address that has a tag asked for and that the second line does not
-- name, with that tag ('otherKeptAddresses'). Such an address is often a
-- contact of the address book, written in full with its label. On the
-- second line it came after the balance and an address already, and the
-- line ran far past 80 columns. A line of its own also sets it apart
-- from what the transaction is for: it says why the transaction is in
-- the listing.
compactTxDoc :: PathNaming -> Network -> AmountUnit -> Maybe PortfolioRef -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
compactTxDoc paths net unit portfolioM balanceM time info =
  vcat $
    compactHeadline unit time info
      : [indent 2 (hcat (intersperse ", " details)) | not (null details)]
        <> (indent 2 <$> otherKeptAddresses paths net info)
  where
    details =
      [(text (portfolioWords paths.namingWallets ref) `withStyle` Account) <+> text net.name | Just ref <- [portfolioM]]
        <> ["balance" <+> neutralAmount unit balance | Just balance <- [balanceM]]
        <> maybeToList (replacesNote (txInfoStatus info))
        <> maybeToList (replacedByNote (txInfoStatus info))
        <> purposePart paths net info
        -- The tags of the transaction end the line, each between
        -- brackets
        <> [hsep (tagMarks tags) | let tags = txOwnTags (txInfoTags info), not (null tags)]

-- | What a replacement says of the transactions it replaces, by their
-- short ids: "replaces a1075db5". Nothing for a transaction that replaces
-- none. It is yellow, as what waits for something: the network keeps only
-- one of the transactions, and until it mines one of them, a payment
-- stands in two places. The user may not expect it, since the replaced
-- transaction is still in the history, so it stands out in every view
-- that shows a pending transaction.
replacesNote :: TxStatus -> Maybe (Doc Style)
replacesNote = \case
  Pending pending
    | not (null (pendingReplaces pending)) ->
        Just (("replaces" <+> shortIdList (pendingReplaces pending)) `withStyle` Waiting)
  _ -> Nothing

-- | What a dead transaction says of the transactions that replaced it, by
-- their short ids: "replaced by 2b69e4f4". Nothing for a live
-- transaction, and nothing for a dead one that names none
-- ('chainReplacedBy'): the network dropped it, or the portfolio cannot
-- tell. It answers what the user asks of a dead payment: whether it went
-- out in another transaction, and in which.
--
-- It has no colour. The word "dead" beside it is red already, and the
-- note waits for nothing, unlike the "replaces" of a pending
-- replacement.
replacedByNote :: TxStatus -> Maybe (Doc Style)
replacedByNote = \case
  OnChain status
    | not (null (chainReplacedBy status)) -> Just ("replaced by" <+> shortIdList (chainReplacedBy status))
  _ -> Nothing

-- | The first line of the compact view of a transaction: its short id,
-- faint for one on the chain ('hashWord'), its time to the minute, what
-- it adds to the account or takes from it, fee included, and where it
-- stands, the confirmations counted up to 999+. Its columns line up from
-- one transaction to the next. It leaves out the type: the sign and the
-- colour of the amount tell what comes in and what goes out, so the line
-- keeps to 80 columns.
compactHeadline :: AmountUnit -> Maybe ZonedTime -> TxInfo -> Doc Style
compactHeadline unit time (TxInfo summary status _ _ _ _ _) =
  columns
    [ hashWord status,
      minuteWord time,
      amountColumn unit (summaryAmount summary),
      statusWords cappedConfirmations (summaryFeeRate summary) status
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
-- address of another wallet, or of another portfolio, for a payment, one
-- of the portfolio for a payment received or one between its addresses.
-- None for a transaction that pays no address.
--
-- Between the addresses of the portfolio, a receiving address comes
-- before a change address, whatever each is paid. The user named the
-- receiving address, and the change of such a payment is often the larger
-- of the two. Naming the change first told the user that the payment went
-- to a change address.
--
-- In a payment received, an address of the portfolio comes before any
-- other, whatever each is paid. One in preparation lists the others too
-- ('paidTo'), and the portfolio is paid "at" its own address only.
mainPayee :: TxInfo -> Maybe Payee
mainPayee info =
  case sortOn rank (fst (paidTo info)) of
    [] -> Nothing
    first : others -> Just (Payee preposition first (length others))
  where
    kind = summaryType (txInfoSummary info)
    preposition = if kind == TxCredit then "at" else "to"
    rank row = (kind == TxCredit && not (ofViewed row), kind == TxInternal && onChangeChain row, Down (txAddressValue row))
    onChangeChain row = maybe False (isIntPath . pathInAccount . (.heldPath)) (txAddressHeld row)
    ofViewed row = maybe False (isNothing . (.heldIn)) (txAddressHeld row)

-- | Whom a transaction pays: "at bip44\/0\/3 \"Rent\"", "to bip84\/0\/5 of
-- portfolio1", "to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and 2 more". An
-- address of the wallet is named by its path, as the paths of the view
-- are written ('heldPathWord'), then by what the function given says of
-- it: 'purposeCell' gives the label of the transaction there, or else
-- that of the address, or else the mark of a change address. The address
-- of another wallet is in full. One of the address book is underlined,
-- then what the second function says of its contact: "to
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\"".
payeeWordsNoted :: PathNaming -> Network -> (HeldAddress -> [Doc Style]) -> (Contact -> [Doc Style]) -> Payee -> Doc Style
payeeWordsNoted paths net noteOf contactNoteOf (Payee preposition row others) =
  hsep $ [text (cs preposition), target] <> note <> more
  where
    target = case (txAddressHeld row, txAddressContact row) of
      (Just held, _) -> heldPathWord paths held
      (Nothing, Just _) -> bookAddressWord net (txAddress row)
      (Nothing, Nothing) -> text (addrToString net (txAddress row))
    -- A path is short: its label stays on its line. So does the label of
    -- a contact, which ends the line.
    note = case (txAddressHeld row, txAddressContact row) of
      (Just held, _) -> noteOf held
      (Nothing, Just contact) -> contactNoteOf contact
      (Nothing, Nothing) -> []
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
-- portfolio of the view does not receive it ('addressRow').
--
-- The other choice was to leave such an address uncoloured. It would show
-- at a glance that the amount leaves the portfolio. The first line of the
-- view says that already. And an address of the user would look like one
-- to check.

-- | An address of the wallet in a transaction: where it is, and its
-- label. It is of the portfolio the view is seen from, or of another
-- portfolio of the database, which is then named.
data HeldAddress = HeldAddress
  { heldPath :: !PoolPath,
    heldLabel :: !Text,
    -- | The portfolio that holds it, when that is another one than the
    -- portfolio of the view
    heldIn :: !(Maybe PortfolioRef)
  }

-- | An address another portfolio holds, as an address of the wallet in a
-- transaction
heldElsewhereAddress :: HeldElsewhere -> HeldAddress
heldElsewhereAddress held = HeldAddress (elsewherePath held) (elsewhereLabel held) (Just (elsewherePortfolio held))

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
-- faint, where its label would be ('bookWord'). So a screen without
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

-- | The columns of the table of txs, that of the tags as wide as given
txColumns :: AmountUnit -> Natural -> [Column]
txColumns unit tagsWide =
  [ column "Txid" shortIdWidth LeftSide,
    column "Time" minuteWidth LeftSide,
    column "Type" typeWidth LeftSide,
    column (amountHeading "Amount" unit) amountWidth RightSide,
    column (amountHeading "Balance" unit) amountWidth RightSide,
    -- Its cells take four characters at most, "999+" and "dead": the
    -- heading sets its width
    column "Confs" 4 RightSide,
    tagsColumn tagsWide,
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

-- | A transaction on one row of the table of txs, under the columns
-- given ('txColumns'): its short id, its time,
-- its type, its amount, the balance after it and its confirmations, each
-- in its column, then what it is for ('purposeCell'). The amount has its sign and its
-- colour, as on the first line of the compact view. A dead transaction leaves its balance empty, since
-- the balance does not count it. Its description says first which
-- transactions replaced it, if any ('replacedByNote'), as the compact
-- view does. The table lists the active portfolio,
-- so no row names its portfolio, and its paths are written as given
-- ('activePaths'): alone, but for an address of another portfolio.
txRowDoc :: PathNaming -> Network -> AmountUnit -> [Column] -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
txRowDoc paths net unit headed balanceM time info@(TxInfo summary status _ _ _ _ _) =
  tableLine (zip headed cells)
  where
    cells =
      [ hashWord status,
        timeCell time,
        text (typeWord (summaryType summary)),
        signedAmountCell unit (summaryAmount summary),
        maybe mempty (text . cs . showAmount unit) balanceM,
        tableConfirmations status,
        tagsCell (txOwnTags (txInfoTags info)),
        descriptionCell
      ]
    -- The words of the compact view, all in one cell, a comma between
    -- them
    descriptionCell = hcat . intersperse ", " $ maybeToList (replacedByNote status) <> purposeWords paths net info

-- | What a transaction is for, as the last cell of the table of txs and
-- the end of the compact view say it. A payment received, or one between the addresses of the wallet, gives the path of
-- the address it pays the most, then the label of the transaction, or
-- else the label of that address, or else that it is a change address:
-- "at bip44\/0\/3 \"Salary\"". A payment to another portfolio of the
-- database reads the same way, its path naming that portfolio: "to
-- bip84\/0\/5 of portfolio1 \"Savings\"". The path says the payment went
-- to the user, which the label alone does not. Any other payment gives its
-- label alone, or else the address of the other wallet it pays the most:
-- "to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU". Each tells how many other
-- addresses it pays. A label and an address are whole, however long.
--
-- In a listing that keeps the transactions at tagged addresses
-- (--address-tag), the address named by its path is followed by those of
-- its tags the listing keeps ('keptTagsAt'). 'otherKeptAddresses' gives
-- the other addresses of the transaction that have such a tag.
--
-- An address of the address book is named as an address of the wallet
-- is: always, with a label or without, since that it is a known address
-- is what the user wants to see. It is underlined, and followed by the
-- label of the transaction, or else by that of its contact
-- ('contactNote').
purposeCell :: PathNaming -> Network -> TxInfo -> Doc Style
purposeCell paths net info = case mainPayee info of
  Just payee
    | namesPayee payee -> payeeWordsNoted paths net (heldNote (txAddress (payeeRow payee))) bookNote payee
  _ -> if Text.null label then mempty else labelWord label
  where
    label = txInfoLabel info
    namesPayee payee =
      isJust (txAddressHeld (payeeRow payee))
        || isJust (txAddressContact (payeeRow payee))
        || Text.null label
    -- The label of the transaction says what it is for better than the
    -- label of the contact it pays. The tags of the contact that the
    -- listing keeps follow, as those of an address of the wallet do.
    bookNote contact =
      (if Text.null label then contactNote contact else [labelWord label])
        <> tagMarks (keptContactTags info contact)
    -- The label of the transaction says what it is for better than the
    -- label of its address
    heldNote addr held =
      ( if Text.null label
          then ownAddressNote held.heldLabel (isIntPath (pathInAccount held.heldPath))
          else [labelWord label]
      )
        <> tagMarks (keptTagsAt info addr)

-- | What 'purposeCell' says of a transaction, as one of the parts a
-- short view puts a comma between. None for a transaction that pays no
-- address and has no label, so that no comma stands alone.
purposePart :: PathNaming -> Network -> TxInfo -> [Doc Style]
purposePart paths net info = [purpose | not (null (renderPlain purpose))]
  where
    purpose = purposeCell paths net info

-- | The addresses that put a transaction in a listing that keeps the
-- transactions at tagged addresses (--address-tag), but for the one
-- 'purposeCell' names, whose tags are beside it there. So the reader
-- sees, for each transaction of such a listing, which address put it
-- there. Outside such a listing there are none.
--
-- First come the addresses of the portfolio that have a tag asked for,
-- in the order of their paths. Each is named by its path, with those
-- tags: "bip84\/1\/2 [kyc]".
--
-- Then comes each contact of the address book that has such a tag, by
-- its address, underlined, with its label and those tags:
-- "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" [rent]". A payment
-- received names the address it was paid at, so the contact that paid it
-- is seen only here. The label is given, unlike that of an address of
-- the portfolio: a path says which address it is, and the address of
-- somebody else says nothing without its name.
--
-- The compact view gives each of them a line of its own
-- ('compactTxDoc'). The tables keep them in their last cell
-- ('purposeWords').
otherKeptAddresses :: PathNaming -> Network -> TxInfo -> [Doc Style]
otherKeptAddresses paths net info =
  [ heldPathWord paths (HeldAddress path "" Nothing) <+> hsep (tagMarks kept)
    | (addr, path) <- ownAddresses,
      Just addr /= named,
      let kept = keptTagsAt info addr,
      not (null kept)
  ]
    <> [ hsep (bookAddressWord net addr : contactNote contact <> tagMarks kept)
         | (addr, contact) <- Map.toList (txInfoContacts info),
           Just addr /= namedContact,
           -- An address another portfolio holds is of the wallet
           not (Map.member addr (txInfoElsewhere info)),
           let kept = keptContactTags info contact,
           not (null kept)
       ]
  where
    -- The contact 'purposeCell' names by its address
    namedContact = case mainPayee info of
      Just payee
        | isNothing (txAddressHeld (payeeRow payee)), isJust (txAddressContact (payeeRow payee)) -> Just (txAddress (payeeRow payee))
      _ -> Nothing
    summary = txInfoSummary info
    -- The addresses of the portfolio in the transaction, each once, in
    -- the order of their paths
    ownAddresses =
      sortOn snd . Map.toList $
        Map.map myOutputsPath (summaryMyOutputs summary) <> Map.map myInputsPath (summaryMyInputs summary)
    -- The address of the portfolio 'purposeCell' names by its path
    named = case mainPayee info of
      Just payee
        | Just held <- txAddressHeld (payeeRow payee), isNothing held.heldIn -> Just (txAddress (payeeRow payee))
      _ -> Nothing

-- | What the last cell of a table says a transaction is for, as the
-- parts it puts a comma between: what 'purposeCell' says, then each
-- other address that has a tag the listing was asked to keep
-- ('otherKeptAddresses'). A row of a table is one line, so these
-- addresses stay in the cell, where the compact view gives each a line
-- ('compactTxDoc'). The cell is the last one, and a wide row scrolls
-- sideways.
purposeWords :: PathNaming -> Network -> TxInfo -> [Doc Style]
purposeWords paths net info = purposePart paths net info <> otherKeptAddresses paths net info

{- The table of addresses -}

-- The table of addresses gives what the compact view gives: the path, the
-- address, what it can spend, what it received, how many transactions
-- it has, then its label. addresses lists the receiving addresses
-- or the change addresses, never both at once. A change address never
-- has a label, so the table of change addresses has no column for one.

-- | The columns of the table of the addresses of the account, on its
-- receiving chain or on its change chain. The path takes the width given,
-- that of the longest path of the account on the chain
-- ('longestPathWidth'), the address that of the widest address the
-- account can have ('addressWidth'), and the tags the second width given.
addressTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Natural -> Bool -> [Column]
addressTableColumns unit net kinds pad tagsWide internal =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column (amountHeading "Total" unit) amountWidth RightSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Received" unit) amountWidth RightSide,
    column "Txs" txCountWidth RightSide,
    tagsColumn tagsWide
  ]
    <> [column "Label" 0 LeftSide | not internal]

-- | The column of the tags, in every table that has one, as wide as
-- given: the widest set of tags that a row of its kind has in the
-- database (the type t'TagWidths'), so that no row runs past it and the
-- label after it stays in line. It comes right before the last column,
-- which holds a label or a description, a text of any length that a
-- table does not cut.
tagsColumn :: Natural -> Column
tagsColumn tagsWide = column "Tags" tagsWide LeftSide

-- | Tags in a cell of a table, or after the name of a field: a space
-- between them, each in its colour. The column or the field says they are
-- tags, so they need no mark.
tagsCell :: [ShownTag] -> Doc Style
tagsCell = hsep . map tagWord

-- | A tag in its colour, or in the colour of the text when it has none
tagWord :: ShownTag -> Doc Style
tagWord shown = maybe id (annotate . TagColour) (shownTagColor shown) (text (cs (tagText (shownTag shown))))

-- | Tags on a line of prose, each between brackets: [kyc] [cold]. The
-- brackets tell a tag from the words around it, a label or a path, on a
-- screen without colours too. They are faint, and only the word has the
-- colour of the tag. A hash sign was not used: a user who copies #kyc
-- to a command line starts a comment of the shell.
tagMarks :: [ShownTag] -> [Doc Style]
tagMarks = map (\shown -> styled Muted "[" <> tagWord shown <> styled Muted "]")

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
-- path, the address, what its coins are worth, what it can spend of them
-- now and what it received, in green unless zero, how many transactions
-- it has, then its label. The total beside the spendable amount tells an
-- address whose coin is locked, or waits, from one that holds nothing.
-- An address no transaction paid leaves its amounts empty, which sets it
-- apart from one that was paid and spent everything.
--
-- An address of a pool that never synced leaves its number of
-- transactions empty too: a zero there would say that nobody paid it,
-- and hw never asked (the type t'AddressFunds'). The column is too
-- narrow for the words of the other views.
addressRowDoc :: AmountUnit -> Network -> [Column] -> DBAddress -> AddressFunds -> [ShownTag] -> Doc Style
addressRowDoc unit net headed addr funds tags =
  tableLine (zip headed cells)
  where
    cells =
      [text (cs (rowPathText net addr)), pretty (dBAddressAddress addr)]
        <> figures
        <> [tagsCell tags]
        <> [labelCell | not (dBAddressInternal addr)]
    -- The three amounts, then the number of transactions
    figures = case funds of
      NotSynced -> replicate 4 mempty
      Holds totals
        | dBAddressBalanceTxs addr > 0 -> (heldAmountCell unit <$> [totals.coinsTotal, totals.coinsSpendable, dBAddressBalanceReceived addr]) <> [txCount]
        | otherwise -> replicate 3 mempty <> [txCount]
    txCount = text (show (dBAddressBalanceTxs addr))
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
    column (amountHeading "Total" unit) amountWidth RightSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Unspendable" unit) amountWidth RightSide,
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
    [] -> [tableLine (zip headed (named <> replicate 6 mempty <> [describedCell]))]
    active ->
      [ tableLine . zip headed $
          named
            <> [text (networkViewNetwork net).name]
            <> holdings net
            <> [text (show (networkViewAddresses net)), describedCell]
        | net <- active
      ]
  where
    -- The three amounts, then the number of coins. A network that never
    -- synced says so under the total, and leaves the rest empty: a zero
    -- there would say that the portfolio holds nothing.
    holdings net
      | networkViewSynced net =
          [ heldAmountCell unit (networkViewTotal net),
            heldAmountCell unit (networkViewSpendable net),
            waitingAmountCell unit (networkViewUnspendable net),
            text (show (networkViewCoins net))
          ]
      | otherwise = notSyncedWords : replicate 3 mempty
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
coinTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Natural -> [Column]
coinTableColumns unit net kinds pad tagsWide =
  [ column "Coin" coinIdWidth LeftSide,
    column "Time" minuteWidth LeftSide,
    column (amountHeading "Value" unit) amountWidth RightSide,
    column "State" coinStateWidth LeftSide,
    column "Confs" 4 RightSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column "Path" pad LeftSide,
    tagsColumn tagsWide,
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
-- holds, its confirmations, the address in the colour of the wallet's
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
        coinState FewLockWords coin,
        confsCell (jsonCoinConfirmations coin),
        ownAddressWord net (jsonCoinAddress coin),
        -- coins lists the active portfolio
        maybe mempty (trailingPathWord . ownPathText ownPathsAlone) (jsonCoinPath coin),
        tagsCell (jsonCoinAddressTags coin),
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
balanceTableColumns :: AmountUnit -> Network -> [AccountKind] -> Natural -> Natural -> [Column]
balanceTableColumns unit net kinds pad tagsWide =
  [ column "Path" pad LeftSide,
    column "Address" (addressWidth net kinds) LeftSide,
    column (amountHeading "Total" unit) amountWidth RightSide,
    column (amountHeading "Spendable" unit) amountWidth RightSide,
    column (amountHeading "Unspendable" unit) amountWidth RightSide,
    tagsColumn tagsWide,
    column "Label" 0 LeftSide
  ]

-- | An address on one row of the table of balances, under the columns
-- given: its path, the address, the value of its coins and what it can
-- spend, in green unless zero, the rest in yellow unless zero, then its
-- label, or that it is a change address
balanceRowDoc :: AmountUnit -> Network -> [Column] -> DBAddress -> CoinBalance -> [ShownTag] -> Doc Style
balanceRowDoc unit net headed addr balance tags =
  tableLine $
    zip
      headed
      [ text (cs (rowPathText net addr)),
        pretty (dBAddressAddress addr),
        heldAmountCell unit (coinBalanceTotal balance),
        heldAmountCell unit (coinBalanceSpendable balance),
        waitingAmountCell unit (coinBalanceUnspendable balance),
        tagsCell tags,
        labelCell
      ]
  where
    labelCell
      | not (Text.null (dBAddressLabel addr)) = labelWord (dBAddressLabel addr)
      | dBAddressInternal addr = changeWord
      | otherwise = mempty

{- The table of pending transactions -}

-- The table of pending transactions gives what the compact view gives:
-- the short id of the hash the other commands take first, where the
-- transaction is, the time it was created, its type, its amount, how far
-- it got, its fee rate, and what it is for, as the table of txs says it.
-- A replacement says first, in yellow, which transactions it replaces, as
-- the compact view does. Where it is takes a column for the wallet when
-- the database has several, then one for the portfolio and one for the
-- network. The listing is read whole ('listingWhole'), so these columns
-- are as wide as the longest name they show, as in the table of
-- portfolios. The hash in full would make a row too wide. The verbose
-- view gives it.

-- | The columns of the table of pending transactions: the short id first,
-- then the wallet when the listing names the wallets, the portfolio and
-- the network, each as wide as the widest the places given hold. The
-- wallet has a column of its own, as in the table of portfolios, so that
-- the eye runs down the wallets apart from the portfolios.
pendingTableColumns :: AmountUnit -> WalletNaming -> Natural -> [Place] -> [Column]
pendingTableColumns unit naming tagsWide places =
  [column "Hash" shortIdWidth LeftSide]
    <> [column "Wallet" (widestCell (cs . walletName . refWallet . placePortfolio) places) LeftSide | naming == WithWallets]
    <> [ column "Portfolio" (widestCell (cs . portfolioName . refPortfolio . placePortfolio) places) LeftSide,
         column "Network" (widestCell (\place -> (placeNetwork place).name) places) LeftSide
       ]
    <> [ column "Time" minuteWidth LeftSide,
         column "Type" typeWidth LeftSide,
         column (amountHeading "Amount" unit) amountWidth RightSide,
         column "Status" pendingStatusWidth LeftSide,
         -- The fee rate has a few digits: the heading sets the width
         column "sat/vB" 1 RightSide,
         tagsColumn tagsWide,
         column "Description" 0 LeftSide
       ]

-- | The width of the column of the statuses of pending transactions: that
-- of "not signed" and "conflicted", the longest
pendingStatusWidth :: Natural
pendingStatusWidth = 10

-- | A pending transaction on one row of its table, under the columns
-- given: its short id, its wallet when the listing names the wallets,
-- its portfolio and its network, the time it was created,
-- its type, its amount, how far it got, its fee rate, and what it is for
-- ('purposeCell'), after the transactions it replaces, if any
-- ('replacesNote'). One in conflict can no longer be sent: its fee rate no
-- longer matters, and its cell is empty. The paths are written as given
-- ('listedPaths'): those of a transaction of another portfolio than the
-- active one name their portfolio.
pendingRowDoc :: AmountUnit -> WalletNaming -> PathNaming -> [Column] -> Place -> Maybe ZonedTime -> TxInfo -> Doc Style
pendingRowDoc unit naming paths headed place time info@(TxInfo summary status _ _ _ _ _) =
  tableLine . zip headed $
    [hashWord status]
      <> [pretty (walletName (refWallet ref)) | naming == WithWallets]
      <> [ pretty (portfolioName (refPortfolio ref)) `withStyle` Account,
           text net.name
         ]
      <> [ timeCell time,
           text (typeWord (summaryType summary)),
           signedAmountCell unit (summaryAmount summary),
           statusCell,
           rateCell,
           tagsCell (txOwnTags (txInfoTags info)),
           descriptionCell
         ]
  where
    net = placeNetwork place
    ref = placePortfolio place
    -- pendingtxs lists the transactions in preparation only
    (statusCell, rateCell) = case status of
      Pending pending
        | pendingConflicted pending -> (pendingStatusWord pending, mempty)
        | otherwise -> (pendingStatusWord pending, text (paidRateText (summaryFeeRate summary)))
      OnChain _ -> (tableConfirmations status, mempty)
    -- The words of the compact view, all in one cell, a comma between
    -- them
    descriptionCell = hcat . intersperse ", " $ maybeToList (replacesNote status) <> purposeWords paths net info

-- | A transaction with every detail: its first line, then its fields. The
-- lines given come first: its account in a listing of every account, or its
-- network in an answer. Its label, if any, follows them. The balance after
-- it, if the listing gives one, follows its fee. Every address it spends
-- from or pays is on a line of its own, in full, with its value, and those
-- of the wallet with their paths, in columns. Below an address of the
-- wallet, its label, and for one it spends from, the labels its coins show,
-- if any ('addressRow'). A change address is marked as one, except under
-- Change, which says it. A path is written as the paths of the view are
-- (the type t'PathNaming'), and the path of an address of another portfolio
-- names that portfolio.
verboseTxDoc :: PathNaming -> Network -> AmountUnit -> [Doc Style] -> Maybe Integer -> Maybe ZonedTime -> TxInfo -> Doc Style
verboseTxDoc paths net unit named balanceM time info@(TxInfo summary status label _ _ _ _) =
  vcat $
    headlineDoc fullConfirmations unit time info
      : ( indent 2
            <$> named
              <> [field 9 "Label" <> labelWord label | not (Text.null label)]
              <> [field 9 "Tags" <> tagsCell tags | let tags = txOwnTags (txInfoTags info), not (null tags)]
              <> statusFields time summary status
              <> spendsField unit info
              <> [feeField unit summary]
              <> [field 9 "Balance" <> neutralAmount unit balance | Just balance <- [balanceM]]
              <> lockTimeField summary
              <> section "From" Nothing True from
              <> section "To" (Just Incoming) True to
              <> section "Change" (Just Incoming) False change
        )
  where
    from = spentFrom info
    (to, change) = paidTo info
    widths = addressColumns paths net unit (from <> to <> change)
    -- What the portfolio receives is green. What it spends, and what
    -- others receive, another portfolio among them, is neither in nor out
    -- of it.
    section name ownStyle marked rows =
      [field 9 name <> align (vcat (addressRow paths net unit widths ownStyle marked <$> rows)) | not (null rows)]

-- | The fields that say where a transaction stands: the hash to name it
-- by, in full and by its short id, its time to the second with its time
-- zone, and what its first line does not explain. The summary tells what
-- became of a dead transaction ('deadStatus').
statusFields :: Maybe ZonedTime -> TxSummary -> TxStatus -> [Doc Style]
statusFields time summary (OnChain status) =
  hashFields Nothing "Txid" (chainTxid status)
    <> [field 9 "Time" <> text (secondText t) | Just t <- [time]]
    <> [field 9 "Status" <> deadStatus summary status | chainDead status]
-- A transaction in preparation shows one hash, the one to name it by: its
-- nosigHash until it is signed, then its id, which the network will know.
statusFields time _ (Pending status) =
  hashFields (Just PendingHash) (if pendingSigned status then "Txid" else "NoSigHash") (pendingShownHash status)
    -- Yellow, as in the compact view ('replacesNote')
    <> [ field 9 "Replaces" <> align (vcat [text (cs $ txHashToHex replaced) `withStyle` Waiting | replaced <- pendingReplaces status])
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

-- | What became of a dead transaction, as its Status field says it.
--
-- One that live transactions replaced names them, each in full on a line
-- of its own under the words "replaced by" ('chainReplacedBy'). The
-- verbose view gives every id in full, and one line for the words and an
-- id would pass 80 columns.
--
-- One that names none was dropped from the network, when the portfolio
-- made it ('ownTransaction'): only a transaction that spends one of its
-- coins could replace it, and the portfolio would hold that transaction.
-- Any other, a payment received for instance, was replaced or dropped,
-- and the portfolio cannot tell which. Its payer can replace it with a
-- transaction that pays the portfolio nothing, which the portfolio never
-- sees.
deadStatus :: TxSummary -> ChainStatus -> Doc Style
deadStatus summary status
  | not (null replacements) = align (vcat ("replaced by" : [text (cs (txHashToHex replacement)) | replacement <- replacements]))
  | ownTransaction summary = "dropped from the network"
  | otherwise = "replaced, or dropped from the network"
  where
    replacements = chainReplacedBy status

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

-- | What a transaction of the portfolio that is in preparation spends of
-- coins that are not the portfolio's ('spentOfOthers'), if anything: all
-- that a sweep into the wallet spends. The first line gives what the
-- portfolio gains or loses, which says nothing of those coins, and they
-- may be the user's too, with keys kept elsewhere. The user who signs
-- must see the total, to check it against the addresses paid.
--
-- An external transaction has no such field: its first line gives that
-- total already ('headlineDoc'). Nor has one on the chain, where the coins
-- of others are those of the payer ('txInPreparation').
spendsField :: AmountUnit -> TxInfo -> [Doc Style]
spendsField unit info =
  [ field 9 "Spends" <> neutralAmount unit (toInteger spent) <+> "of coins that are not this portfolio's"
  | txInPreparation info,
    summaryType summary /= TxExternal,
    spent > 0
  ]
  where
    summary = txInfoSummary info
    spent = spentOfOthers summary

-- | The fee and its rate. Below them, the change the fee took in, too
-- small for an output: that change comes neither in nor out.
feeField :: AmountUnit -> TxSummary -> Doc Style
feeField unit summary =
  field 9 "Fee"
    <> align
      ( vcat $
          [neutralAmount unit (fromIntegral (summaryFee summary)) <> "," <+> paidRateDoc (summaryFeeRate summary)]
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
    -- spends from in a transaction on the chain: that wallet's coins are
    -- nothing to this one.
    txAddressValue :: !(Maybe Natural),
    -- | Where an address of the wallet is, and its label: one of the
    -- portfolio of the view, or of another portfolio of the database
    txAddressHeld :: !(Maybe HeldAddress),
    -- | The labels the coins spent from an address of the portfolio show,
    -- each once. None for an address paid.
    txAddressCoinLabels :: ![Text],
    -- | The tags of an address of the portfolio of the view, all of
    -- them: the verbose view of a transaction shows them below the
    -- address, after its label
    txAddressShownTags :: ![ShownTag],
    -- | The contact of the address book the address is, for an address
    -- that is not of the wallet: its label and its tags
    txAddressContact :: !(Maybe Contact)
  }

-- | The addresses a transaction spends from: those of the portfolio with
-- what they spend and the labels their coins show, then those of others.
-- Among these, an address of another portfolio of the database comes with
-- what it spends, since those coins are the user's too. An address of
-- another wallet comes without in a transaction on the chain, where its
-- coins are the payer's, and with what it spends in one in preparation
-- ('txInPreparation'): the user may be about to sign it, as a sweep,
-- or to send it, and checks what each address gives.
spentFrom :: TxInfo -> [TxAddress]
spentFrom info =
  [TxAddress a (Just v) (Just (HeldAddress p l Nothing)) (coinLabelsOf outpoints) (tagsAt info a) Nothing | (a, MyInputs v p l outpoints) <- Map.assocs (summaryMyInputs summary)]
    <> [fromOthers a v | (a, v) <- Map.assocs (summaryOtherInputs summary)]
  where
    summary = txInfoSummary info
    coinLabelsOf outpoints = nub [label | outpoint <- outpoints, Just label <- [Map.lookup outpoint.hash (txInfoInputLabels info)]]
    fromOthers a v = case Map.lookup a (txInfoElsewhere info) of
      Just held -> TxAddress a (Just v) (Just (heldElsewhereAddress held)) [] [] Nothing
      Nothing -> TxAddress a (if txInPreparation info then Just v else Nothing) Nothing [] [] (contactAt info a)

-- | The contact of the address book an address of others in a
-- transaction is, if it is one
contactAt :: TxInfo -> Address -> Maybe Contact
contactAt info addr = Map.lookup addr (txInfoContacts info)

-- | The tags of an address of the portfolio in a transaction, all of
-- them, or none for an address without tags
tagsAt :: TxInfo -> Address -> [ShownTag]
tagsAt info addr = Map.findWithDefault [] addr (txAddressTags (txInfoTags info))

-- | The addresses a transaction pays, and the change it sends back to the
-- portfolio. What a payment sends back to the portfolio is its change. In
-- a payment the portfolio receives, the outputs to others are the change
-- of the payer, left out once it is on the chain. Between the addresses
-- of the portfolio, no output can be told to be the change. An address of
-- another portfolio of the database is among those of others, since the
-- payment leaves the portfolio, and says where it is.
--
-- A payment received that is in preparation ('txInPreparation') lists the
-- outputs to others too, after those of the portfolio. hw cannot tell
-- the payment of another wallet from a sweep, whose coins are the user's:
-- one sweep may pay the portfolio a tenth of them and a stranger the
-- rest, and the user who signs it must see every address it pays. An
-- external transaction pays others only.
paidTo :: TxInfo -> ([TxAddress], [TxAddress])
paidTo info =
  case summaryType summary of
    TxDebit -> (toOthers, toWallet)
    TxCredit
      | txInPreparation info -> (toWallet <> toOthers, [])
      | otherwise -> (toWallet, [])
    TxInternal -> (toWallet <> toOthers, [])
    TxExternal -> (toOthers, [])
  where
    summary = txInfoSummary info
    toWallet = [TxAddress a (Just v) (Just (HeldAddress p l Nothing)) [] (tagsAt info a) Nothing | (a, MyOutputs v p l) <- Map.assocs (summaryMyOutputs summary)]
    -- An address another portfolio holds is of the wallet, whatever the
    -- address book says of it
    toOthers =
      [ TxAddress a (Just v) held [] [] (if isJust held then Nothing else contactAt info a)
        | (a, v) <- Map.assocs (summaryOtherOutputs summary),
          let held = heldElsewhereAddress <$> Map.lookup a (txInfoElsewhere info)
      ]

-- | The widths of the columns of the addresses of a transaction, each that
-- of its widest cell
data AddressColumns = AddressColumns
  { addressColumn :: !Int,
    valueColumn :: !Int,
    pathColumn :: !Int
  }

-- | The widths of the columns for the addresses given, all those of one
-- transaction. The path of an address is measured as the view writes it,
-- with its portfolio when it names one ('heldPathText').
addressColumns :: PathNaming -> Network -> AmountUnit -> [TxAddress] -> AddressColumns
addressColumns paths net unit rows =
  AddressColumns
    { addressColumn = widest [addrToString net (txAddress row) | row <- rows],
      valueColumn = widest [valueText unit v | Just v <- txAddressValue <$> rows],
      pathColumn = widest [heldPathText paths held | Just held <- txAddressHeld <$> rows]
    }
  where
    widest = maximum . (0 :) . map shownWidth

-- | A value of an address, as its column shows it: "0.00050000 btc"
valueText :: AmountUnit -> Natural -> String
valueText unit v = cs (showAmount unit (fromIntegral v) <> " " <> showUnit unit (fromIntegral v))

-- | An address of a transaction: the address, its value right-aligned,
-- then for an address of the wallet, its path, as the paths of the view
-- are written ('heldPathText'). Below them, on a line that
-- starts under the address, an address of the wallet has its label, or
-- that it is a change address when marked, then the labels its coins
-- show, after the word "from". The line below keeps the columns of
-- addresses lined up, however long the labels. An address of the wallet
-- and its label are in its colour, of whichever portfolio it is. Its
-- value is in the style given when it is of the portfolio of the view.
-- The value of an address of another portfolio has no style, as that of
-- an address of another wallet: it is neither in nor out of the portfolio
-- the amount of the view is counted for. An address of the address book
-- is underlined, with the label and the tags of its contact on the line
-- below ('contactNoteBelow').
addressRow :: PathNaming -> Network -> AmountUnit -> AddressColumns -> Maybe Style -> Bool -> TxAddress -> Doc Style
addressRow paths net unit widths ownStyle marked row =
  withNotesBelow (cellsLine firstLine) notes
  where
    firstLine =
      [(addressColumn widths, address, addressDoc)]
        <> [(valueColumn widths, padding value <> value, valueDoc value) | Just v <- [txAddressValue row], let value = valueText unit v]
        <> [(pathColumn widths, path, trailingPathWord path) | Just held <- [txAddressHeld row], let path = heldPathText paths held]
    notes = case (txAddressHeld row, txAddressContact row) of
      (Just held, _) ->
        ownAddressNoteBelow held.heldLabel (marked && isIntPath (pathInAccount held.heldPath))
          -- The tags are one note: a space between them, as on every
          -- other line that shows tags
          <> [hsep (tagMarks tags) | let tags = txAddressShownTags row, not (null tags)]
          <> [styled Muted "from" <+> fillSep (punctuate "," (wrappingLabelWord <$> spentLabels)) | let spentLabels = txAddressCoinLabels row, not (null spentLabels)]
      -- An address of the address book: its label, then its tags
      (Nothing, Just contact) -> contactNoteBelow contact
      (Nothing, Nothing) -> []
    address = addrToString net (txAddress row)
    addressDoc
      | isJust (txAddressHeld row) = styled OwnAddress address
      | isJust (txAddressContact row) = styled BookAddress address
      | otherwise = text address
    style = case txAddressHeld row of
      Just held | isNothing held.heldIn -> ownStyle
      _ -> Nothing
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

-- | A transaction of a portfolio just prepared, as preparetx,
-- preparesendall and bumpfee answer with it: the transaction with every
-- detail ('txAnswerDoc'), then the coins it leaves in the portfolio, if
-- any, below it and apart from it ('leftCoinsDoc'). send and sendall show
-- the same before they ask for the mnemonic, so that the user reads there
-- what preparetx or preparesendall would have answered. The last argument
-- is the active portfolio, which may be another one than that of the
-- transaction.
preparedDoc :: WalletNaming -> AmountUnit -> Place -> TxInfo -> [LeftCoin] -> Maybe PortfolioRef -> IO (Doc Style)
preparedDoc naming unit place info left activeM = do
  let seen = SeenFrom (placePortfolio place) activeM
      net = placeNetwork place
  txDoc <- txAnswerDoc naming net (Just seen) unit info
  leftDocs <-
    if null left
      then return []
      else (\leftDoc -> [mempty, leftDoc]) <$> leftCoinsDoc (pathsSeenFrom naming seen) net unit left
  return (vcat (txDoc : leftDocs))

-- | A sweep just prepared, as preparesweep answers with it: the
-- transaction with every detail ('txAnswerDoc'), as its portfolio sees it
-- when it pays one, then what the sweep leaves out, if anything, below it
-- and apart from it: the addresses given to sweep from that hold no coin,
-- the coins that hold tokens, and the coins worth less than their fee
-- ('unsweptDoc'). The user gave them to be swept: without a word, a sweep
-- that left one out reads as one that swept it.
sweepDoc :: WalletNaming -> AmountUnit -> Maybe SeenFrom -> Network -> TxInfo -> [Address] -> [UnsweptCoin] -> IO (Doc Style)
sweepDoc naming unit seenM net info empty unswept = do
  txDoc <- txAnswerDoc naming net seenM unit info
  return . vcat . intersperse mempty $
    txDoc
      : [emptyNote | not (null empty)]
        <> [unsweptDoc net unit reason left | reason <- [HoldsTokens, WorthLessThanItsFee], let left = filter ((== reason) . (.reason)) unswept, not (null left)]
  where
    emptyNote =
      vcat $
        text (counted (fromIntegral (length empty)) "address given holds" "addresses given hold" <> " no coin and " <> isOrAre <> " not in the sweep:")
          : [indent 2 (text (addrToString net addr)) | addr <- empty]
    isOrAre = if length empty == 1 then "is" else "are" :: String

-- | The coins a sweep leaves at their addresses for a reason, as
-- preparesweep names them below the sweep: each as 'sweptCoinDoc' shows
-- it. The coins that hold tokens end with what to do with them, since no
-- command of hw moves them.
unsweptDoc :: Network -> AmountUnit -> UnsweptReason -> [UnsweptCoin] -> Doc Style
unsweptDoc net unit reason unswept =
  vcat $
    text (counted (fromIntegral (length unswept)) one many <> " not in the sweep:")
      : [indent 2 (sweptCoinDoc net unit left) | left <- unswept]
        <> [sentence (wordsOf "hw does not handle tokens and never spends such a coin. Move it with a wallet that handles the tokens of Bitcoin Cash.") | reason == HoldsTokens]
  where
    (one, many) = case reason of
      WorthLessThanItsFee -> ("coin is worth less than its fee and is", "coins are worth less than their fee and are") :: (String, String)
      HoldsTokens -> ("coin holds tokens (CashTokens) and is", "coins hold tokens (CashTokens) and are")

-- | A coin from outside the wallet in a report of preparesweep: its
-- outpoint with the txid in full, as --coin of preparesweep takes it, then
-- its address and its amount on the line below, as a From line of the
-- sweep gives them. The wallet does not hold the coin, so it has no short
-- id, no path and no time to show, as a coin of the wallet has
-- ('reportCoinDoc').
sweptCoinDoc :: Network -> AmountUnit -> UnsweptCoin -> Doc Style
sweptCoinDoc net unit left =
  vcat
    [ text (coinText left.outpoint),
      indent 2 ("at" <+> text (addrToString net left.address) <> "  " <> neutralAmount unit (fromIntegral left.value))
    ]

-- | The coins a transaction just prepared leaves in the portfolio on its
-- network, each with why it stays ('reportCoinDoc'), a blank line between
-- them. Their paths are written as those of the transaction above them
-- are.
leftCoinsDoc :: PathNaming -> Network -> AmountUnit -> [LeftCoin] -> IO (Doc Style)
leftCoinsDoc paths net unit left = do
  coins <- forM left $ \coin -> reportCoinDoc paths net unit [text (leftCoinWhy coin)] coin.leftCoin
  return . vcat . intersperse mempty $
    text (counted (fromIntegral (length left)) "coin stays" "coins stay" <> " in the portfolio:") : coins

-- | A coin in a report, before a question or after a transaction just
-- prepared: as the compact view of coins shows it, then what the report
-- says of it, a line each. A coin looks the same wherever hw shows it, and
-- the user tells it by its short id, its time, its value and its address,
-- as coins lists it. Its path names its portfolio when the report is on
-- another portfolio than the active one (the type t'PathNaming'), as a
-- replacement by bumpfee may be.
reportCoinDoc :: PathNaming -> Network -> AmountUnit -> [Doc Style] -> JsonCoin -> IO (Doc Style)
reportCoinDoc paths net unit said coin = do
  time <- traverse utcToLocalZonedTime (jsonCoinTime coin)
  return $ vcat (compactCoinDoc paths net unit time coin : (indent 2 <$> said))

{- The questions of the commands that prepare a transaction -}

-- The reports the commands show on stderr before they ask. A sentence
-- wraps at the width of the screen, as the help does. A coin is shown as
-- coins shows it, then what the report says of it ('reportCoinDoc'). The
-- amounts are in the unit asked for.

-- | A sentence of words and amounts, wrapped at the width of the screen
sentence :: [Doc Style] -> Doc Style
sentence = fillSep

-- | The words of a text, for a sentence
wordsOf :: String -> [Doc Style]
wordsOf = map pretty . words

-- | A fee rate that is asked for, in satoshis per virtual byte: a whole
-- number
rateDoc :: Natural -> Doc Style
rateDoc feeByte = text (show feeByte <> " sat/vB")

-- | The fee rate a transaction pays, in satoshis per virtual byte, with
-- the decimal of a rate that is not whole ('paidRateText')
paidRateDoc :: PaidRate -> Doc Style
paidRateDoc paid = text (paidRateText paid <> " sat/vB")

-- | The report on the coins worth less than their fee, as preparesendall
-- shows it before its question: the coins, each as coins shows it among
-- the views given, with the rates it is worth spending at, then what
-- spending them and leaving them does. preparesendall works in the active
-- portfolio, so the paths are alone ('ownPathsAlone').
dustReportDoc :: Network -> AmountUnit -> [JsonCoin] -> DustReport -> IO (Doc Style)
dustReportDoc net unit views report =
  dustReportWith unit report
    <$> sequence [reportCoinDoc ownPathsAlone net unit said view | (sized, said) <- dustCoinsSaid unit report, Just view <- [Map.lookup sized.coin.outpoint byOutpoint]]
  where
    byOutpoint = Map.fromList [(jsonCoinOutpoint view, view) | view <- views]

-- | What preparesweep says of the dust coins of a sweep, before it asks
-- whether to spend them too: the report of 'dustReportDoc', on coins the
-- wallet does not hold. Each is shown as the sweep names it
-- ('sweptCoinDoc').
sweepDustReportDoc :: Network -> AmountUnit -> DustReport -> Doc Style
sweepDustReportDoc net unit report =
  dustReportWith
    unit
    report
    [ vcat (sweptCoinDoc net unit left : (indent 2 <$> said))
      | (sized, said) <- dustCoinsSaid unit report,
        Just addr <- [sized.coin.address],
        let left = UnsweptCoin sized.coin.outpoint addr (fromIntegral sized.coin.value) WorthLessThanItsFee
    ]

-- | The dust coins of a report, each with what the report says of it: the
-- fee of its input when the dust coins do not all cost the same, since the
-- header then gives none, and the fee rates at which it is worth spending
dustCoinsSaid :: AmountUnit -> DustReport -> [(SizedCoin, [Doc Style])]
dustCoinsSaid unit report = [(sized, [hsep (ownFee sized <> [worthAt sized])]) | sized <- report.dustCoins]
  where
    severalFees = length (nub (inputFee report.rate <$> report.dustCoins)) > 1
    ownFee sized = ["costs" <+> neutralAmount unit (fromIntegral (inputFee report.rate sized)) <> "," | severalFees]
    worthAt sized = case worthSpendingUpTo sized of
      0 -> "worth less than its fee at any rate"
      upTo -> "worth spending at" <+> rateDoc upTo <+> "or less"

-- | The report on the dust coins around the coins as their command shows
-- them: how many they are and what each costs, the coins, then what
-- spending them and leaving them does, with what remains for the
-- recipients either way. Where the coins are (the type t'DustPlace') words
-- what the two choices leave behind.
dustReportWith :: AmountUnit -> DustReport -> [Doc Style] -> Doc Style
dustReportWith unit report coins =
  vcat . intersperse mempty $ [header] <> coins <> [vcat [spent, left]]
  where
    count = length report.dustCoins
    one = count == 1
    them = if one then "it" else "them"
    their = if one then "its address" else "their addresses"
    amount v = neutralAmount unit (fromIntegral v)
    -- The fee of an input depends on the kind of its coin. The header
    -- gives it when the dust coins all cost the same, and each coin
    -- gives its own otherwise.
    header = case nub (inputFee report.rate <$> report.dustCoins) of
      [fee]
        | one -> sentence $ wordsOf "1 coin costs more to spend than it is worth at" <> [rateDoc report.rate <> ",", amount fee <> ":"]
        | otherwise -> sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ",", amount fee] <> wordsOf "each:"
      _ -> sentence $ wordsOf (show count <> " coins cost more to spend than they are worth at") <> [rateDoc report.rate <> ":"]
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
          <> wordsOf ("less" <> emptied)
    -- A portfolio that spends its dust coins too may have no coin left
    emptied = case report.place of
      InPortfolio False -> ", and the portfolio has no coin left on the network."
      _ -> "."
    left = sentence $ kept <> wordsOf "Once the other coins are gone," <> later
    kept = case report.place of
      InPortfolio _ -> wordsOf ("Leaving " <> them <> ", the portfolio keeps") <> [amount dustValue <> "."]
      AtSweptAddresses -> wordsOf ("Leaving " <> them <> " keeps") <> [amount dustValue] <> wordsOf ("at " <> their <> ".")
    later = case report.laterRate of
      Just upTo
        | one -> wordsOf "it can only be spent at" <> [rateDoc upTo] <> wordsOf "or less."
        | otherwise -> wordsOf "they can only be spent together, at" <> [rateDoc upTo] <> wordsOf "or less."
      Nothing -> wordsOf ((if one then "it" else "they") <> " can never be spent: any fee would leave less than") <> [amount report.dustLimit <> "."]

-- | What a command says of a transaction that joins groups of coins,
-- before it asks whether to go on ("Haskoin.Wallet.Tx.Mixing"): each
-- group on a line, with its tags, how many of its coins the transaction
-- spends and what they are worth, then why it matters. The coins at
-- addresses without a tag are a group too, named "no tag". The values are
-- right-aligned, so that their points line up.
--
-- The groups are told and not the coins: the transaction is shown after
-- the question, with the tags of each address it spends from under it.
mixingDoc :: AmountUnit -> [SpentGroup] -> Doc Style
mixingDoc unit groups =
  vcat $
    [sentence (wordsOf "The transaction spends coins that do not all have the same tags:")]
      <> [indent 2 (cellsLine (cells spent)) | spent <- groups]
      <> [sentence (wordsOf "A transaction shows that its coins have one owner, so it joins these groups.")]
  where
    cells spent =
      [ (widest groupName, groupName spent, nameDoc spent),
        (widest groupCount, groupCount spent, text (groupCount spent)),
        (widest groupWorth, worth, text worth)
      ]
      where
        worth = rblock (fromIntegral (widest groupWorth)) (groupWorth spent)
    -- The text of the tags is what 'tagMarks' shows, without its colours
    groupName spent
      | null spent.spentGroupTags = "no tag"
      | otherwise = unwords ["[" <> cs (tagText (shownTag shown)) <> "]" | shown <- spent.spentGroupTags]
    nameDoc spent
      | null spent.spentGroupTags = styled Muted "no tag"
      | otherwise = hsep (tagMarks spent.spentGroupTags)
    groupCount spent = counted (genericLength spent.spentGroupCoins) "coin" "coins"
    groupWorth spent = valueText unit spent.spentGroupValue
    widest :: (SpentGroup -> String) -> Int
    widest cell = maximum (0 : map (shownWidth . cell) groups)

-- | What bumpfee says of the transactions that spend the change of the one
-- to replace, before it asks to replace them all: their short ids, as txs
-- shows them
childrenDoc :: [Sent] -> Doc Style
childrenDoc children =
  vcat $
    sentence (wordsOf "Replacing the transaction removes those of the portfolio that spend its change:")
      : [indent 2 (styled Muted (shortHashText (sentId child))) | child <- children]

-- | What bumpfee says of the coins to add, before it asks to add them: each
-- as coins shows it ('reportCoinDoc'). bumpfee replaces a transaction in
-- its own portfolio, which may be another one than the active one, and
-- the paths of its coins then name it.
addedCoinsDoc :: PathNaming -> Network -> AmountUnit -> Natural -> [JsonCoin] -> IO (Doc Style)
addedCoinsDoc paths net unit feeByte added = do
  coins <- mapM (reportCoinDoc paths net unit []) added
  return . vcat . intersperse mempty $ header : coins
  where
    them = if length added == 1 then "this coin" else "these coins"
    header = sentence $ wordsOf "The coins of the transaction cannot pay its fee at" <> [rateDoc feeByte <> ","] <> wordsOf ("and " <> them <> " of the portfolio would:")

-- | What bumpfee says of the ways the recipients can pay, those of
-- 'Haskoin.Wallet.Tx.BumpFee.reductions', before it asks which: each
-- recipient, numbered when there are several, with what it receives, then
-- what it would receive paying alone, and its share when all can pay, a
-- blank line between them
--
-- It starts with why the recipients are asked: the coins of the
-- transaction cannot pay, and no coin is added (@noCoinAdded@).
reducedDoc :: Network -> AmountUnit -> Natural -> Maybe HeldBack -> [Reduction] -> Doc Style
reducedDoc _ _ _ _ [] = mempty
reducedDoc net unit feeByte held ways@(way : _) =
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
          <> endedWith "." (noCoinAdded unit held)
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

-- | The words that say that bumpfee adds no coin of the portfolio, with
-- what the portfolio holds and cannot spend now when it holds some. The
-- user is about to have the recipients pay, or --yes did it already, and
-- "no coin is added" alone hid that the portfolio had coins: under a tag
-- that locks them, locked with lockcoin, or without a block yet. coins
-- says which, and unlocking them lets the sender pay the fee.
noCoinAdded :: AmountUnit -> Maybe HeldBack -> [Doc Style]
noCoinAdded _ Nothing = wordsOf "and no coin of the portfolio is added"
noCoinAdded unit (Just held) =
  wordsOf "and no coin of the portfolio is added: it holds"
    <> [neutralAmount unit (fromIntegral held.heldBackValue)]
    <> wordsOf ("more, in " <> counted held.heldBackCoins "coin" "coins" <> " it cannot spend now, and hw coins says why")

-- | The words of a sentence, the last one followed by a sign
endedWith :: Doc Style -> [Doc Style] -> [Doc Style]
endedWith sign said = case reverse said of
  [] -> []
  lastWord : before -> reverse ((lastWord <> sign) : before)

-- | What bumpfee says in place of 'reducedDoc' when all the recipients pay
-- in equal shares without a question. The payer tells why. When the
-- sender pays, --yes took the shares, since neither the change nor the
-- coins of the portfolio could pay what is missing. When the recipients
-- pay, --recipients-pay asked them to pay what the higher fee adds
-- ('Haskoin.Wallet.Tx.BumpFee.sharedReduction'). Nothing is asked, so the
-- recipients are not numbered, and no other way is shown. Each recipient
-- comes with what it receives, then what it receives paying its share, a
-- blank line between them. A lone recipient pays it all.
sharedDoc :: FeePayer -> Network -> AmountUnit -> Natural -> Maybe HeldBack -> Reduction -> Doc Style
sharedDoc payer net unit feeByte held way =
  vcat . intersperse mempty $ header : rows
  where
    amount v = neutralAmount unit (fromIntegral v)
    paid = amount way.reductionMissing
    several = length way.reductionRecipients > 1
    who
      | several = wordsOf "the recipients pay the"
      | otherwise = wordsOf "the recipient pays the"
    shares
      | several = wordsOf "in equal shares, out of what they receive:"
      | otherwise = wordsOf "out of what it receives:"
    header = sentence $ case payer of
      SenderPays ->
        wordsOf "The coins of the transaction cannot pay its fee at"
          <> [rateDoc feeByte <> ","]
          <> endedWith "." (noCoinAdded unit held)
          <> wordsOf "With --yes,"
          <> who
          <> [paid, "missing"]
          <> shares
      RecipientsPay ->
        wordsOf "As --recipients-pay asks,"
          <> who
          <> [paid]
          <> wordsOf "the higher fee adds at"
          <> [rateDoc feeByte <> ","]
          <> shares
    paying = if several then "paying its share" else "paying it"
    rows =
      [ vcat [indent 2 (columns [text (addrToString net addr), amount before]), indent 4 (amount after <+> paying)]
        | (addr, before, after) <- way.reductionRecipients
      ]

-- | Print the answer of a command. A listing shows each element in the
-- view given, and an address that goes with a code shows it on a terminal.
-- The words of a new secret come in the layout given. A portfolio is
-- named with its wallet or not, as given: the answer does not say it,
-- since it follows from the wallets of the database ('WalletNaming').
prettyPrinter :: AmountUnit -> View -> WalletNaming -> AddressCode -> TxShown -> WordsLayout -> Response -> IO ()
prettyPrinter unit view naming code shown layout res = do
  responsePrinter unit view naming code shown layout res
  when (responseFailed res) exitFailure

-- | Print the answer of a command, as 'prettyPrinter' does, without the
-- exit status of a failure
responsePrinter :: AmountUnit -> View -> WalletNaming -> AddressCode -> TxShown -> WordsLayout -> Response -> IO ()
responsePrinter unit view naming code shown layout answer =
  case answer of
    -- A transaction just signed, shown in full before the secret was asked
    ResponseTx _ info _ | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    ResponseOutsideTx _ info | SignedTx fileM <- shown -> renderIO $ signedDoc fileM info
    -- A transaction send, sendall or sweep just sent, shown in full
    -- before the secret too
    ResponseSentPending _ info after _ | SentTx <- shown -> renderIO $ withSyncAfter after (sentDoc info)
    -- The transaction broadcast sent, as the wallet held it
    ResponseSentPending place info after activeM -> do
      shownTx <- txAnswerDoc naming (placeNetwork place) (Just (SeenFrom (placePortfolio place) activeM)) unit info
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
    ResponsePortfolio portfolio -> renderIO =<< listedPortfolioDoc Compact unit portfolio
    -- The outcome first, then the portfolio as portfolios shows it
    ResponseSwitched portfolio -> do
      portfolioDoc <- listedPortfolioDoc Compact unit portfolio
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
      renderIO =<< addressAnswerDoc naming code unit activeM ref addr funds tags followedM
    ResponseTags uses -> renderIO (tagUsesDoc uses)
    ResponseTagRenamed old use -> renderIO (tagRenamedDoc old use)
    ResponseTagMerged old use -> renderIO (tagMergedDoc old use)
    ResponseTagDeleted tag had -> renderIO (tagDeletedDoc tag had)
    ResponseContact net contact -> renderIO =<< contactAnswerDoc net contact
    ResponseContactDeleted _ contact -> renderIO (contactDeletedDoc contact)
    ResponseDerivedAddr ref path addr -> renderIO $ derivedAddrDoc naming ref path addr
    ResponseCoin ref coin activeM -> renderIO =<< coinAnswerDoc naming unit activeM ref coin
    ResponseTx place txInfo activeM ->
      renderIO =<< txAnswerDoc naming (placeNetwork place) (Just (SeenFrom (placePortfolio place) activeM)) unit txInfo
    -- Each portfolio's view of the transaction, a blank line between them
    ResponseLabelledTxs held activeM -> renderIO =<< heldTxsDoc naming unit activeM held
    ResponseHeldTxs held activeM -> renderIO =<< heldTxsDoc naming unit activeM held
    ResponsePrepared place txInfo left activeM -> renderIO =<< preparedDoc naming unit place txInfo left activeM
    ResponseSweep placeM net txInfo empty unswept activeM ->
      renderIO =<< sweepDoc naming unit (flip SeenFrom activeM . placePortfolio <$> placeM) net txInfo empty unswept
    ResponseOutsideTx net txInfo -> renderIO =<< txAnswerDoc naming net Nothing unit txInfo
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
        [syncResultsPrinter naming unit results | not (null results)]
          <> [notServedDoc notServed | not (null notServed)]
          <> [failedNetworksDoc "not synced" notSynced | not (null notSynced)]
    ResponseRecover searched gap walletM created results elsewhere notServed notSearched activeM hintM usedOnHidden ->
      renderIO . vcat . intersperse mempty $
        [vcat (recoveredDoc searched gap walletM created : [activeDoc active | Just active <- [activeM], not (null created)] <> [text (cs hint) | Just hint <- [hintM]])]
          <> [syncResultsPrinter naming unit results | not (null results)]
          <> [usedOnHiddenDoc naming usedOnHidden | not (null usedOnHidden)]
          <> [foreignFundsDoc unit elsewhere | not (null elsewhere)]
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
    listing = mapM_ (renderIO . listingDoc) =<< sectionsPrinter unit view naming (TagWidths 0 0 0) (severalGroups answer) answer
    withSyncAfter after sent = vcat (intersperse mempty (sent : syncAfterDocs naming unit after))

-- | The secret newportfolio made, which it shows once, its words in the
-- layout given
secretDoc :: WordsLayout -> NewSecret -> Doc Style
secretDoc layout (NewMnemonic source ws) = mnemonicDoc layout source ws
secretDoc layout (NewShares source threshold shares) = sharesDoc layout source threshold shares

-- | Where the sheet is that newportfolio wrote for the secret it made:
-- a PDF file to print, with a line for each word. It is named right
-- below the words, where the user is about to look for paper. The
-- sentence says what to do with the file, since nothing was asked for it.
--
-- The file is on a line of its own, with nothing else on it, not even an
-- indent, as the master private key is: the two together are wider than
-- 80 columns, and a line selected whole on the terminal holds the path
-- alone, to give it to a program that prints.
secretSheetDoc :: NewSecret -> FilePath -> Doc Style
secretSheetDoc secret file = vcat [text (toPrint <> ":"), styled UserText file]
  where
    toPrint = case secret of
      NewMnemonic _ _ -> "A sheet to print, to write the mnemonic on"
      NewShares _ _ [_] -> "A sheet to print, to write the share on"
      NewShares {} -> "A sheet to print, with a page to write each share on"

-- | The master private key of a wallet, which --show-master-key asks
-- for: newportfolio shows it once, for the secret it made, and
-- checkmnemonic for a secret that matches. The key is on a line of its own,
-- with nothing else on it, not even an indent: a line selected whole on
-- the terminal holds the key alone, and a script takes the line that
-- starts with xprv. The key is one text of 111 characters, which the
-- layout never breaks at 80 columns.
masterKeyDoc :: Text -> Doc Style
masterKeyDoc key = vcat ["Master private key", styled Secret (cs key)]

-- | The portfolio newportfolio created: its name and its wallet, the
-- fingerprint of the wallet, which names the mnemonic from then on, and
-- the accounts it has ('newAccountsDoc'), then which portfolio is active
-- ('activeDoc')
newPortfolioDoc :: PortfolioView -> ActiveAfter -> Doc Style
newPortfolioDoc portfolio active =
  vcat $
    (styled Good "Created" <+> (pretty (portfolioName (refPortfolio ref)) `withStyle` Account) <+> "of" <+> pretty (walletName (refWallet ref)))
      : ( indent 2
            <$> [ field 11 "Fingerprint" <> pretty (fingerprintToText (viewFingerprint portfolio)),
                  field 11 "Accounts" <> newAccountsDoc (viewNetworks portfolio)
                ]
        )
      <> [activeDoc active]
  where
    ref = viewRef portfolio

-- | The accounts of a new portfolio on the networks the view shows: how
-- many they are, then each network with the types of its accounts, as in
-- "3, on btc (bip44 and bip84) and bch (bip44)".
--
-- The types are there so that the count adds up on the line. Bitcoin Cash
-- has bip44 accounts only, so the count is not twice the networks named,
-- and a count beside the networks alone looked wrong. A hidden network is
-- neither named nor counted, as in every view, although the portfolio
-- has its accounts.
--
-- With every network shown, the value is wider than 80 columns. It wraps
-- under its first line, between two networks, never inside the types of
-- one ('andListWords').
newAccountsDoc :: [NetworkView] -> Doc Style
newAccountsDoc networks = align (sentence (text (show accounts <> ", on") : andListWords (networkWithTypes <$> networks)))
  where
    accounts = sum (length . networkViewAccounts <$> networks)
    -- "btc (bip44 and bip84)", bip44 first
    networkWithTypes net = (networkViewNetwork net).name <> " (" <> andList (cs . kindName <$> sort (keysKind <$> networkViewAccounts net)) <> ")"

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

-- | The portfolios recover found used on hidden networks, a line for
-- each with those networks, under the syncs, which leave the hidden
-- networks out. A portfolio that was paid on a hidden network alone
-- would otherwise be recovered with nothing to show for it. The command
-- that shows the networks is the next step ('showNetworksStep').
usedOnHiddenDoc :: WalletNaming -> [(PortfolioRef, [Network])] -> Doc Style
usedOnHiddenDoc naming used =
  vcat
    [ (text (portfolioWords naming ref) `withStyle` Account) <> "  " <> text ("used on " <> andList ((.name) <$> nets) <> hidden nets)
      | (ref, nets) <- used
    ]
  where
    hidden [_] = ", which is hidden"
    hidden _ = ", which are hidden"

-- | A transaction as each portfolio that holds it sees it, in full, a
-- blank line between the portfolios: what label and show answer. Each
-- view that is not of the active portfolio given says so. Each portfolio
-- is named with its wallet or not, as given, the same in every view: a
-- payment between two wallets shows in a view of each.
heldTxsDoc :: WalletNaming -> AmountUnit -> Maybe PortfolioRef -> [(Place, TxInfo)] -> IO (Doc Style)
heldTxsDoc naming unit activeM held =
  vcat . intersperse mempty <$> mapM viewOf held
  where
    viewOf (place, txInfo) = txAnswerDoc naming (placeNetwork place) (Just (SeenFrom (placePortfolio place) activeM)) unit txInfo

-- | A transaction just signed, as signtx and signsweep answer on the
-- screen: its id, and the file it was written to, if any. The transaction
-- itself was shown before the secret was asked for.
signedDoc :: Maybe FilePath -> TxInfo -> Doc Style
signedDoc fileM info =
  vcat $
    [doneWithTxid "Signed" hash | Just hash <- [txInfoHash info]]
      <> ["Written to" <+> styled UserText file | Just file <- [fileM]]

-- | What follows the answer of a sending, each part after a blank line:
-- the portfolios the transaction touches as the sync left them
-- ('syncedAfterDoc'), then why the sync did not go through, when it did
-- not. Nothing for a transaction that touches no portfolio.
--
-- The failure starts with what matters most to a user who reads the word
-- \"failed\" below a payment: the transaction is sent.
syncAfterDocs :: WalletNaming -> AmountUnit -> SyncAfter -> [Doc Style]
syncAfterDocs naming unit after =
  (syncedAfterDoc naming unit <$> after.syncedAfter)
    <> [wrappedAfter (text sent) sent failure | Just failure <- [after.notSyncedAfter]]
  where
    sent = "The transaction is sent, and the wallet could not sync after it:"

-- | What send and sendall answer with on the screen: that the transaction
-- was sent, and its txid. The command showed the transaction in full
-- before it asked for the mnemonic, and nothing of it changed since, so
-- showing it again would only push the txid off the screen.
sentDoc :: TxInfo -> Doc Style
sentDoc info = vcat [doneWithTxid "Sent" hash | Just hash <- [txInfoHash info]]

-- | What a command did to a transaction, in a word, then its txid:
-- "Signed, txid 3f2a...", as signtx, send and sendall answer once they
-- showed the transaction in full
doneWithTxid :: String -> TxHash -> Doc Style
doneWithTxid done hash = styled Good done <> "," <+> "txid" <+> styled PendingHash (cs (txHashToHex hash))

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
-- send, sendall, preparetx, preparesendall, preparesweep, signtx,
-- signsweep, broadcast, importwallet or sync, or after a command that
-- created a portfolio that is not the active one, below its answer. In the pretty output only: a
-- script reading the JSON answer knows its next step
-- ('nextStepsPrinter').
printNextSteps :: Command -> Response -> IO ()
printNextSteps cmd res =
  forM_ (nextStepsPrinter cmd res) $ \steps -> putStrLn "" >> renderIO steps

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
  ResponseContacts groups -> length groups > 1
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
-- have fixed widths for the same reason, and its column of tags the width
-- given, that of the widest tags of the database. The portfolios, addresses,
-- transactions and coins show in the view given, their times in the time
-- zone of the computer.
--
-- With the flag, each section says where it is in a line above its rows:
-- its portfolio, with its wallet or not as given, and its network, and
-- its account for the addresses. The
-- callers set it for a listing that may cover several places. The
-- sections of balances always have that line, which gives the total of
-- the network: the specification of the listing asks for a total for
-- each network, and a listing narrowed to one network has one too.
sectionsPrinter :: AmountUnit -> View -> WalletNaming -> TagWidths -> Bool -> Response -> IO (Maybe [Rows])
sectionsPrinter unit view naming tagsWide titled = \case
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
          pad = longestPathWidth (groupAccounts g) (nub [dBAddressInternal addr | (addr, _, _) <- addrs])
          -- addresses lists the addresses of one chain
          internal = or [dBAddressInternal addr | (addr, _, _) <- addrs]
          headed = addressTableColumns unit net (countsKind <$> groupAccounts g) pad tagsWide.addressTagsWidth internal
          title = [maybe (placeLine naming (groupPlace g)) (accountRefLine naming) (groupAccountRef g) | titled]
      if view == Table
        then return $ Rows none title (tableHeader headed) [addressRowDoc unit net headed addr funds tags | (addr, funds, tags) <- addrs] False
        else do
          docs <- mapM (\(addr, funds, tags) -> listedAddressDoc view net unit pad addr funds tags) addrs
          return $ Rows none title [] docs True
  ResponseBalances groups ->
    sections (text "There are no addresses with coins to display") groups $ \none (g, total) -> onceSynced none g $ do
      let net = placeNetwork (groupPlace g)
          addrs = groupRows g
          pad = longestPathWidth (groupAccounts g) [False, True]
          headed = balanceTableColumns unit net (countsKind <$> groupAccounts g) pad tagsWide.addressTagsWidth
          title = [balanceTitle naming unit (groupPlace g) total]
      return $
        if view == Table
          then Rows none title (tableHeader headed) [balanceRowDoc unit net headed addr balance tags | (addr, balance, tags) <- addrs] False
          else Rows none title [] [listedBalanceDoc view net unit pad addr balance tags | (addr, balance, tags) <- addrs] True
  ResponseTxs groups ->
    sections (text "There are no transactions to display") groups $ \none g -> onceSynced none g $ do
      let net = placeNetwork (groupPlace g)
          title = [placeLine naming (groupPlace g) | titled]
          headed = txColumns unit tagsWide.txTagsWidth
          -- txs lists the active portfolio
          paths = activePaths naming
      if view == Table
        then do
          rows <- mapM (\(ListedTx info balanceM) -> txRowDoc paths net unit headed balanceM <$> localTxTime info <*> pure info) (groupRows g)
          return $ Rows none title (tableHeader headed) rows False
        else do
          docs <- mapM (\(ListedTx info balanceM) -> listedTxDoc paths view net unit Nothing balanceM info) (groupRows g)
          return $ Rows none title [] docs True
  -- The listing covers every portfolio of every wallet, so each
  -- transaction says where it is, with its wallet when the database has
  -- several, and the paths of one of another portfolio than the active
  -- one name that portfolio
  ResponsePendingTxs activeM held -> do
    let none = text "There are no pending transactions"
        -- The listing is read whole: these are all its places
        headed = pendingTableColumns unit naming tagsWide.txTagsWidth (fst <$> held)
        pathsAt place = listedPaths naming activeM (placePortfolio place)
    if view == Table
      then do
        rows <- mapM (\(place, info) -> pendingRowDoc unit naming (pathsAt place) headed place <$> localTxTime info <*> pure info) held
        return $ Just [Rows none [] (tableHeader headed) rows False]
      else do
        docs <- mapM (\(place, info) -> listedTxDoc (pathsAt place) view (placeNetwork place) unit (Just (placePortfolio place)) Nothing info) held
        return $ Just [apartRows none docs]
  ResponseCoins groups ->
    sections (text "There are no coins to display") groups $ \none g -> onceSynced none g $ do
      let net = placeNetwork (groupPlace g)
          coins = groupRows g
          headed = coinTableColumns unit net (countsKind <$> groupAccounts g) (longestPathWidth (groupAccounts g) [False, True]) tagsWide.addressTagsWidth
          title = [placeLine naming (groupPlace g) | titled]
      if view == Table
        then do
          rows <- mapM (\coin -> coinRowDoc net unit headed <$> traverse utcToLocalZonedTime (jsonCoinTime coin) <*> pure coin) coins
          return $ Rows none title (tableHeader headed) rows False
        else do
          docs <- mapM (listedCoinDoc view net unit) coins
          return $ Rows none title [] docs True
  -- The contacts of each network are read whole, so the columns of a
  -- table fit the widest cell of its section
  ResponseContacts groups ->
    sections noContacts groups $ \none (net, contacts) -> do
      let headed = contactTableColumns tagsWide.contactTagsWidth contacts
          title = [text net.name | titled]
      if view == Table
        then return $ Rows none title (tableHeader headed) (contactRowDoc headed <$> contacts) False
        else do
          docs <- mapM (listedContactDoc view) contacts
          return $ Rows none title [] docs True
  _ -> return Nothing
  where
    -- A section for each group, or one without rows for no group
    sections :: Doc Style -> [group] -> (Doc Style -> group -> IO Rows) -> IO (Maybe [Rows])
    sections none [] _ = return $ Just [apartRows none []]
    sections none groups rowsOf = Just <$> mapM (rowsOf none) groups
    -- The section of a group of balances, coins or txs, as given. A
    -- group without a row on a network that never synced has the line
    -- that says so instead ('notSyncedLine'), whether the sections say
    -- where they are or not. The line stands as its one row, since a
    -- section without rows shows nothing: no title and no header of a
    -- table come with it, with no row under them.
    onceSynced :: Doc Style -> Group row -> IO Rows -> IO Rows
    onceSynced none g rows
      | groupSynced g || not (null (groupRows g)) = rows
      | otherwise = return (Rows none [] [] [notSyncedLine naming (groupPlace g)] False)

{- Tags -}

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

-- | The first line of a listing that keeps the rows with some tags: what
-- it lists, and the tags it keeps, each in its colour, as in "Addresses
-- tagged kyc or cold". A listing of transactions may keep them by their
-- own tags, by the tags of their addresses, or by both: "Transactions
-- tagged rent, or at addresses tagged kyc". A listing that keeps the rows
-- without a tag says so the same way: "Addresses without a tag", or
-- after the tags it keeps too, "Addresses tagged kyc, or without a tag"
-- ('keptTagsWords'). A blank line follows, which sets the line apart
-- from the listing.
keptTagsDoc :: KeptTags -> Doc Style
keptTagsDoc kept = vcat [hsep (text kept.keptNoun : keptTagsWords text tagWord kept), mempty]

-- | The line above the addresses balances lists on a network: where they
-- are, then the value of every coin of the portfolio there, what it can
-- spend now, and the rest, which waits. The figures count every coin the
-- listing covers on the network, whatever part of them a page shows.
balanceTitle :: WalletNaming -> AmountUnit -> Place -> CoinBalance -> Doc Style
balanceTitle naming unit place total =
  columns
    [ placeLine naming place,
      commaList (balanceFigures unit (coinBalanceTotal total) (coinBalanceSpendable total))
    ]

-- | All that balances, coins and txs show of a network where the
-- portfolio has addresses and never synced: where it is, then that it is
-- not synced ('notSyncedWords'). hw asked no server what the addresses
-- hold there, and "There are no coins to display" would say that they
-- hold nothing.
notSyncedLine :: WalletNaming -> Place -> Doc Style
notSyncedLine naming place = columns [placeLine naming place, notSyncedWords]

-- | The width of the longest derivation path of the accounts on the
-- chains, False for the receiving one and True for the change one, with
-- the type of the account in front, as the views write it: that of the
-- newest address there
longestPathWidth :: [AccountCounts] -> [Bool] -> Natural
longestPathWidth accounts chains = fromIntegral $ maximum (0 : [width account internal | account <- accounts, internal <- chains])
  where
    width account internal =
      let count = if internal then countsInternal account else countsExternal account
          newest = PoolPath (countsKind account) (chainPath internal (fromIntegral (count - 1)))
       in if count == 0 then 0 else Text.length (poolPathText newest)

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

-- | A chunk of a listing, where the listing says it is (the type
-- t'ChunkAt'). The first chunk of a listing without rows says so. A chunk
-- that starts a section has the title of the section, if any, and the
-- header of its table, after a blank line that sets it apart from the
-- section before. A chunk that goes on in its section comes under the rows
-- before it, after the blank line that sets them apart if one does. An
-- answer that is not a listing is a failure here. A portfolio is named with
-- its wallet or not, as given, and the columns of tags of a table have
-- the widths given, both the same for every chunk of a listing.
chunkDoc :: AmountUnit -> View -> WalletNaming -> TagWidths -> ChunkAt -> Response -> IO (Doc Style)
chunkDoc unit view naming tagsWide at answer = do
  sectionsM <- sectionsPrinter unit view naming tagsWide (chunkTitled at) answer
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
-- ('switchStep'). A sync covers every portfolio, so the new
-- transactions it found may be in another portfolio than the active one,
-- which txs lists alone: the steps then start with the switch to that
-- portfolio ('txsSteps'). A pending transaction in conflict needs no
-- switch, since pendingtxs lists every portfolio ('conflictSteps').
nextStepsPrinter :: Command -> Response -> Maybe (Doc Style)
nextStepsPrinter cmd res =
  case (cmd, res) of
    (CommandNewPortfolio _, ResponseNewPortfolio _ _ view active _ _)
      | not active.activeAfterActivated -> stepsPrinter [switchStep (viewRef view)]
    (CommandRecover _, ResponseRecover _ _ walletM created _ _ _ _ activeM _ usedOnHidden) ->
      stepsPrinter $
        -- The active portfolio is none of those found paid: an older
        -- one, or the portfolio0 that a new wallet always gets
        [ switchStep (PortfolioRef wallet (minimum created))
          | Just wallet <- [walletM],
            Just active <- [activeM],
            not (null created),
            active.activeAfterRef `notElem` (PortfolioRef wallet <$> created)
        ]
          -- Each network once, in the order of the networks of a portfolio
          <> [showNetworksStep nets | nets@(_ : _) <- [filter (`elem` concatMap snd usedOnHidden) portfolioNetworks]]
    (CommandPrepareTx opts, ResponsePrepared _ info _ _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    (CommandPrepareSendAll opts, ResponsePrepared _ info _ _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    (CommandBumpFee opts, ResponsePrepared _ info _ _)
      | Just shown <- shownOf info -> afterPreparing opts.output shown
    -- The keys of a file at hand for the sweep are the ones to sign it with
    (CommandPrepareSweep opts, ResponseSweep (Just _) _ info _ _ _)
      | Just shown <- shownOf info ->
          stepsPrinter
            [ ("Next, sign it with the private keys of the coins", hwLine "signsweep" (pendingHashWord shown : keysArgs opts.keys)),
              case opts.output of
                Nothing -> ("Or export it, to sign it where the keys are", hwLine "exporttx" [pendingHashWord shown, metavarWord "FILE"])
                Just file -> ("Or sign its file where the keys are", hwLine "signsweep" [optionWord "--input", fileWord file])
            ]
    -- A sweep outside the wallet is in its file only
    (CommandPrepareSweep opts, ResponseSweep Nothing _ _ _ _ _)
      | Just file <- opts.output ->
          stepsPrinter [("Next, sign its file with the private keys of the coins", hwLine "signsweep" ([optionWord "--input", fileWord file] <> keysArgs opts.keys))]
    (CommandSignTx signing, ResponseTx _ info _) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseTx _ info _) -> afterSigning signing info
    (CommandSignSweep signing _ _, ResponseOutsideTx _ info) -> afterSigning signing info
    (CommandBroadcast {}, ResponseSentPending _ _ after _) -> afterSending after
    (CommandBroadcast {}, ResponseSentTx _ _ after) -> afterSending after
    -- A copy holds the history as its last sync left it
    (CommandRestoreDatabase {}, ResponseRestored _ restored _ _)
      | not (null (databaseShownWallets restored)) ->
          stepsPrinter [("Next, sync the wallet, to bring the history of the copy up to date", hwLine "sync" [])]
    (CommandSend {}, ResponseSentPending _ _ after _) -> afterSending after
    (CommandSendAll {}, ResponseSentPending _ _ after _) -> afterSending after
    -- A sweep that pays a portfolio. One that pays none leaves nothing to
    -- follow in the wallet.
    (CommandSweep {}, ResponseSentPending _ _ after _) -> afterSending after
    -- The file may label things the wallet does not have yet, and it may
    -- bring portfolios that the active one leaves aside. The second
    -- import names the wallet and the portfolio the first one named, so
    -- that it reads no more of the file than the user asked for.
    (CommandImportWallet scope file, ResponseImport report activeM)
      | not (null steps) -> stepsPrinter steps
      where
        steps =
          concat
            [ [ ("Next, sync the wallet, for the addresses, transactions and coins the file labels", hwLine "sync" []),
                ("Then import the file again, for the labels it could not place", hwLine "importwallet" (fileWord file : fileScopeWords scope))
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
          case troubleSteps results of
            [] -> stepsPrinter (txsSteps activeM (nub [placePortfolio (syncResPlace result) | result <- results, syncResTxUpdates result > 0]))
            steps -> stepsPrinter steps
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
    -- What a sync found wrong with pending transactions, and what to do
    -- about each: those in conflict, and those the store did not publish
    troubleSteps results =
      case (sum (syncResConflicted <$> results), concatMap (map fst . syncResNotPublished) results) of
        (0, []) -> []
        (conflicts, refused) -> conflictSteps conflicts <> notPublishedSteps (conflicts > 0) refused
    -- The command that sent a transaction synced the wallet too
    -- ('syncAfterDocs'), so there is no next step, unless that sync did
    -- not go through, or found something wrong with another pending
    -- transaction
    afterSending after =
      stepsPrinter $
        [("Next, sync the wallet to follow the transaction", hwLine "sync" []) | isJust after.notSyncedAfter]
          <> troubleSteps after.syncedAfter
    -- Without a file, signsweep asks for the keys on the terminal
    keysArgs (Just (KeysInFile file)) = [optionWord "--key-file", fileWord file]
    keysArgs _ = []

-- | The options that name one wallet and one portfolio of a file of a
-- wallet, as importwallet takes them: none for the whole file
fileScopeWords :: ImportScope -> [Doc Style]
fileScopeWords scope =
  concat [[optionWord "--wallet", pretty (fingerprintToText fingerprint)] | Just fingerprint <- [scope.scopeWallet]]
    <> concat [[optionWord "--portfolio", text (show index)] | Just index <- [scope.scopePortfolio]]

-- | The step that makes a portfolio a command created the active one, to
-- work in it: the switch to it, which names its wallet always, since the
-- active portfolio may be of another wallet
switchStep :: PortfolioRef -> (String, Doc Style)
switchStep ref = ("To work in " <> refText ref <> ", make it the active one", switchLine ref)

-- | The step that shows the hidden networks recover found portfolios
-- used on, in one command: setdefault takes --show-network several times
showNetworksStep :: [Network] -> (String, Doc Style)
showNetworksStep nets =
  ( "To show " <> andList ((.name) <$> nets),
    hwLine "setdefault" (concat [[optionWord "--show-network", text net.name] | net <- nets])
  )

-- | The command line that makes a portfolio the active one, which names
-- its wallet always
switchLine :: PortfolioRef -> Doc Style
switchLine ref = hwLine "switch" [text (show (refPortfolio ref)), optionWord "--wallet", text (show (refWallet ref))]

-- | The steps after a sync that found pending transactions in conflict,
-- given how many it found. They are pendingtxs, to find them, and
-- deletetx, to delete them, wherever they are: pendingtxs lists every
-- portfolio, and deletetx finds the portfolio of a transaction by
-- itself. So no step switches, whichever portfolio is active.
conflictSteps :: Natural -> [(String, Doc Style)]
conflictSteps conflicts
  | conflicts == 0 = []
  | conflicts == 1 =
      [ ("Next, find the pending transaction in conflict, which can no longer be sent", hwLine "pendingtxs" []),
        ("Then delete it, to free its coins", hwLine "deletetx" [metavarWord "HASH"])
      ]
  | otherwise =
      [ ("Next, find the pending transactions in conflict, which can no longer be sent", hwLine "pendingtxs" []),
        ("Then delete them, to free their coins", hwLine "deletetx" [metavarWord "HASH"])
      ]

-- | The step after a sync that sent pending transactions again which
-- the store did not publish, given whether steps about conflicts come
-- before it. The sync said why of each. Such a transaction keeps its coins
-- locked, and every sync sends it again, so the step is to delete it. Each
-- is named by its short id, as the sync named it.
notPublishedSteps :: Bool -> [TxHash] -> [(String, Doc Style)]
notPublishedSteps _ [] = []
notPublishedSteps afterOthers refused =
  [(start <> ", to free " <> its <> " coins", vcat [hwLine "deletetx" [pendingHashWord (shortHashText txid)] | txid <- refused])]
  where
    start = (if afterOthers then "Also delete " else "Next, delete ") <> what
    (what, its) = case refused of
      [_] -> ("the pending transaction that was not sent again", "its")
      _ -> ("the pending transactions that were not sent again", "their")

-- | The steps after a sync that found no conflict, given the active
-- portfolio and the portfolios whose transactions it updated, each once.
-- A sync that updated no transaction has no step: txs would show what it
-- showed before. txs lists the active portfolio alone. So the step is txs
-- when the sync updated the active portfolio. When it updated other
-- portfolios alone, the steps start with the switch to the one, or to
-- each in turn: a txs typed without a switch would show none of what the
-- sync found.
txsSteps :: Maybe PortfolioRef -> [PortfolioRef] -> [(String, Doc Style)]
txsSteps activeM updated
  | null updated = []
  | maybe False (`elem` updated) activeM = [("Next, see the transactions", hwLine "txs" [])]
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
stepsPrinter [] = Nothing
stepsPrinter steps = Just $ vcat [vcat [styled Field (what <> ":"), indent 2 command] | (what, command) <- steps]

-- | A command line of hw, "hw signtx NOSIGHASH", the command in the colour
-- of its kind as the help shows it
hwLine :: String -> [Doc Style] -> Doc Style
hwLine name args = hsep ("hw" : commandWord name : args)

-- | A command of hw, in the colour of its kind, as the list of commands
-- shows it
commandWord :: String -> Doc Style
commandWord name = maybe (styled Command name) commandName (commandNamed program name)

-- | An option, in the colour the help shows the options in
optionWord :: String -> Doc Style
optionWord = styled Option

-- | What the user has to fill in, as the help shows it
metavarWord :: String -> Doc Style
metavarWord = styled Metavar

-- | The hash that names a pending transaction
pendingHashWord :: String -> Doc Style
pendingHashWord = styled PendingHash

-- | The name of a file in a command the user may copy, quoted when the
-- shell needs it ('shellWord')
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
