{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about wallets and portfolios: a new mnemonic or its shares,
-- the line of a wallet, the portfolios of a listing under their wallets and
-- in their table, and what newportfolio and recover say of the portfolios
-- they made.
module Haskoin.Wallet.Cli.PrettyPrinter.Portfolio
  ( mnemonicDoc,
    sharesDoc,
    secretDoc,
    secretSheetDoc,
    masterKeyDoc,
    walletLine,
    listedWalletDocs,
    listedPortfolioDoc,
    walletDoc,
    activeMarkDoc,
    compactPortfolioDoc,
    verbosePortfolioDoc,
    portfolioTableColumns,
    portfolioRowDocs,
    newPortfolioDoc,
    activeDoc,
    recoveredDoc,
    usedOnHiddenDoc,
  )
where

import Data.Function (on)
import Data.List (intersperse, sort)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Haskoin
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Cli.Command (WordsLayout (..))
import Haskoin.Wallet.Cli.PrettyPrinter.Balance (heldAmountCell, notSyncedWords, shortBalanceFigures, waitingAmount, waitingAmountCell)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (amountPrinter, andList, andListWords, block, cellsLine, columns, commaList, counted, countPrinter, dayText, field, labelWord, orList, rblock, secondText, sentence, styled, text, withStyle)
import Haskoin.Wallet.Cli.PrettyPrinter.Place (portfolioWords, refText)
import Haskoin.Wallet.Cli.PrettyPrinter.Table (Column (..), Side (..), amountColumn, column, tableLine, widestCell)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Config.View (View (..))
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (kindName)
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, hsep, indent, pretty, vcat, (<+>))

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

{- Wallets -}

-- | The line of a wallet: its name, its fingerprint after the word that
-- says what it is, then the cells given, such as its description.
--
-- Every view that lists wallets starts a wallet with this line, and puts
-- its portfolios under it: the listing of the portfolios, the view of a
-- database file and that of a file of a wallet. So a wallet reads the
-- same wherever the user meets it.
--
-- The fingerprint is what names a wallet for good: the name follows an
-- index, which a deleted wallet frees. It is written after the word
-- fingerprint, since eight hexadecimal digits alone read as the short id
-- of a transaction or of a coin, and a user who looks for the fingerprint
-- looks for the word.
walletLine :: Natural -> Fingerprint -> [Doc Style] -> Doc Style
walletLine index fingerprint cells =
  columns $
    [ pretty (walletName index) `withStyle` Account,
      "fingerprint" <+> pretty (fingerprintToText fingerprint)
    ]
      <> cells

{- Portfolios -}

-- | The portfolios of a listing, in the view asked for: a document for
-- each wallet, with its portfolios under its line ('walletDoc'), the time
-- each was created in the time zone of the computer.
--
-- The portfolios of a wallet must follow each other, as the listing gives
-- them, and come in one chunk, as a listing read whole does: a wallet cut
-- in two would show its line twice.
listedWalletDocs :: View -> UnitChoice -> [PortfolioView] -> IO [Doc Style]
listedWalletDocs view choice portfolios =
  map (walletDoc view choice) . NonEmpty.groupBy ((==) `on` (refWallet . viewRef . snd)) <$> mapM createdLocally portfolios
  where
    createdLocally portfolio = do
      created <- utcToLocalZonedTime (viewCreated portfolio)
      return (created, portfolio)

-- | One portfolio as the listing of the portfolios shows it, under the
-- line of its wallet: what switch and label answer with
listedPortfolioDoc :: UnitChoice -> PortfolioView -> IO (Doc Style)
listedPortfolioDoc choice portfolio = vcat <$> listedWalletDocs Compact choice [portfolio]

-- | A wallet with its portfolios, each with the time it was created: the
-- line of the wallet, with its fingerprint and its description when it
-- has one ('walletLine'), then its portfolios under it, indented, in the
-- view asked for, a blank line between two of them.
--
-- The listing was once flat: each portfolio named its wallet on its first
-- line, repeated the description of the wallet under it, and gave the
-- fingerprint in the verbose view alone, among the descriptors. A
-- fingerprint and a description are of the wallet, so they are said once,
-- on its line, in every view.
walletDoc :: View -> UnitChoice -> NonEmpty (ZonedTime, PortfolioView) -> Doc Style
walletDoc view choice portfolios =
  vcat $
    walletLine (refWallet (viewRef first)) (viewFingerprint first) [labelWord description | let description = viewWalletDescription first, not (Text.null description)]
      : intersperse mempty [indent 2 (portfolioDoc created portfolio) | (created, portfolio) <- NonEmpty.toList portfolios]
  where
    -- Any of them names the wallet
    first = snd (NonEmpty.head portfolios)
    portfolioDoc = case view of
      Verbose -> verbosePortfolioDoc choice
      -- The table is laid out by sectionsPrinter ('portfolioRowDocs')
      _ -> compactPortfolioDoc choice

-- | The first line of a portfolio: the mark of the active one, its name,
-- and its description when it has one. The line of its wallet is above
-- it ('walletDoc'). The mark is a column of its own before the name, a *
-- for the active portfolio and a space for the others, so that the names
-- line up and the eye finds the active one at the edge of the portfolios.
-- The lines under the first one are indented past the mark.
portfolioLine :: PortfolioView -> Doc Style
portfolioLine portfolio =
  activeMark portfolio
    <+> columns
      ( (pretty (portfolioName (refPortfolio (viewRef portfolio))) `withStyle` Account)
          : [labelWord description | let description = viewDescription portfolio, not (Text.null description)]
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
--
-- The amounts are in the unit of the network ('unitOn'): a portfolio shows
-- its bitcoins as btc and, a line below, its coins of Bitcoin Cash as bch.
networkFigures :: UnitChoice -> NetworkView -> Doc Style
networkFigures choice net
  | networkViewSynced net = commaList (shortBalanceFigures unit (networkViewTotal net) (networkViewSpendable net) <> [coins, addresses])
  | otherwise = commaList [notSyncedWords, addresses]
  where
    unit = unitOn (networkViewNetwork net) choice
    coins = text (counted (fromIntegral (networkViewCoins net)) "coin" "coins")
    addresses = text (counted (networkViewAddresses net) "address" "addresses")

-- | A portfolio in a few lines: its first line, with the mark of the
-- active portfolio, the date it was created, then a line for each
-- network it has addresses or coins on, with what it holds there.
--
-- A portfolio without such a network has one line in their place. It has
-- no addresses yet, or it has some on hidden networks alone
-- ('viewUsedOnHidden'): it then has nothing on the shown networks, which
-- is true and names no hidden network.
compactPortfolioDoc :: UnitChoice -> ZonedTime -> PortfolioView -> Doc Style
compactPortfolioDoc choice created portfolio =
  vcat $
    portfolioLine portfolio
      : (indent 4 <$> (("created" <+> text (dayText created)) : networks))
  where
    active = activeNetworks portfolio
    pad = fromIntegral (maximum (0 : (length . (.name) . networkViewNetwork <$> active)))
    networks
      | null active && viewUsedOnHidden portfolio = [styled Muted "nothing on the shown networks"]
      | null active = [styled Muted "no addresses yet"]
      | otherwise = [text (block pad (networkViewNetwork net).name) <> "  " <> networkFigures choice net | net <- active]

-- | A portfolio with every detail: its first line, the time it was
-- created, then every network it has an account on, with each of its
-- balances, and under each network its accounts, each with its path and
-- the keys another wallet takes it by. The output descriptor comes whole,
-- with its origin, the fingerprint of the line of the wallet and the path
-- of the account. Each account adds its key in the form of SLIP-132, a
-- zpub or a vpub for a bip84 account, for the wallets that take only that
-- form.
--
-- A descriptor row takes 148 columns or more, and a zpub row 126. The
-- terminal wraps such a row, which goes on at the first column of the
-- screen. The portfolios listing has the pager wrap its lines in this
-- view, so that the user can copy a descriptor whole
-- ('Haskoin.Wallet.Cli.Command.wrapsLines').
verbosePortfolioDoc :: UnitChoice -> ZonedTime -> PortfolioView -> Doc Style
verbosePortfolioDoc choice created portfolio =
  vcat $
    portfolioLine portfolio
      : ( indent 4
            <$> ((field 11 "Created" <> text (secondText created)) : concatMap networkBlock (viewNetworks portfolio))
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
      where
        unit = unitOn (networkViewNetwork net) choice
    accountBlock keys =
      columns [text (cs (kindName (keysKind keys))), text (pathToStr (keysPath keys))]
        : ( indent 2
              <$> ( [field 10 "Descriptor" <> text (cs (keysDescriptor keys))]
                      <> [field 10 "SLIP-132" <> text (cs key) | Just key <- [keysSlip132 keys]]
                  )
          )

{- The table of portfolios -}

-- The table of portfolios gives a row to each network of each portfolio
-- that has addresses or coins, as the compact view gives it a line. A
-- portfolio without any gets one row with no network, so that it shows.
-- The listing is read whole, so its first columns are as wide as their
-- widest cells.

-- | The columns of the table of portfolios, for the portfolios listed. The
-- first one, without a heading, marks the active portfolio, as the other
-- views do. The fingerprint of the wallet comes after its name, on every
-- row, as on the line of a wallet. Each amount has the unit of its
-- network after it ('amountColumn'): btc on a row of Bitcoin, bch on one
-- of Bitcoin Cash.
portfolioTableColumns :: UnitChoice -> [PortfolioView] -> [Column]
portfolioTableColumns choice portfolios =
  [ column "" 1 LeftSide,
    column "Wallet" (widestCell (cs . walletName . refWallet . viewRef) portfolios) LeftSide,
    -- Eight hexadecimal digits, under a wider heading
    column "Fingerprint" 8 LeftSide,
    column "Portfolio" (widestCell (cs . portfolioName . refPortfolio . viewRef) portfolios) LeftSide,
    column "Network" (widestCell (\net -> (networkViewNetwork net).name) (concatMap activeNetworks portfolios)) LeftSide,
    amountColumn "Total" unit,
    amountColumn "Spendable" unit,
    amountColumn "Unspendable" unit,
    -- The headings set the widths: 99 999 coins, 999 999 999 addresses
    column "Coins" 1 RightSide,
    column "Addresses" 1 RightSide,
    column "Description" 0 LeftSide
  ]
  where
    unit = chosenUnit choice

-- | A portfolio on the rows of the table of portfolios: one for each
-- network it has addresses or coins on, or one without a network
portfolioRowDocs :: UnitChoice -> [Column] -> PortfolioView -> [Doc Style]
portfolioRowDocs choice headed portfolio =
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
      where
        unit = unitOn (networkViewNetwork net) choice
    named =
      [ activeMark portfolio,
        pretty (walletName (refWallet (viewRef portfolio))),
        pretty (fingerprintToText (viewFingerprint portfolio)),
        pretty (portfolioName (refPortfolio (viewRef portfolio))) `withStyle` Account
      ]
    describedCell =
      hsep [labelWord description | let description = viewDescription portfolio, not (Text.null description)]

{- New portfolios -}

-- | The portfolio newportfolio created: its name and its wallet, the
-- fingerprint of the wallet, which names the mnemonic from then on, and
-- the accounts it has (@newAccountsDoc@), then which portfolio is active
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

-- | What a command that created portfolios says of the active one, the
-- one the commands work in: that it made the first portfolio of the
-- database the active one, or which one is still active. switch, which a
-- next step names, makes a new one active (@switchStep@ of
-- "Haskoin.Wallet.Cli.PrettyPrinter.NextSteps").
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
-- that shows the networks is the next step (@showNetworksStep@ of
-- "Haskoin.Wallet.Cli.PrettyPrinter.NextSteps").
usedOnHiddenDoc :: WalletNaming -> [(PortfolioRef, [Network])] -> Doc Style
usedOnHiddenDoc naming used =
  vcat
    [ (text (portfolioWords naming ref) `withStyle` Account) <> "  " <> text ("used on " <> andList ((.name) <$> nets) <> hidden nets)
      | (ref, nets) <- used
    ]
  where
    hidden [_] = ", which is hidden"
    hidden _ = ", which are hidden"
