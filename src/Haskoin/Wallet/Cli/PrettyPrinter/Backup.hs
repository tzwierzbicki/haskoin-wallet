{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answers about database files: what show tells of one, the copies
-- that showbackups lists, and what backup and restoredatabase did.
module Haskoin.Wallet.Cli.PrettyPrinter.Backup
  ( databaseFileAnswerDoc,
    databaseFileDoc,
    backupFilesDocs,
    restoredAnswerDoc,
    restoredDoc,
    backupsDoc,
  )
where

import Data.List (genericLength, intersperse)
import Data.Maybe (isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, ZonedTime, utcToLocalZonedTime)
import Haskoin.Wallet.Cli.PrettyPrinter.Base (agoText, block, columns, counted, dayText, field, fileHeadingDoc, labelWord, styled, text, withStyle)
import Haskoin.Wallet.Cli.PrettyPrinter.Portfolio (activeMarkDoc, walletLine)
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.Style
import Haskoin.Wallet.Database.Backup (BackupsShown (..), CopiesShown (..))
import Haskoin.Wallet.Database.File (DatabaseShown (..), PortfolioShown (..), WalletShown (..))
import Haskoin.Wallet.Database.Portfolio
import Numeric.Natural (Natural)
import Prettyprinter (Doc, align, indent, vcat, (<+>))
import System.FilePath (takeFileName)

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
-- (@walletImportDoc@ of "Haskoin.Wallet.Cli.PrettyPrinter.WalletFile")
-- and of the listing of the portfolios: the line of a wallet, with its
-- fingerprint ('walletLine'), and its portfolios under it. The user reads
-- the three the same way, and the heading tells which file it is.
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

-- | A wallet of a database file: its line, with the fingerprint of its
-- seed and its description when it has one ('walletLine'), then a line
-- for each of its portfolios, given the date each was created. The line of a portfolio
-- starts with the mark of the active one ('activeMarkDoc'), as in the
-- listing of the portfolios. Its name is padded to the longest name of
-- the wallet, so that the days line up. The description comes last, since
-- not every portfolio has one.
shownWalletDoc :: WalletShown -> [ZonedTime] -> Doc Style
shownWalletDoc wallet created =
  vcat $
    walletLine (walletShownIndex wallet) (walletShownFingerprint wallet) [labelWord description | let description = walletShownDescription wallet, not (Text.null description)]
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
-- already, and names that one ("Haskoin.Wallet.Database.Backup").
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
-- lists those of a database file (@shownWalletDoc@), then the backup hw
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
