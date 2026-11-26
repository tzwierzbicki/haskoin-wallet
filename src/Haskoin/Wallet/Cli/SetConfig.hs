{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | setconfig without an option: a question for each setting of the
-- configuration file, and the options the answers stand for.
--
-- An option of setconfig changes one default, and suits a script and a
-- user who knows the name of the option. A user who does not know what
-- hw can be told has to read the documentation to find out. So setconfig
-- given no option asks instead, as the setup of the first run does
-- ("Haskoin.Wallet.Cli.Setup"): each setting is explained, with the
-- values it takes ("Haskoin.Wallet.Cli.Question.Settings"). The user
-- meets every setting once, and changes those they care about.
--
-- Each question starts on the value hw has now, so Enter keeps it. A
-- user who presses Enter to the end changes nothing, and one who wants to
-- change the last setting does not have to give the eleven others again.
--
-- The answers become the options of setconfig that change something
-- ('changedSettings'), and setconfig then runs as if they were typed.
-- Only a setting whose answer differs from the configuration is written.
-- A key that the file leaves to its default stays out of the file, and
-- keeps following the default of hw: the address of the public server,
-- for instance, should a later hw change it.
--
-- The table journal-accounts has no question: its lines are written in
-- the file, one for each tag.
module Haskoin.Wallet.Cli.SetConfig
  ( askSetConfig,
    askSettings,
    changedSettings,
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import Haskoin.Wallet.Cli.Command (SetConfig (..))
import Haskoin.Wallet.Cli.Listing (ListingOrder (..))
import Haskoin.Wallet.Cli.Question (Asking, askEntry, askOne, askSeveral, askingHere, checking, say, startingOn)
import Haskoin.Wallet.Cli.Question.Settings
import Haskoin.Wallet.Cli.Style (Style (..))
import Haskoin.Wallet.Config
import Prettyprinter

-- | Ask the user of this run of hw about every setting, and give the
-- options of setconfig that their answers stand for: one for each
-- setting they changed. The questions are menus on a terminal, and lines
-- to type elsewhere.
askSetConfig :: Config -> ExceptT String IO SetConfig
askSetConfig cfg = do
  asking <- liftIO (askingHere "hw setconfig")
  changedSettings cfg <$> askSettings asking cfg

-- | Ask the twelve questions, in the way given, each starting on what
-- the configuration given has, and give the configuration the user wants.
--
-- The questions come by subject: where the commands work, how the
-- answers look, what hw keeps by itself, then the server. The settings a
-- user is most likely to change come first, and the ones about the
-- server, which most users leave alone, come last.
--
-- The default network is asked among the networks just chosen, since it
-- must be one hw shows. It starts on the default network of the
-- configuration, or on the first network shown when the user just hid
-- that one.
askSettings :: Asking -> Config -> ExceptT String IO Config
askSettings asking cfg = do
  say asking introDoc
  shown <- askSeveral asking 1 (checking (configShownNetworks cfg) networksQuestion)
  defaultNetwork <- askOne asking 2 (startingOn (configDefaultNetwork cfg) (defaultNetworkQuestion shown))
  defaultType <- askOne asking 3 (startingOn (configDefaultType cfg) defaultTypeQuestion)
  unit <- askOne asking 4 (startingOn (configUnit cfg) unitQuestion)
  view <- askOne asking 5 (startingOn (configListingView cfg) listingViewQuestion)
  reversed <- askOne asking 6 (startingOn (configListingReverse cfg) listingReverseQuestion)
  journal <- askOne asking 7 (startingOn (configJournal cfg) journalQuestion)
  copiesDatabase <- askOne asking 8 (startingOn (configAutoBackupDatabase cfg) autoBackupDatabaseQuestion)
  copiesLabels <- askOne asking 9 (startingOn (configAutoBackupLabels cfg) autoBackupLabelsQuestion)
  host <- askEntry asking 10 (hostEntry (configHost cfg))
  requests <- askEntry asking 11 (parallelRequestsEntry (configParallelRequests cfg))
  gap <- askEntry asking 12 (gapEntry (configGap cfg))
  -- A line between the last answer and what setconfig wrote
  say asking mempty
  return
    cfg
      { configShownNetworks = shown,
        configDefaultNetwork = defaultNetwork,
        configDefaultType = defaultType,
        configUnit = unit,
        configListingView = view,
        configListingReverse = reversed,
        configJournal = journal,
        configAutoBackupDatabase = copiesDatabase,
        configAutoBackupLabels = copiesLabels,
        configHost = host,
        configParallelRequests = requests,
        configGap = gap
      }

-- | What setconfig says before its first question
introDoc :: Doc Style
introDoc =
  vsep
    [ annotate Title "The settings of hw.",
      fillSep (pretty <$> Text.words "Twelve questions go through them. Each one starts on the value hw has now, and Enter keeps it. hw writes what you changed after the last question.")
    ]

-- | The options of setconfig that turn the first configuration into the
-- second: an option for each setting that differs, and none for a
-- setting that is the same. Two configurations that are the same give no
-- option at all ('Haskoin.Wallet.Cli.Command.noSettings').
--
-- The networks hw shows are two lists of options: those to show, which
-- the second configuration shows and the first does not, and those to
-- hide, the other way round.
changedSettings :: Config -> Config -> SetConfig
changedSettings before after =
  SetConfig
    { network = changed configDefaultNetwork,
      kind = changed configDefaultType,
      unit = changed configUnit,
      view = changed configListingView,
      listingOrder = (\reversed -> if reversed then ReverseOrder else UsualOrder) <$> changed configListingReverse,
      showNetworks = [net | net <- configShownNetworks after, net `notElem` configShownNetworks before],
      hideNetworks = [net | net <- configShownNetworks before, net `notElem` configShownNetworks after],
      journal = changed configJournal,
      autoBackupDatabase = changed configAutoBackupDatabase,
      autoBackupLabels = changed configAutoBackupLabels,
      host = changed configHost,
      parallelRequests = changed configParallelRequests,
      gap = changed configGap
    }
  where
    -- The setting of the second configuration, when it is not that of
    -- the first
    changed :: (Eq a) => (Config -> a) -> Maybe a
    changed setting
      | setting before == setting after = Nothing
      | otherwise = Just (setting after)
