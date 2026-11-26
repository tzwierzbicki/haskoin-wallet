{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The setup of hw: the questions plain hw asks the first time it is run,
-- when the data directory has no configuration file, and the file it
-- writes from the answers.
--
-- hw needs three choices that it cannot make well for its user. They are
-- the networks to show, the default network and the default type of
-- address ("Haskoin.Wallet.Config"). hw used to write a file of defaults
-- the first time any command ran. A user then found out about these
-- settings only by reading the documentation, often after receiving on a
-- type of address they did not want. So the setup asks, and explains each
-- choice in a few sentences before it does. Every other setting keeps its
-- default, and stays in the file as a comment. setdefault without an
-- option asks about them all later ("Haskoin.Wallet.Cli.SetDefault").
--
-- A fourth question asks whether hw keeps a journal for ledger, the
-- accounting program ('journalQuestion'). hw could go without asking:
-- no journal is a sound default, and it stays the default answer. The
-- question is there because the journal is a feature a user does not go
-- looking for. A user who keeps accounts learns here that hw can feed
-- them, and one who does not presses Enter. The question says what
-- ledger is, since most users of a wallet have never met it.
--
-- Only plain hw runs the setup. A command run before it is refused, and
-- told to run plain hw ('Haskoin.Wallet.Config.notSetUp'). A command does
-- not start the setup by itself: the questions would come in the middle
-- of what the user asked for, and a script would stop on them.
--
-- The questions are menus on a terminal, and lines to type elsewhere
-- ("Haskoin.Wallet.Cli.Question"). A script answers them on the input, a
-- line for each. A script may also write the configuration file itself,
-- since a file that is there means hw is set up.
--
-- = After the setup
--
-- A user who has just set hw up has no wallet yet, and may not know
-- which command makes one. So the setup ends with the commands to run
-- next ('firstSteps'): newportfolio, for a new mnemonic or for one the
-- user has already.
module Haskoin.Wallet.Cli.Setup
  ( -- * Running the setup
    needsSetup,
    setupMain,
    askChoices,

    -- * What the user chose
    Choices (..),
    choiceKeys,
    choiceRows,
    doneDoc,
    firstSteps,
    journalNote,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network)
import Haskoin.Wallet.Cli.PrettyPrinter.CommandLine (hwLine, optionWord)
import Haskoin.Wallet.Cli.PrettyPrinter.Defaults (keyRowsDoc)
import Haskoin.Wallet.Cli.PrettyPrinter.NextSteps (stepsPrinter)
import Haskoin.Wallet.Cli.Question (Asking, askOne, askSeveral, askingHere, say)
import Haskoin.Wallet.Cli.Question.Settings (defaultNetworkQuestion, defaultTypeQuestion, journalQuestion, networksQuestion)
import Haskoin.Wallet.Cli.Style (Style (..), outputOf, putDoc)
import Haskoin.Wallet.Cli.Wrap (dieWith)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Config.TomlEdit (tomlValue)
import Haskoin.Wallet.Keys (AccountKind, kindName)
import Prettyprinter
import System.Environment (getArgs)
import System.IO (stdout)
import qualified Toml

{- Running the setup -}

-- | Whether this run of hw is the setup: hw was typed alone, and its data
-- directory has no configuration file. With a file, plain hw shows its
-- help, as it always did.
needsSetup :: IO Bool
needsSetup = do
  args <- getArgs
  if null args then not <$> (hasConfigFile =<< hwDataDirectory) else return False

-- | Run the setup: ask the four questions, write the configuration file
-- of ~\/.hw from the answers, and say where it is and how to change it
-- later. A setup that is cancelled, or that fails, writes nothing and
-- ends hw as a failure.
--
-- The setup writes the configuration file and nothing else. A journal
-- the user asked for is written by the next command
-- ('Haskoin.Wallet.Commands.journalBeforeCommand'), and the setup says so
-- ('journalNote'). The journal is read from the database, which the
-- setup does not open: a database of an earlier hw may be there, and
-- the command that meets it says what to do.
--
-- A ~\/.hw that links to a directory that is not there is refused before
-- the first question. Its drive is not mounted, and the answers could
-- not be written.
setupMain :: IO ()
setupMain = do
  dir <- hwDataDirectory
  done <- runExceptT $ do
    checkDataDirectoryLink dir
    asking <- liftIO (askingHere "The setup")
    choices <- askChoices asking
    file <- writeFirstConfig dir (choiceKeys choices)
    return (file, choices)
  case done of
    Left problem -> dieWith problem
    Right (file, choices) -> do
      output <- outputOf stdout
      putDoc stdout output (doneDoc file (journalFile def {configDataDirectory = dir}) choices)

-- | Ask the four questions, in the way given, and give what the user
-- chose. The default network is asked among the networks just chosen,
-- since it must be one hw shows.
askChoices :: Asking -> ExceptT String IO Choices
askChoices asking = do
  say asking introDoc
  shown <- askSeveral asking 1 networksQuestion
  defaultNetwork <- askOne asking 2 (defaultNetworkQuestion shown)
  defaultType <- askOne asking 3 defaultTypeQuestion
  journal <- askOne asking 4 journalQuestion
  -- A line between the last answer and what the setup wrote
  say asking mempty
  return (Choices shown defaultNetwork defaultType journal)

-- | What the setup says before its first question
introDoc :: Doc Style
introDoc =
  vsep
    [ annotate Title "Welcome to hw.",
      fillSep (pretty <$> Text.words "hw has no configuration file yet. Four questions set it up, and you can change every answer later.")
    ]

{- What the user chose -}

-- | What the user chose in the setup
data Choices = Choices
  { -- | The networks hw shows, in the order of the networks of a
    -- portfolio
    chosenShown :: ![Network],
    -- | The default network, one of those shown
    chosenNetwork :: !Network,
    -- | The default type of address
    chosenType :: !AccountKind,
    -- | Whether hw keeps a journal for ledger
    chosenJournal :: !Bool
  }
  deriving (Eq, Show)

-- | The keys of the configuration file the choices set, with their
-- values, in the order the file shows them
choiceKeys :: Choices -> [(Text, Toml.Value)]
choiceKeys choices =
  [ (defaultNetworkKey, Toml.Text (networkName choices.chosenNetwork)),
    (shownNetworksKey, Toml.List (Toml.Text . networkName <$> choices.chosenShown)),
    (defaultTypeKey, Toml.Text (kindName choices.chosenType)),
    (journalKey, Toml.Bool choices.chosenJournal)
  ]

-- | The choices as the setup shows them once they are written: each key
-- of the file with its value, as setdefault and showdefault show them
choiceRows :: Choices -> [(Text, Text)]
choiceRows choices =
  [ (shownNetworksKey, Text.intercalate ", " (networkName <$> choices.chosenShown)),
    (defaultNetworkKey, networkName choices.chosenNetwork),
    (defaultTypeKey, kindName choices.chosenType),
    (journalKey, tomlValue (Toml.Bool choices.chosenJournal))
  ]

-- | What the setup says once the file is written: where it is, what it
-- sets, where the journal will be when the user asked for one
-- ('journalNote'), the two ways to change the settings later, and the
-- commands to run next ('firstSteps'). setdefault is in the colour of a
-- command, so that it stands out as what to type.
--
-- The first path is the configuration file, and the second the journal
-- hw keeps ('Haskoin.Wallet.Config.journalFile').
--
-- The steps come last, apart from the rest. They are what the user does
-- now, so they are the last lines on the screen. The settings above them
-- are for later.
doneDoc :: FilePath -> FilePath -> Choices -> Doc Style
doneDoc file journal choices =
  vsep $
    ["hw is set up. Your settings were written to" <+> annotate UserText (pretty file)]
      <> keyRowsDoc (choiceRows choices)
      <> [journalNote journal | choices.chosenJournal]
      <> [ mempty,
           fillSep $
             plain "To change them later, run"
               <> [annotate Command "hw setdefault" <> ","]
               <> plain "or edit that file. It lists every setting of hw, with its default.",
           mempty
         ]
      <> maybeToList (stepsPrinter firstSteps)
  where
    -- The words of a sentence, which the line breaks between
    plain :: Text -> [Doc Style]
    plain = map pretty . Text.words

-- | What the setup says of the journal the user asked for: where the
-- file will be, for the user to include it in a journal of their own,
-- and that the next command writes it. The setup writes no journal
-- itself ('setupMain'), so it does not say that the file is there.
journalNote :: FilePath -> Doc Style
journalNote journal =
  "Your next command writes the journal for ledger to" <+> annotate UserText (pretty journal)

-- | What to run once hw is set up, each step with what it does and its
-- command, as the commands of a payment show their next steps.
--
-- A new user has no wallet, and no command but newportfolio works
-- without one. So the setup names it, in its two forms: a new mnemonic,
-- or a mnemonic the user has already. The help comes last, for the
-- rest. The steps are the same for every answer of the setup. They do
-- not look at the database either: the setup runs before hw opens it.
firstSteps :: [(String, Doc Style)]
firstSteps =
  [ ("Next, create a wallet, with a new mnemonic to write down", hwLine "newportfolio" []),
    ("Or create the wallet of a mnemonic you have already", hwLine "newportfolio" [optionWord "--existing"]),
    ("To see what else hw does", hsep ["hw", optionWord "--help"])
  ]
