{-# LANGUAGE OverloadedStrings #-}

-- | The setup of hw as its user meets it, in a home hw never ran in: a
-- command run before the setup, which is refused and creates nothing;
-- plain hw with its four questions answered by typed lines, as a script
-- or a pipe answers them; and plain hw on a terminal, where the questions
-- are menus and the answers are keys. "Haskoin.Wallet.Cli.SetupSpec" has
-- the rules of the menus themselves.
--
-- A new home has no ~/.hw ('withNewHome'). The setup writes
-- ~/.hw/config.toml, and nothing else. A journal asked for in the setup
-- is written by the command that comes next.
module Haskoin.Wallet.Workflow.SetupSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Workflow.TestUtils (seedTwice)
import System.Directory (createDirectory, createDirectoryLink, doesPathExist)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import qualified Toml

spec :: Spec
spec = do
  beforeSetupSpec
  typedSpec
  keyboardSpec

-- | The refusal of a command run in a home where hw was never set up
notSetUpIn :: FilePath -> Text
notSetUpIn home =
  "hw is not set up yet: there is no configuration file at "
    <> Text.pack (configOf home)
    <> ". Run hw without a command to set it up.\n"

{- Before the setup -}

beforeSetupSpec :: Spec
beforeSetupSpec =
  describe "A command run before hw is set up" $ do
    it "is refused, and told to run hw without a command" $
      withNewHome $ \home -> do
        run <- hw home ["portfolios"] []
        runErr run `shouldBe` notSetUpIn home

    it "exits with 1, and answers nothing" $
      withNewHome $ \home -> do
        run <- hw home ["portfolios"] []
        (runExit run, runOut run) `shouldBe` (ExitFailure 1, "")

    -- The setup creates ~/.hw, when it writes the file
    it "creates nothing: not even the directory ~/.hw" $
      withNewHome $ \home -> do
        _ <- hw home ["portfolios"] []
        doesPathExist (home </> ".hw") `shouldReturn` False

    it "is refused the same when it would write the configuration file, as setdefault does" $
      withNewHome $ \home -> do
        run <- hw home ["setdefault", "--unit", "sat"] []
        runErr run `shouldBe` notSetUpIn home

    it "still shows its help" $
      withNewHome $ \home -> do
        run <- hw home ["receive", "--help"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isInfixOf "Usage: hw receive"

    it "still shows the version of hw, when the command is --version" $
      withNewHome $ \home -> do
        run <- hw home ["--version"] []
        runExit run `shouldBe` ExitSuccess
        runOut run `shouldSatisfy` Text.isPrefixOf "Software version: "

    -- A script, or the user, may write the file: it is what says that hw
    -- is set up. An empty file sets nothing, so every default applies.
    it "runs once a configuration file is there, even an empty one written by hand" $
      withNewHome $ \home -> do
        createDirectory (home </> ".hw")
        writeFile (configOf home) ""
        run <- hw home ["showdefault", "--network"] []
        runOut run `shouldBe` "default-network: btc\nRead from " <> Text.pack (configOf home) <> "\n"

    -- The drive of the wallet is not mounted: the wallet is set up, and
    -- must not be set up again
    it "is refused for the link, not for a missing setup, when ~/.hw links to a directory that is not there" $
      withNewHome $ \home -> do
        createDirectoryLink (home </> "drive") (home </> ".hw")
        run <- hw home ["portfolios"] []
        runErr run
          `shouldBe` Text.pack ("The directory " <> (home </> ".hw") <> " links to " <> (home </> "drive") <> ", which does not exist. If it is on a drive, mount the drive and try again.\n")

{- Typed answers -}

-- | The four keys the setup sets in a configuration file, with the
-- version every file of hw sets, when no journal was asked for: the
-- networks shown, the default network and the default type are given
setUpWith :: [Text] -> Text -> Text -> Map.Map Text Toml.Value
setUpWith shown defaultNetwork defaultType =
  Map.fromList
    [ ("version", Toml.Integer 1),
      ("shown-networks", Toml.List (Toml.Text <$> shown)),
      ("default-network", Toml.Text defaultNetwork),
      ("default-type", Toml.Text defaultType),
      ("journal", Toml.Bool False)
    ]

-- The input of hw is no terminal here: each question is a line to type
typedSpec :: Spec
typedSpec =
  describe "Plain hw, where it was never set up, with its answers typed" $ do
    it "writes the networks to show, the default network and the default type that were typed" $
      withNewHome $ \home -> do
        _ <- hw home [] ["btc btctest4", "btctest4", "bip84", "no"]
        configKeysOf home `shouldReturn` setUpWith ["btc", "btctest4"] "btctest4" "bip84"

    it "takes btc and bch, btc, bip44 and no journal for four empty lines" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "", "", ""]
        configKeysOf home `shouldReturn` setUpWith ["btc", "bch"] "btc" "bip44"

    it "turns the journal for ledger on when yes is typed for the fourth question" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "", "", "yes"]
        configKeyOf home "journal" `shouldReturn` Just (Toml.Bool True)

    it "does not take maybe for the journal: it asks again, and takes the next answer" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "", "maybe", "yes"]
        runErr run `shouldSatisfy` Text.isInfixOf "Type one of no, yes, or nothing for no\n"
        configKeyOf home "journal" `shouldReturn` Just (Toml.Bool True)

    -- hw always has a default network and a default type
    it "does not take none for the default network: it asks again, and takes the next answer" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "none", "bch", "", ""]
        runErr run `shouldSatisfy` Text.isInfixOf "Type one of btc, bch, or nothing for btc\n"
        configKeysOf home `shouldReturn` setUpWith ["btc", "bch"] "bch" "bip44"

    it "does not take none for the default type: it asks again, and takes the next answer" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "none", "bip84", ""]
        runErr run `shouldSatisfy` Text.isInfixOf "Type one of bip44, bip84, or nothing for bip44\n"
        configKeysOf home `shouldReturn` setUpWith ["btc", "bch"] "btc" "bip84"

    it "exits with success" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "", ""]
        runExit run `shouldBe` ExitSuccess

    -- The lines are those of a terminal 80 columns wide
    it "says what it wrote, in which file, how to change it later, and what to run next" $
      withNewHome $ \home -> do
        run <- hw home [] ["btc btctest4", "btctest4", "bip84", "no"]
        runOut run
          `shouldBe` Text.unlines
            [ "hw is set up. Your settings were written to " <> Text.pack (configOf home),
              "shown-networks : btc, btctest4",
              "default-network: btctest4",
              "default-type   : bip84",
              "journal        : false",
              "",
              "To change them later, run hw setdefault, or edit that file. It lists every",
              "setting of hw, with its default.",
              "",
              "Next, create a wallet, with a new mnemonic to write down:",
              "  hw newportfolio",
              "Or create the wallet of a mnemonic you have already:",
              "  hw newportfolio --existing",
              "To see what else hw does:",
              "  hw --help"
            ]

    it "asks its four questions on the error output, each with its default in brackets" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "", ""]
        filter (Text.isSuffixOf "] ") (Text.lines (runErr run))
          `shouldBe` [ "Networks to show, separated by spaces [btc bch] ",
                       "Default network: btc or bch [btc] ",
                       "Default type of address: bip44 or bip84 [bip44] ",
                       "Keep a journal for ledger: no or yes [no] "
                     ]

    it "explains each question before it asks it" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "", ""]
        filter (\line -> any (`Text.isPrefixOf` line) ["1. ", "2. ", "3. ", "4. "]) (Text.lines (runErr run))
          `shouldBe` [ "1. Which networks do you work with?",
                       "2. Which network is the default?",
                       "3. Which type of address is the default?",
                       "4. Should hw keep a journal for ledger, the accounting program?"
                     ]

    it "says how many questions there are before the first one" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "", ""]
        runErr run `shouldSatisfy` Text.isInfixOf "Four questions set it up"

    -- The lines are those of a terminal 80 columns wide
    it "says where the journal will be, below the settings, when a journal was asked for" $
      withNewHome $ \home -> do
        run <- hw home [] ["", "", "", "yes"]
        take 2 (drop 4 (Text.lines (runOut run)))
          `shouldBe` [ "journal        : true",
                       "Your next command writes the journal for ledger to " <> Text.pack (home </> ".hw" </> "portfolios.ledger")
                     ]

    -- The new home has no wallet, so the journal holds its two comments
    it "has the next command write the journal that was asked for" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "", "", "yes"]
        _ <- hw home ["portfolios"] []
        Text.lines <$> Text.IO.readFile (home </> ".hw" </> "portfolios.ledger")
          `shouldReturn` [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
                           "; hw writes this file whole each time. Do not edit it: your changes would be lost."
                         ]

    -- The default network must be one hw shows
    it "offers the networks just chosen, and no other, as the default network" $
      withNewHome $ \home -> do
        run <- hw home [] ["bch btctest4", "", "", ""]
        runErr run `shouldSatisfy` Text.isInfixOf "Default network: bch or btctest4 [bch] "

    it "asks again for a default network that is none of those chosen, and takes the next answer" $
      withNewHome $ \home -> do
        run <- hw home [] ["bch", "btc", "bch", "", ""]
        runErr run `shouldSatisfy` Text.isInfixOf "Type one of bch, or nothing for bch\n"
        configKeysOf home `shouldReturn` setUpWith ["bch"] "bch" "bip44"

    it "asks again for the networks when one typed is no network, naming those there are" $
      withNewHome $ \home -> do
        run <- hw home [] ["btc doge", "btc", "", "", ""]
        runErr run `shouldSatisfy` Text.isInfixOf "\"doge\" is none of btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4, bchreg\n"
        configKeysOf home `shouldReturn` setUpWith ["btc"] "btc" "bip44"

    -- Three answers out of four
    it "writes nothing when the input ends before the last answer, and exits with 1" $
      withNewHome $ \home -> do
        run <- hw home [] ["btc", "btc", "bip84"]
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isSuffixOf "Cancelled: the input ended before the answer\n"
        doesPathExist (home </> ".hw") `shouldReturn` False

    it "keeps every setting it did not ask about as a comment of the file, with its default" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "", "", ""]
        written <- Text.lines <$> Text.IO.readFile (configOf home)
        filter (`elem` ["# gap = 20", "# unit = \"btc\"", "# host = \"https://api.haskoin.com/\""]) written
          `shouldBe` ["# host = \"https://api.haskoin.com/\"", "# unit = \"btc\"", "# gap = 20"]

    it "sets hw up: the next command runs with what was chosen" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "bch", "bip84", ""]
        run <- hw home ["showdefault", "--network", "--type"] []
        runOut run `shouldBe` "default-network: bch\ndefault-type   : bip84\nRead from " <> Text.pack (configOf home) <> "\n"

    -- The setup ends by naming this command. The new home has no wallet,
    -- so the wallet of the mnemonic is the first one.
    it "leaves hw ready for the command it names next: newportfolio --existing creates the first wallet" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "", "", ""]
        run <- hw home ["newportfolio", "--existing"] seedTwice
        take 1 (Text.lines (runOut run)) `shouldBe` ["Created portfolio0 of wallet0"]

    -- With a configuration file, plain hw is what it always was
    it "asks nothing the second time: plain hw then shows its help, and leaves the file as it is" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "bch", "", ""]
        again <- hw home [] ["btc", "btc", "bip84", "yes"]
        runErr again `shouldSatisfy` Text.isPrefixOf "\n▁ ▄ ▄ ▁ ▄ ▁ ▁ ▁\n▁ ▄ ▄ ▄ ▁ ▄ ▄ ▄  hw\n\nhw is a BIP44 and BIP84 wallet for Bitcoin and Bitcoin Cash."
        configKeysOf home `shouldReturn` setUpWith ["btc", "bch"] "bch" "bip44"

    it "is refused before its first question when ~/.hw links to a directory that is not there" $
      withNewHome $ \home -> do
        createDirectoryLink (home </> "drive") (home </> ".hw")
        run <- hw home [] ["", "", "", ""]
        runErr run
          `shouldBe` Text.pack ("The directory " <> (home </> ".hw") <> " links to " <> (home </> "drive") <> ", which does not exist. If it is on a drive, mount the drive and try again.\n")

{- Keys on a terminal -}

-- | What a menu shows last, once it is ready for keys
menuShown :: Text
menuShown = "Enter confirms."

-- The input and the error output of hw are a terminal here: each question
-- is a menu. Down is the escape sequence of its arrow key. The colours
-- are off, so that the rows read as text.
keyboardSpec :: Spec
keyboardSpec =
  describe "Plain hw, where it was never set up, on a terminal" $ do
    let atKeyboard home = hwAtKeyboard [("NO_COLOR", "1")] home []
        enter = "\r"
        down = "\ESC[B"
    it "takes btc and bch, btc, and bip44 when Enter is pressed on each menu" $
      withNewHome $ \home -> do
        _ <- atKeyboard home [(menuShown, enter), (menuShown, enter), (menuShown, enter), (menuShown, enter)]
        configKeysOf home `shouldReturn` setUpWith ["btc", "bch"] "btc" "bip44"

    -- The fourth menu lists no, then yes
    it "turns the journal for ledger on when Enter is pressed on yes in the fourth menu" $
      withNewHome $ \home -> do
        _ <- atKeyboard home [(menuShown, enter), (menuShown, enter), (menuShown, enter), (menuShown, down <> enter)]
        configKeyOf home "journal" `shouldReturn` Just (Toml.Bool True)

    -- Down to bch and Space unchecks it. Down to btctest and Space checks
    -- it. The second menu then lists btc and btctest, and the third bip44
    -- and bip84. Enter on the fourth takes no journal.
    it "checks and unchecks networks with Space, and picks the row the arrow keys moved to" $
      withNewHome $ \home -> do
        _ <-
          atKeyboard
            home
            [ (menuShown, down <> " " <> down <> " " <> enter),
              (menuShown, down <> enter),
              (menuShown, down <> enter),
              (menuShown, enter)
            ]
        configKeysOf home `shouldReturn` setUpWith ["btc", "btctest"] "btctest" "bip84"

    -- The lists have no row past their last network or type
    it "keeps the cursor on the last row of a list when Down is pressed there" $
      withNewHome $ \home -> do
        _ <-
          atKeyboard
            home
            [ (menuShown, enter),
              (menuShown, down <> down <> down <> enter),
              (menuShown, down <> down <> down <> enter),
              (menuShown, enter)
            ]
        configKeysOf home `shouldReturn` setUpWith ["btc", "bch"] "bch" "bip84"

    it "shows the networks as a checklist, btc and bch checked, with the cursor on the first" $
      withNewHome $ \home -> do
        run <- atKeyboard home [(menuShown, enter), (menuShown, enter), (menuShown, enter), (menuShown, enter)]
        runErr run
          `shouldSatisfy` Text.isInfixOf "  > [x] btc       Bitcoin\r\n    [x] bch       Bitcoin Cash\r\n    [ ] btctest   Bitcoin, test network 3\r\n"

    it "says below each menu which keys it takes" $
      withNewHome $ \home -> do
        run <- atKeyboard home [(menuShown, enter), (menuShown, enter), (menuShown, enter), (menuShown, enter)]
        runErr run `shouldSatisfy` Text.isInfixOf "   Up and Down move. Space checks or unchecks. Enter confirms.\r\n"
        runErr run `shouldSatisfy` Text.isInfixOf "   Up and Down move. Enter confirms.\r\n"

    -- Space on btc, Down, Space on bch: nothing is checked. Then Space
    -- checks bch again, where the cursor is.
    it "does not take Enter with no network checked: it says to check one, and waits" $
      withNewHome $ \home -> do
        run <-
          atKeyboard
            home
            [ (menuShown, " " <> down <> " " <> enter),
              ("Check at least one network: press Space on its row.", " " <> enter),
              (menuShown, enter),
              (menuShown, enter),
              (menuShown, enter)
            ]
        runErr run `shouldSatisfy` Text.isInfixOf "Check at least one network: press Space on its row."
        configKeysOf home `shouldReturn` setUpWith ["bch"] "bch" "bip44"

    -- ESC [ n F moves the cursor up n lines, and ESC [ 0 J clears the
    -- screen from there. The checklist is 8 networks, an empty line and
    -- its keys: 10 lines. Each of the three lists is 2 rows and the same
    -- two lines: 4 lines.
    it "leaves the answer to each question where its menu was, the menu erased" $
      withNewHome $ \home -> do
        run <- atKeyboard home [(menuShown, enter), (menuShown, down <> enter), (menuShown, enter), (menuShown, enter)]
        runErr run `shouldSatisfy` Text.isInfixOf "\ESC[10F\ESC[0J\ESC[?25h   btc, bch\r\n"
        runErr run `shouldSatisfy` Text.isInfixOf "\ESC[4F\ESC[0J\ESC[?25h   bch\r\n"
        runErr run `shouldSatisfy` Text.isInfixOf "\ESC[4F\ESC[0J\ESC[?25h   bip44\r\n"
        runErr run `shouldSatisfy` Text.isInfixOf "\ESC[4F\ESC[0J\ESC[?25h   no\r\n"

    it "is cancelled with Escape: nothing is written, and hw exits with 1" $
      withNewHome $ \home -> do
        run <- atKeyboard home [(menuShown, "\ESC")]
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "The setup was cancelled. Nothing was written."
        doesPathExist (home </> ".hw") `shouldReturn` False

    -- ESC [ ? 25 l hides the cursor and ESC [ ? 25 h shows it
    it "shows the cursor of the terminal again after the last menu" $
      withNewHome $ \home -> do
        run <- atKeyboard home [(menuShown, enter), (menuShown, enter), (menuShown, enter), (menuShown, enter)]
        snd (Text.breakOnEnd "\ESC[?25l" (runErr run)) `shouldSatisfy` Text.isInfixOf "\ESC[?25h"

    it "says what it wrote on its output, as with typed answers" $
      withNewHome $ \home -> do
        run <- atKeyboard home [(menuShown, enter), (menuShown, enter), (menuShown, enter), (menuShown, enter)]
        take 5 (Text.lines (runOut run))
          `shouldBe` [ "hw is set up. Your settings were written to " <> Text.pack (configOf home),
                       "shown-networks : btc, bch",
                       "default-network: btc",
                       "default-type   : bip44",
                       "journal        : false"
                     ]
