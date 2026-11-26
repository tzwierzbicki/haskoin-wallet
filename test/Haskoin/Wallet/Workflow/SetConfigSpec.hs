{-# LANGUAGE OverloadedStrings #-}

-- | setconfig without an option, as its user meets it: twelve questions,
-- one for each setting of the configuration file, each starting on the
-- value hw has now. They are answered with typed lines, as a script or a
-- pipe answers them, and with keys on a terminal, where nine of them are
-- menus. "Haskoin.Wallet.Cli.SetConfigSpec" has the rule of what the
-- answers change, and "Haskoin.Wallet.Cli.Question.SettingsSpec" the
-- questions.
--
-- The home of a test is set up ('withHome'): its file sets the address of
-- a server, btc as the default network, and shows the eight networks.
-- Every other setting has the default of hw.
module Haskoin.Wallet.Workflow.SetConfigSpec (spec) where

import qualified Data.ByteString as BS
import Data.Char (isDigit)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.HwTestUtils
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec
import qualified Toml

spec :: Spec
spec = do
  typedSpec
  keyboardSpec

-- | An empty line for each of the twelve questions: every setting is kept
keepAll :: [Text]
keepAll = replicate 12 ""

-- | The lines typed for the twelve questions, all empty but the one of
-- the question given, from 1, which has the answer given
answering :: Int -> Text -> [Text]
answering question typed = replicate (question - 1) "" <> [typed] <> replicate (12 - question) ""

{- Typed answers -}

-- The input of hw is no terminal here: each question is a line to type
typedSpec :: Spec
typedSpec =
  describe "setconfig without an option, with its answers typed" $ do
    it "changes nothing for twelve empty lines, and says so" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] keepAll
        runOut run `shouldBe` "Nothing changed in " <> Text.pack (configOf home) <> "\n"

    it "leaves the file as it was, byte for byte, when nothing changed" $
      withHome $ \home -> do
        earlier <- BS.readFile (configOf home)
        _ <- hw home ["setconfig"] keepAll
        BS.readFile (configOf home) `shouldReturn` earlier

    it "exits with success when nothing changed" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] keepAll
        runExit run `shouldBe` ExitSuccess

    -- The fourth question is about the unit
    it "writes the one setting whose answer differs, and no other" $
      withHome $ \home -> do
        earlier <- configKeysOf home
        _ <- hw home ["setconfig"] (answering 4 "sat")
        configKeysOf home `shouldReturn` Map.insert "unit" (Toml.Text "sat") earlier

    it "answers with the key it wrote and the file, as setconfig with an option does" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] (answering 4 "sat")
        runOut run `shouldBe` "unit: sat\nWritten to " <> Text.pack (configOf home) <> "\n"

    it "answers the same in JSON" $
      withHome $ \home -> do
        hwJson home ["setconfig"] (answering 4 "sat")
          `shouldReturn` ResponseSetConfig [("unit", "sat")] (configOf home) Nothing

    it "changes nothing for an answer that names the value hw has already" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] (answering 4 "btc")
        runOut run `shouldBe` "Nothing changed in " <> Text.pack (configOf home) <> "\n"

    -- With btc and bch shown, the first question fits on one line
    it "asks its twelve questions on the error output, each with the value hw has now in brackets" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        run <- hw home ["setconfig"] keepAll
        filter (Text.isSuffixOf "] ") (Text.lines (runErr run))
          `shouldBe` [ "Networks to show, separated by spaces [btc bch] ",
                       "Default network: btc or bch [btc] ",
                       "Default type of address: bip44 or bip84 [bip44] ",
                       "Unit of the amounts: btc, bch, bit or sat [btc] ",
                       "View of the listings: compact, verbose or tabular [compact] ",
                       "Reverse the listings: no or yes [no] ",
                       "Keep a journal for ledger: no or yes [no] ",
                       "Keep copies of the database: yes or no [yes] ",
                       "Keep copies of the labels: yes or no [yes] ",
                       "Address of the server [http://127.0.0.1:1/] ",
                       "Requests at once, from 1 to 100 [1] ",
                       "Gap, from 1 to 1000 [20] "
                     ]

    it "numbers its questions from 1 to 12, by subject" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] keepAll
        filter (maybe False (isDigit . fst) . Text.uncons) (Text.lines (runErr run))
          `shouldBe` [ "1. Which networks do you work with?",
                       "2. Which network is the default?",
                       "3. Which type of address is the default?",
                       "4. Which unit do you count in?",
                       "5. How do the listings show their elements?",
                       "6. Do the listings list their rows in the reverse order?",
                       "7. Should hw keep a journal for ledger, the accounting program?",
                       "8. Should hw keep copies of the database?",
                       "9. Should hw keep copies of your labels?",
                       "10. Which server does hw ask about the chain?",
                       "11. How many requests does hw send to the server at once?",
                       "12. How many unused addresses in a row does hw look at?"
                     ]

    it "says before the first question that Enter keeps a value, and when the answers are written" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] keepAll
        unwrapped (runErr run)
          `shouldSatisfy` Text.isPrefixOf "The settings of hw. Twelve questions go through them. Each one starts on the value hw has now, and Enter keeps it. hw writes what you changed after the last question."

    -- The file sets them, so an empty line keeps them
    it "starts each question on what the file sets, not on the default of hw" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        setUnit home "sat"
        setConfigKey home "default-type" (Toml.Text "bip84")
        setConfigKey home "gap" (Toml.Integer 50)
        run <- hw home ["setconfig"] keepAll
        filter (\line -> any (`Text.isPrefixOf` line) ["Default type", "Unit", "Gap"]) (Text.lines (runErr run))
          `shouldBe` [ "Default type of address: bip44 or bip84 [bip84] ",
                       "Unit of the amounts: btc, bch, bit or sat [sat] ",
                       "Gap, from 1 to 1000 [50] "
                     ]
        runOut run `shouldBe` "Nothing changed in " <> Text.pack (configOf home) <> "\n"

    -- bch stands for whole coins as btc does, and is a value of its own
    it "keeps a unit of bch that the file sets, for twelve empty lines" $
      withHome $ \home -> do
        setUnit home "bch"
        run <- hw home ["setconfig"] keepAll
        runOut run `shouldBe` "Nothing changed in " <> Text.pack (configOf home) <> "\n"

    -- Every answer differs from what the home has
    it "writes every setting when the twelve answers differ" $
      withHome $ \home -> do
        _ <- hw home ["setconfig"] ["btc btctest4", "btctest4", "bip84", "sat", "tabular", "yes", "yes", "no", "no", "http://localhost:3000/", "4", "50"]
        configKeysOf home
          `shouldReturn` Map.fromList
            [ ("shown-networks", Toml.List ["btc", "btctest4"]),
              ("default-network", "btctest4"),
              ("default-type", "bip84"),
              ("unit", "sat"),
              ("listing-view", "tabular"),
              ("listing-reverse", Toml.Bool True),
              ("journal", Toml.Bool True),
              ("autobackup-database", Toml.Bool False),
              ("autobackup-labels", Toml.Bool False),
              ("host", "http://localhost:3000/"),
              ("parallel-requests", Toml.Integer 4),
              ("gap", Toml.Integer 50)
            ]

    -- The keys are padded to the longest, autobackup-database, of 19
    -- letters. The journal was turned on, so the last line says where it is.
    it "answers with every key it wrote, then with where the journal is" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] ["btc btctest4", "btctest4", "bip84", "sat", "tabular", "yes", "yes", "no", "no", "http://localhost:3000/", "4", "50"]
        Text.lines (runOut run)
          `shouldBe` [ "default-network    : btctest4",
                       "default-type       : bip84",
                       "unit               : sat",
                       "listing-view       : tabular",
                       "listing-reverse    : true",
                       "journal            : true",
                       "shown-networks     : btc, btctest4",
                       "autobackup-database: false",
                       "autobackup-labels  : false",
                       "host               : http://localhost:3000/",
                       "parallel-requests  : 4",
                       "gap                : 50",
                       "Written to " <> Text.pack (configOf home),
                       "hw now keeps a journal for ledger at " <> Text.pack (home </> ".hw" </> "portfolios.ledger")
                     ]

    -- The home has no wallet, so the journal holds its two comments
    it "writes the journal at once when the seventh answer turns it on, as --journal does" $
      withHome $ \home -> do
        _ <- hw home ["setconfig"] (answering 7 "yes")
        Text.lines <$> Text.IO.readFile (home </> ".hw" </> "portfolios.ledger")
          `shouldReturn` [ "; The transactions of every portfolio of hw on the chain, as a journal for ledger.",
                           "; hw writes this file whole each time. Do not edit it: your changes would be lost."
                         ]

    it "takes y for a question of yes and no, and an answer in capitals" $
      withHome $ \home -> do
        _ <- hw home ["setconfig"] ["", "", "", "SAT", "", "y", "", "", "", "", "", ""]
        (,) <$> configKeyOf home "unit" <*> configKeyOf home "listing-reverse"
          `shouldReturn` (Just "sat", Just (Toml.Bool True))

    -- btc is the default network, and the first answer hides it. The
    -- default network must be one hw shows.
    it "asks for the default network among the networks just chosen, and writes the one that replaces a hidden default" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] (answering 1 "bch btctest4")
        runErr run `shouldSatisfy` Text.isInfixOf "Default network: bch or btctest4 [bch] "
        (,) <$> configKeyOf home "shown-networks" <*> configKeyOf home "default-network"
          `shouldReturn` (Just (Toml.List ["bch", "btctest4"]), Just "bch")

    -- A gap of zero would refuse every new address. The thirteenth line
    -- answers the twelfth question again.
    it "does not take a gap of 0: it asks again, and takes the next answer" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] (answering 12 "0" <> ["30"])
        runErr run `shouldSatisfy` Text.isInfixOf "Type a whole number from 1 to 1000\n"
        configKeyOf home "gap" `shouldReturn` Just (Toml.Integer 30)

    it "does not take a server without http:// or https://: it asks again, and takes the next answer" $
      withHome $ \home -> do
        run <- hw home ["setconfig"] (replicate 9 "" <> ["localhost:3000", "http://localhost:3000/", "", ""])
        runErr run `shouldSatisfy` Text.isInfixOf "Type the address of a server, which starts with http:// or https://\n"
        configKeyOf home "host" `shouldReturn` Just "http://localhost:3000/"

    -- The unit was answered with sat before the gap was refused three
    -- times. The fourth gap, a good one, is not read.
    it "stops after three answers it cannot read, says so, and writes nothing" $
      withHome $ \home -> do
        earlier <- configKeysOf home
        run <- hw home ["setconfig"] (["", "", "", "sat"] <> replicate 7 "" <> ["0", "1001", "many", "30"])
        runExit run `shouldBe` ExitFailure 1
        unwrapped (runOut run)
          `shouldBe` "Error: Type a whole number from 1 to 1000. hw setconfig stopped after three answers it could not read. Nothing was written."
        configKeysOf home `shouldReturn` earlier

    -- Four answers out of twelve, the fourth one a change
    it "writes nothing when the input ends before the last answer, and exits with 1" $
      withHome $ \home -> do
        earlier <- configKeysOf home
        run <- hw home ["setconfig"] ["", "", "", "sat"]
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldBe` "Error: Cancelled: the input ended before the answer\n"
        configKeysOf home `shouldReturn` earlier

    -- The file the setup writes shows the gap in a comment, with its
    -- default of 20
    it "sets a key below the comment of the file that shows its default" $
      withNewHome $ \home -> do
        _ <- hw home [] ["", "", "", ""]
        _ <- hw home ["setconfig"] (answering 12 "30")
        written <- Text.lines <$> Text.IO.readFile (configOf home)
        take 2 (dropWhile (/= "# gap = 20") written) `shouldBe` ["# gap = 20", "gap = 30"]

    it "still writes one default without a question when an option names it" $
      withHome $ \home -> do
        run <- hw home ["setconfig", "--unit", "sat"] []
        runErr run `shouldBe` ""
        configKeyOf home "unit" `shouldReturn` Just "sat"

{- Keys on a terminal -}

-- | What a menu shows last, once it is ready for keys
menuShown :: Text
menuShown = "Enter confirms."

-- The input and the error output of hw are a terminal here. The first
-- nine questions are menus, and the last three are lines to type. Down is
-- the escape sequence of its arrow key. The colours are off, so that the
-- rows read as text.
keyboardSpec :: Spec
keyboardSpec =
  describe "setconfig without an option, on a terminal" $ do
    let atKeyboard home = hwAtKeyboard [("NO_COLOR", "1")] home ["setconfig"]
        enter = "\r"
        down = "\ESC[B"
        -- Enter on each of the nine menus, but for the keys given for
        -- the menu of one question, from 1
        menusWith question keys = [(menuShown, if at == question then keys else enter) | at <- [1 .. 9 :: Int]]
        menus = menusWith 0 enter
        -- Enter on each of the three lines to type, which keeps the
        -- value in brackets
        typedLines = [("Address of the server [", enter), ("Requests at once, from 1 to 100 [", enter), ("Gap, from 1 to 1000 [", enter)]
    it "changes nothing when Enter is pressed on each of the twelve questions" $
      withHome $ \home -> do
        earlier <- configKeysOf home
        run <- atKeyboard home (menus <> typedLines)
        runOut run `shouldBe` "Nothing changed in " <> Text.pack (configOf home) <> "\n"
        configKeysOf home `shouldReturn` earlier

    -- btc and btctest4 are shown, and the six others hidden
    it "shows the networks as a checklist, with those hw shows now checked" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "btctest4"]
        run <- atKeyboard home (menus <> typedLines)
        runErr run
          `shouldSatisfy` Text.isInfixOf "  > [x] btc       Bitcoin\r\n    [ ] bch       Bitcoin Cash\r\n    [ ] btctest   Bitcoin, test network 3 (deprecated)\r\n    [x] btctest4  Bitcoin, test network 4\r\n"

    -- The file sets bip84, the second row of the third menu
    it "starts the cursor of a list on the value hw has now, which Enter keeps" $
      withHome $ \home -> do
        setConfigKey home "default-type" (Toml.Text "bip84")
        run <- atKeyboard home (menus <> typedLines)
        runErr run
          `shouldSatisfy` Text.isInfixOf "    bip44  Legacy addresses, which start with 1 on Bitcoin\r\n  > bip84  Native SegWit addresses, which start with bc1q: lower fees\r\n"
        configKeyOf home "default-type" `shouldReturn` Just "bip84"

    -- The fourth menu lists btc, bch, bit and sat: three times Down is sat
    it "writes the row the arrow keys moved to" $
      withHome $ \home -> do
        run <- atKeyboard home (menusWith 4 (down <> down <> down <> enter) <> typedLines)
        runOut run `shouldBe` "unit: sat\nWritten to " <> Text.pack (configOf home) <> "\n"

    -- Down to bch and Space unchecks it. btc stays the default network.
    it "hides the network that Space unchecks" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        _ <- atKeyboard home (menusWith 1 (down <> " " <> enter) <> typedLines)
        configKeyOf home "shown-networks" `shouldReturn` Just (Toml.List ["btc"])

    it "takes an address typed for the server, and a number typed for the gap" $
      withHome $ \home -> do
        _ <-
          atKeyboard home $
            menus
              <> [ ("Address of the server [", "http://localhost:3000/" <> enter),
                   ("Requests at once, from 1 to 100 [", enter),
                   ("Gap, from 1 to 1000 [", "50" <> enter)
                 ]
        (,) <$> configKeyOf home "host" <*> configKeyOf home "gap"
          `shouldReturn` (Just "http://localhost:3000/", Just (Toml.Integer 50))

    -- ESC [ n F moves the cursor up n lines, and ESC [ 0 J clears the
    -- screen from there. The menu of the unit is 4 rows, an empty line
    -- and its keys: 6 lines. The menu of the view is 3 rows and the same
    -- two lines: 5 lines.
    it "leaves the answer to each menu where the menu was, the menu erased" $
      withHome $ \home -> do
        run <- atKeyboard home (menus <> typedLines)
        runErr run `shouldSatisfy` Text.isInfixOf "\ESC[6F\ESC[0J\ESC[?25h   btc\r\n"
        runErr run `shouldSatisfy` Text.isInfixOf "\ESC[5F\ESC[0J\ESC[?25h   compact\r\n"

    -- The fourth menu was answered with sat before Escape
    it "is cancelled with Escape on a menu: nothing is written, and hw exits with 1" $
      withHome $ \home -> do
        earlier <- configKeysOf home
        run <- atKeyboard home (take 5 (menusWith 4 (down <> down <> down <> enter)) <> [(menuShown, "\ESC")])
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldBe` "Error: hw setconfig was cancelled. Nothing was written.\n"
        configKeysOf home `shouldReturn` earlier

    -- Ctrl-D on an empty line is the end of the input of a terminal
    it "is cancelled with Ctrl-D on a line to type, as on a menu" $
      withHome $ \home -> do
        earlier <- configKeysOf home
        run <- atKeyboard home (menus <> [("Address of the server [", "\EOT")])
        runExit run `shouldBe` ExitFailure 1
        runOut run `shouldBe` "Error: hw setconfig was cancelled. Nothing was written.\n"
        configKeysOf home `shouldReturn` earlier
