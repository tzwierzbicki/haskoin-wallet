{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The setup of hw, before anything is asked: the keys a menu reads from
-- the bytes of a terminal, what a checklist and a list make of each key,
-- how they are drawn, how many lines of the screen they take, a typed
-- answer to the question about the networks, the rows of the four
-- questions, and what the setup writes and says once they are answered.
-- "Haskoin.Wallet.Workflow.SetupSpec" runs the setup itself, with keys
-- pressed and lines typed.
module Haskoin.Wallet.Cli.SetupSpec (spec) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Network (..), bch, btc, btcTest4)
import Haskoin.Wallet.Cli.Setup
import Haskoin.Wallet.Cli.Style (Output (..), putDoc, renderPlain)
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils (inScratchDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import Test.Hspec
import qualified Toml

spec :: Spec
spec = do
  keysSpec
  checklistSpec
  listSpec
  screenSpec
  typedSpec
  questionsSpec
  writtenSpec

{- Keys -}

keysSpec :: Spec
keysSpec =
  describe "The keys a menu reads from a terminal" $ do
    it "are Up and Down for the escape sequences of the arrow keys" $
      keysOf "\ESC[A\ESC[B" `shouldBe` [KeyUp, KeyDown]

    it "are Up and Down for the arrow keys of a terminal in application mode" $
      keysOf "\ESCOA\ESCOB" `shouldBe` [KeyUp, KeyDown]

    it "are Up and Down for k and j, as in less" $
      keysOf "kj" `shouldBe` [KeyUp, KeyDown]

    it "are Space for the space bar" $
      keysOf " " `shouldBe` [KeySpace]

    it "are Enter for a carriage return or a line feed" $ do
      keysOf "\r" `shouldBe` [KeyEnter]
      keysOf "\n" `shouldBe` [KeyEnter]

    it "are a cancel for Escape alone" $
      keysOf "\ESC" `shouldBe` [KeyCancel]

    it "are a cancel for Ctrl-D" $
      keysOf "\EOT" `shouldBe` [KeyCancel]

    -- The sequence of the Right arrow ends with C, which is no key typed
    it "are one key to ignore for the Right arrow, whose sequence is read whole" $
      keysOf "\ESC[C" `shouldBe` [KeyOther]

    -- The Delete key sends a parameter before the byte that ends its
    -- sequence
    it "are one key to ignore for a sequence with a parameter" $
      keysOf "\ESC[3~" `shouldBe` [KeyOther]

    it "are a key to ignore for a letter no menu takes" $
      keysOf "x" `shouldBe` [KeyOther]

    it "come in the order they were pressed" $
      keysOf "\ESC[B \r" `shouldBe` [KeyDown, KeySpace, KeyEnter]

    it "are none for no byte" $
      keysOf "" `shouldBe` []

    -- A slow link may deliver an arrow key in two parts
    it "may be cut short after Escape, or after what opens a sequence" $
      (endsInsideEscape <$> ["\ESC", "\ESC[", "\ESCO"]) `shouldBe` [True, True, True]

    it "are whole after an arrow key or a letter" $
      (endsInsideEscape <$> ["\ESC[A", "x", ""]) `shouldBe` [False, False, False]

{- A checklist -}

-- | Three networks to check, the first two checked, as the first
-- question has them
threeNetworks :: [Row Network]
threeNetworks =
  [ Row btc "btc" "Bitcoin" True,
    Row bch "bch" "Bitcoin Cash" True,
    Row btcTest4 "btctest4" "Bitcoin, test network 4" False
  ]

-- | A menu after the keys, pressed in their order
afterKeys :: (Key -> Menu a -> Menu a) -> [Key] -> Menu a -> Menu a
afterKeys pressed keys menu = foldl (flip pressed) menu keys

-- | The names of the rows of a checklist that are checked
checkedNames :: Menu a -> [Text]
checkedNames menu = [row.rowName | row <- checkedRows menu]

checklistSpec :: Spec
checklistSpec =
  describe "A checklist" $ do
    let menu = menuOf threeNetworks
    it "starts with the cursor on its first row" $
      menuCursor menu `shouldBe` 0

    it "moves the cursor down a row with Down" $
      menuCursor (afterKeys pressedOnChecklist [KeyDown] menu) `shouldBe` 1

    it "moves the cursor back up with Up" $
      menuCursor (afterKeys pressedOnChecklist [KeyDown, KeyDown, KeyUp] menu) `shouldBe` 1

    -- Three rows, numbered from 0
    it "keeps the cursor on its last row when Down is pressed there" $
      menuCursor (afterKeys pressedOnChecklist [KeyDown, KeyDown, KeyDown, KeyDown] menu) `shouldBe` 2

    it "keeps the cursor on its first row when Up is pressed there" $
      menuCursor (afterKeys pressedOnChecklist [KeyUp] menu) `shouldBe` 0

    it "unchecks the checked row under the cursor with Space, and no other" $
      checkedNames (afterKeys pressedOnChecklist [KeySpace] menu) `shouldBe` ["bch"]

    it "checks the unchecked row under the cursor with Space, and no other" $
      checkedNames (afterKeys pressedOnChecklist [KeyDown, KeyDown, KeySpace] menu) `shouldBe` ["btc", "bch", "btctest4"]

    it "has the row as it was after Space is pressed twice on it" $
      afterKeys pressedOnChecklist [KeySpace, KeySpace] menu `shouldBe` menu

    it "does not move its cursor when Space is pressed" $
      menuCursor (afterKeys pressedOnChecklist [KeyDown, KeySpace] menu) `shouldBe` 1

    it "is left as it is by a key it does not take" $
      afterKeys pressedOnChecklist [KeyOther] menu `shouldBe` menu

    it "is answered with its checked rows, in their order" $
      (map rowName <$> someChecked menu) `shouldBe` Right ["btc", "bch"]

    -- hw would then show nothing
    it "is not answered with no row checked, and says how to check one" $
      (map rowName <$> someChecked (afterKeys pressedOnChecklist [KeySpace, KeyDown, KeySpace] menu))
        `shouldBe` Left "Check at least one network: press Space on its row."

    it "is drawn a row on a line, with > on the row under the cursor and [x] on the checked ones, then the keys it takes" $
      (renderPlain <$> checklistLines (afterKeys pressedOnChecklist [KeyDown] menu))
        `shouldBe` [ "    [x] btc       Bitcoin",
                     "  > [x] bch       Bitcoin Cash",
                     "    [ ] btctest4  Bitcoin, test network 4",
                     "",
                     "   Up and Down move. Space checks or unchecks. Enter confirms."
                   ]

{- A list -}

listSpec :: Spec
listSpec =
  describe "A list of which one row is picked" $ do
    let menu = menuOf threeNetworks
    it "is answered with its first row when Enter is pressed at once" $
      (rowName <$> pickedRow menu) `shouldBe` Right "btc"

    it "is answered with the row the cursor was moved to" $
      (rowName <$> pickedRow (afterKeys pressedOnList [KeyDown, KeyDown] menu)) `shouldBe` Right "btctest4"

    it "is left as it is by Space: it has nothing to check" $
      afterKeys pressedOnList [KeySpace] menu `shouldBe` menu

    it "is drawn a row on a line, with > on the row under the cursor and no box, then the keys it takes" $
      (renderPlain <$> listLines (afterKeys pressedOnList [KeyDown] menu))
        `shouldBe` [ "    btc       Bitcoin",
                     "  > bch       Bitcoin Cash",
                     "    btctest4  Bitcoin, test network 4",
                     "",
                     "   Up and Down move. Enter confirms."
                   ]

{- The screen -}

-- hw moves the cursor up by these lines to draw a menu over itself
screenSpec :: Spec
screenSpec =
  describe "The lines of the screen a menu takes" $ do
    it "are one for each of its lines on a terminal wide enough" $
      screenLines (Just 80) [40, 0, 59] `shouldBe` 3

    -- 40 columns on 30 take 2 lines, the empty line 1, and 59 columns 2
    it "are more for a line wider than the terminal, which wraps" $
      screenLines (Just 30) [40, 0, 59] `shouldBe` 5

    it "are one for a line that fills the terminal exactly" $
      screenLines (Just 40) [40] `shouldBe` 1

    it "are one for each of its lines when the width of the terminal is not known" $
      screenLines Nothing [200, 0] `shouldBe` 2

{- Typed answers -}

-- | The names of the networks a typed answer to the first question gives
typedNetworks :: Text -> Either String [String]
typedNetworks typed = map (.name) <$> readChecked networksQuestion.questionRows typed

typedSpec :: Spec
typedSpec =
  describe "A typed answer to the question about the networks" $ do
    it "is btc and bch when empty, the networks checked by default" $
      typedNetworks "" `shouldBe` Right ["btc", "bch"]

    it "names the networks to show, separated by spaces" $
      typedNetworks "btc btctest4" `shouldBe` Right ["btc", "btctest4"]

    it "names them separated by commas too" $
      typedNetworks "bch, btctest4" `shouldBe` Right ["bch", "btctest4"]

    it "gives them in the order of the networks of a portfolio, each once, whatever the order typed" $
      typedNetworks "btctest4 btc btc" `shouldBe` Right ["btc", "btctest4"]

    it "reads a name in any case" $
      typedNetworks "BTC" `shouldBe` Right ["btc"]

    it "is refused with a name that is no network, with the names there are" $
      typedNetworks "btc doge"
        `shouldBe` Left "\"doge\" is none of btc, bch, btctest, btctest4, btcreg, bchtest, bchtest4, bchreg"

{- The questions -}

questionsSpec :: Spec
questionsSpec =
  describe "The questions of the setup" $ do
    it "offer the eight networks of a portfolio, with btc and bch checked" $
      [(row.rowName, row.rowChecked) | row <- networksQuestion.questionRows]
        `shouldBe` [ ("btc", True),
                     ("bch", True),
                     ("btctest", False),
                     ("btctest4", False),
                     ("btcreg", False),
                     ("bchtest", False),
                     ("bchtest4", False),
                     ("bchreg", False)
                   ]

    it "say what each network is, after its name" $
      (rowNote <$> networksQuestion.questionRows)
        `shouldBe` [ "Bitcoin",
                     "Bitcoin Cash",
                     "Bitcoin, test network 3 (deprecated)",
                     "Bitcoin, test network 4",
                     "Bitcoin, a test chain of your own",
                     "Bitcoin Cash, test network 3 (deprecated)",
                     "Bitcoin Cash, test network 4",
                     "Bitcoin Cash, a test chain of your own"
                   ]

    -- The default network must be one hw shows
    it "offer the networks just chosen as the default network, in their order, and no other" $
      (rowName <$> (defaultNetworkQuestion [bch, btcTest4]).questionRows) `shouldBe` ["bch", "btctest4"]

    it "give the network a row of the default network names" $
      ((.name) . rowValue <$> (defaultNetworkQuestion [bch, btcTest4]).questionRows) `shouldBe` ["bch", "btctest4"]

    it "ask for the default network with the networks to choose from, the last after an or" $
      (defaultNetworkQuestion [btc, bch, btcTest4]).questionPrompt `shouldBe` "Default network: btc, bch or btctest4"

    -- The one network shown is the only default there can be
    it "ask for the default network with one name when one network is shown" $
      (defaultNetworkQuestion [bch]).questionPrompt `shouldBe` "Default network: bch"

    it "offer bip44 and bip84 as the default type of address, bip44 first" $
      [(row.rowName, row.rowValue) | row <- defaultTypeQuestion.questionRows]
        `shouldBe` [("bip44", Bip44), ("bip84", Bip84)]

    -- hw always has a default network and a default type
    it "offer no way to choose no default network or no default type" $
      filter (== "none") ((rowName <$> (defaultNetworkQuestion [btc, bch]).questionRows) <> (rowName <$> defaultTypeQuestion.questionRows))
        `shouldBe` []

    -- hw keeps no journal unless its user asks, so no is what Enter takes
    it "offer no and yes for the journal, no first" $
      [(row.rowName, row.rowValue) | row <- journalQuestion.questionRows]
        `shouldBe` [("no", False), ("yes", True)]

    -- The word ledger alone could be taken for something else
    it "name ledger as the accounting program in the title of the question about the journal" $
      journalQuestion.questionTitle `shouldBe` "4. Should hw keep a journal for ledger, the accounting program?"

    -- Most users of a wallet have never met ledger
    it "say what ledger is, where it comes from and what it is for, before anything about hw" $
      journalQuestion.questionWhy
        `shouldSatisfy` Text.isPrefixOf
          "ledger is a free accounting program of the command line (ledger-cli.org). \
          \It keeps books by double entry from a text file you write, the journal. \
          \Its reports say what each account holds, what you earned and what you spent, over any period. "

    it "say that exportjournal still writes the journal for a user who answers no" $
      journalQuestion.questionWhy `shouldSatisfy` Text.isSuffixOf "With no, hw exportjournal still writes the journal when you ask."

    it "ask for the journal with no and yes to choose from" $
      journalQuestion.questionPrompt `shouldBe` "Keep a journal for ledger: no or yes"

{- What the setup writes -}

writtenSpec :: Spec
writtenSpec =
  describe "What the setup writes" $ do
    -- btc and btctest4 shown, btctest4 the default network, bip84 the
    -- default type, and a journal kept
    let choices = Choices [btc, btcTest4] btcTest4 Bip84 True
        -- What the setup says of these files and choices, line by line
        saidOf = lines . renderPlain . doneDoc "/home/someone/.hw/config.toml" "/home/someone/.hw/portfolios.ledger"
    it "is the four keys of the configuration file the questions are about, each with what was chosen" $
      choiceKeys choices
        `shouldBe` [ ("default-network", "btctest4"),
                     ("shown-networks", Toml.List ["btc", "btctest4"]),
                     ("default-type", "bip84"),
                     ("journal", Toml.Bool True)
                   ]

    it "is shown key by key, in the order of the questions" $
      choiceRows choices
        `shouldBe` [ ("shown-networks", "btc, btctest4"),
                     ("default-network", "btctest4"),
                     ("default-type", "bip84"),
                     ("journal", "true")
                   ]

    it "is said with the file it went to, and with the two ways to change it later" $
      take 7 (saidOf (Choices [btc, bch] btc Bip84 False))
        `shouldBe` [ "hw is set up. Your settings were written to /home/someone/.hw/config.toml",
                     "shown-networks : btc, bch",
                     "default-network: btc",
                     "default-type   : bip84",
                     "journal        : false",
                     "",
                     "To change them later, run hw setdefault, or edit that file. It lists every setting of hw, with its default."
                   ]

    -- The user includes that file in a journal of their own, so the setup
    -- gives its path. The setup writes no journal itself.
    it "is said with where the journal will be, and that the next command writes it, when a journal was asked for" $
      take 3 (drop 4 (saidOf (Choices [btc, bch] btc Bip84 True)))
        `shouldBe` [ "journal        : true",
                     "Your next command writes the journal for ledger to /home/someone/.hw/portfolios.ledger",
                     ""
                   ]

    -- A new user has no wallet yet, so the first two steps make one
    it "ends with the commands to run next: a new wallet, the wallet of a mnemonic, the help" $
      drop 7 (saidOf (Choices [btc, bch] btc Bip84 False))
        `shouldBe` [ "",
                     "Next, create a wallet, with a new mnemonic to write down:",
                     "  hw newportfolio",
                     "Or create the wallet of a mnemonic you have already:",
                     "  hw newportfolio --existing",
                     "To see what else hw does:",
                     "  hw --help"
                   ]

    it "gives the same commands to run next, whatever was chosen" $
      drop 8 (saidOf (Choices [btcTest4] btcTest4 Bip44 True))
        `shouldBe` [ "",
                     "Next, create a wallet, with a new mnemonic to write down:",
                     "  hw newportfolio",
                     "Or create the wallet of a mnemonic you have already:",
                     "  hw newportfolio --existing",
                     "To see what else hw does:",
                     "  hw --help"
                   ]

    -- Cyan, the colour of a command everywhere in hw: 36 is its code
    it "names setdefault in the colour of a command" $
      inScratchDirectory $ \dir -> do
        withFile (dir </> "said") WriteMode $ \h ->
          putDoc h Styled (doneDoc "/home/someone/.hw/config.toml" "/home/someone/.hw/portfolios.ledger" choices)
        said <- BS.readFile (dir </> "said")
        said `shouldSatisfy` BS.isInfixOf "\ESC[0;36mhw setdefault\ESC[0m"
