{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A menu of hw, before anything is shown: the keys it reads from the
-- bytes of a terminal, what a checklist and a list make of each key, how
-- they are drawn, and how many lines of the screen they take.
-- "Haskoin.Wallet.Workflow.SetupSpec" and
-- "Haskoin.Wallet.Workflow.SetDefaultSpec" run the menus themselves, with
-- keys pressed.
module Haskoin.Wallet.Cli.Question.MenuSpec (spec) where

import Data.Text (Text)
import Haskoin (Network (..), bch, btc, btcTest4)
import Haskoin.Wallet.Cli.Question.Menu
import Haskoin.Wallet.Cli.Style (renderPlain)
import Test.Hspec

spec :: Spec
spec = do
  keysSpec
  checklistSpec
  listSpec
  screenSpec

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
