{-# LANGUAGE OverloadedStrings #-}

-- | Setting a key in the text of a TOML file: the new value goes where
-- the old one was, or on a new line, and every other character of the
-- text stays as it is. The texts are those a user writes in the
-- configuration file of hw.
module Haskoin.Wallet.Config.TomlEditSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Wallet.Config.TomlEdit
import Test.Hspec
import qualified Toml

spec :: Spec
spec = do
  replaceSpec
  addSpec
  refusalSpec
  writingSpec

-- | A key the text sets already
replaceSpec :: Spec
replaceSpec =
  describe "Setting a key a TOML file has" $ do
    it "puts the new value where the old one was" $
      setTomlKey "unit" "sat" "unit = \"btc\"\n" `shouldBe` Right "unit = \"sat\"\n"

    it "keeps the comment after the value on its line" $
      setTomlKey "unit" "sat" "unit = \"btc\"   # I count in satoshis\n"
        `shouldBe` Right "unit = \"sat\"   # I count in satoshis\n"

    it "keeps the spaces around the equals sign as they were typed" $
      setTomlKey "unit" "sat" "unit   =\t\"btc\"\n" `shouldBe` Right "unit   =\t\"sat\"\n"

    it "leaves every other line as it was: the comments, the empty lines and the other keys" $
      setTomlKey
        "unit"
        "sat"
        ( Text.unlines
            [ "# My settings",
              "",
              "gap = 30  # my old wallet used many addresses",
              "unit = \"btc\"",
              "",
              "# The server at home",
              "host = \"http://localhost:3000/\""
            ]
        )
        `shouldBe` Right
          ( Text.unlines
              [ "# My settings",
                "",
                "gap = 30  # my old wallet used many addresses",
                "unit = \"sat\"",
                "",
                "# The server at home",
                "host = \"http://localhost:3000/\""
              ]
          )

    -- The first # is part of the address, and the second starts a comment
    it "reads a # inside a text in quotes as part of the value, not as a comment" $
      setTomlKey "host" "http://localhost:3000/" "host = \"http://store.example/#main\" # the store\n"
        `shouldBe` Right "host = \"http://localhost:3000/\" # the store\n"

    it "replaces a text written in single quotes, and writes the new one in double quotes" $
      setTomlKey "unit" "sat" "unit = 'btc'\n" `shouldBe` Right "unit = \"sat\"\n"

    it "replaces a number" $
      setTomlKey "gap" (Toml.Integer 30) "gap = 20 # addresses\n" `shouldBe` Right "gap = 30 # addresses\n"

    it "replaces a boolean" $
      setTomlKey "autobackup-labels" (Toml.Bool False) "autobackup-labels = true\n"
        `shouldBe` Right "autobackup-labels = false\n"

    it "replaces a list on one line" $
      setTomlKey "shown-networks" (networks ["btc", "bch", "btctest4"]) "shown-networks = [ \"btc\", \"bch\" ] # mine\n"
        `shouldBe` Right "shown-networks = [\"btc\", \"bch\", \"btctest4\"] # mine\n"

    it "replaces a list written on several lines with a list on one line" $
      setTomlKey
        "shown-networks"
        (networks ["btc", "bch", "btctest4"])
        ( Text.unlines
            [ "shown-networks = [",
              "  \"btc\",",
              "  \"bch\",",
              "]",
              "gap = 30"
            ]
        )
        `shouldBe` Right "shown-networks = [\"btc\", \"bch\", \"btctest4\"]\ngap = 30\n"

    it "sets a key written in quotes, and leaves its quotes" $
      setTomlKey "unit" "sat" "\"unit\" = \"btc\"\n" `shouldBe` Right "\"unit\" = \"sat\"\n"

    it "sets a key above a table, and leaves the table as it is" $
      setTomlKey "unit" "sat" "unit = \"btc\"\n\n[backups]\nunit = \"day\"\n"
        `shouldBe` Right "unit = \"sat\"\n\n[backups]\nunit = \"day\"\n"

    it "keeps the line ends of a file written on Windows" $
      setTomlKey "unit" "sat" "unit = \"btc\"\r\ngap = 30\r\n" `shouldBe` Right "unit = \"sat\"\r\ngap = 30\r\n"

    it "sets the last line of a file that does not end with the end of a line" $
      setTomlKey "unit" "sat" "gap = 30\nunit = \"btc\"" `shouldBe` Right "gap = 30\nunit = \"sat\""

-- | A key the text does not set
addSpec :: Spec
addSpec =
  describe "Setting a key a TOML file does not have" $ do
    it "adds its line at the end of the file" $
      setTomlKey "unit" "sat" "gap = 30\n" `shouldBe` Right "gap = 30\nunit = \"sat\"\n"

    it "ends the last line of the file first when it has no end" $
      setTomlKey "unit" "sat" "gap = 30" `shouldBe` Right "gap = 30\nunit = \"sat\"\n"

    it "makes the one line of an empty file" $
      setTomlKey "unit" "sat" "" `shouldBe` Right "unit = \"sat\"\n"

    -- The file hw writes shows each setting this way
    it "adds its line below the comment that shows the key with a value, and keeps the comment" $
      setTomlKey
        "unit"
        "sat"
        ( Text.unlines
            [ "# The unit of the amounts.",
              "# unit = \"btc\"",
              "",
              "# The gap.",
              "# gap = 20"
            ]
        )
        `shouldBe` Right
          ( Text.unlines
              [ "# The unit of the amounts.",
                "# unit = \"btc\"",
                "unit = \"sat\"",
                "",
                "# The gap.",
                "# gap = 20"
              ]
          )

    it "adds its line below the last of several such comments" $
      setTomlKey "unit" "sat" "# unit = \"btc\"\ngap = 30\n#unit=\"bit\"\nhost = \"http://localhost:3000/\"\n"
        `shouldBe` Right "# unit = \"btc\"\ngap = 30\n#unit=\"bit\"\nunit = \"sat\"\nhost = \"http://localhost:3000/\"\n"

    -- gaps is another key than gap
    it "does not take the comment of a key whose name only starts the same" $
      setTomlKey "gap" (Toml.Integer 30) "# gaps = 5\nunit = \"sat\"\n"
        `shouldBe` Right "# gaps = 5\nunit = \"sat\"\ngap = 30\n"

    -- The comment is about the key, and sets nothing
    it "does not take a comment that names the key without a value" $
      setTomlKey "unit" "sat" "# unit is the unit of the amounts\ngap = 30\n"
        `shouldBe` Right "# unit is the unit of the amounts\ngap = 30\nunit = \"sat\"\n"

    it "adds its line above a table when a comment there shows the key" $
      setTomlKey "unit" "sat" "# unit = \"btc\"\n\n[backups]\nkeep = 2\n"
        `shouldBe` Right "# unit = \"btc\"\nunit = \"sat\"\n\n[backups]\nkeep = 2\n"

-- | What is left as it is, with the reason
refusalSpec :: Spec
refusalSpec =
  describe "A TOML file left as it is" $ do
    -- A line added at the end would be a key of the table
    it "is one with a table and no comment that shows the key, when the key is new" $
      setTomlKey "unit" "sat" "gap = 30\n\n[backups]\nkeep = 2\n"
        `shouldBe` Left "has a table at line 3, and the key \"unit\" must come before it"

    -- A line added there would set unit in the table
    it "is one whose only comment that shows the key is below the header of a table" $
      setTomlKey "unit" "sat" "gap = 30\n[backups]\n# unit = \"btc\"\n"
        `shouldBe` Left "has a table at line 2, and the key \"unit\" must come before it"

    it "is one whose value of the key has a comment inside, which the new value would replace" $
      setTomlKey
        "shown-networks"
        (networks ["btc"])
        ( Text.unlines
            [ "gap = 30",
              "shown-networks = [",
              "  \"btc\",",
              "  \"bch\", # for the old coins",
              "]"
            ]
        )
        `shouldBe` Left "has a comment inside the value of \"shown-networks\", at line 2, which the change would remove"

    it "is one where the key names a table" $
      setTomlKey "unit" "sat" "[unit]\nname = \"btc\"\n" `shouldBe` Left "has a table named \"unit\""

    -- The second equals sign is the eighth character of the line
    it "is one that is not TOML, with the line and the column where it stops being TOML" $
      setTomlKey "unit" "sat" "gap = 30\nunit = = \"btc\"\n"
        `shouldBe` Left "is not a TOML file, at line 2, column 8: parse error: unexpected '='"

-- | How a value is written
writingSpec :: Spec
writingSpec =
  describe "The line that sets a key of a TOML file" $ do
    it "writes a text in quotes" $
      tomlLine "unit" "sat" `shouldBe` "unit = \"sat\""

    it "writes a number and a boolean as they are" $ do
      tomlLine "gap" (Toml.Integer 20) `shouldBe` "gap = 20"
      tomlLine "autobackup-labels" (Toml.Bool True) `shouldBe` "autobackup-labels = true"

    it "writes a quote inside a text with a backslash before it" $
      tomlLine "host" "http://store.example/\"main\"" `shouldBe` "host = \"http://store.example/\\\"main\\\"\""

    -- 96 characters: a line of the usual 80 columns would be cut in two
    it "writes a list on one line, whatever its length" $
      tomlLine "shown-networks" (networks ["btc", "bch", "btctest", "btctest4", "btcreg", "bchtest3", "bchtest4", "bchreg"])
        `shouldBe` "shown-networks = [\"btc\", \"bch\", \"btctest\", \"btctest4\", \"btcreg\", \"bchtest3\", \"bchtest4\", \"bchreg\"]"

    it "is read back by a file as the value it wrote" $
      setTomlKey "host" "http://store.example/\"main\"" "" `shouldBe` Right "host = \"http://store.example/\\\"main\\\"\"\n"

-- | A list of networks, by their names, as a value of a TOML file
networks :: [Text] -> Toml.Value
networks names = Toml.List (Toml.Text <$> names)
