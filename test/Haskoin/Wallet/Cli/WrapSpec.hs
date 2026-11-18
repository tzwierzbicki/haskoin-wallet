{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.Cli.WrapSpec (spec) where

import Haskoin.Wallet.Cli.Wrap (wrapText)
import Test.Hspec

spec :: Spec
spec =
  describe "A text hw writes for a person, wrapped for a screen of 80 columns" $ do
    -- The line has 80 characters
    it "keeps a line that fits the screen as it is" $
      wrapText "This line has eighty characters, the width of the screen, and stays as it is now"
        `shouldBe` "This line has eighty characters, the width of the screen, and stays as it is now"

    -- The line has 81 characters: its last word goes to the next line
    it "breaks a line that is one character too wide before its last word" $
      wrapText "This line has eighty-one characters, one more than the screen, and breaks at once"
        `shouldBe` "This line has eighty-one characters, one more than the screen, and breaks at\nonce"

    it "breaks a long sentence between words, each line as full as its words allow" $
      wrapText "Error: portfolio0 is never deleted: a wallet always keeps it, and a new one would be the same portfolio, with the same keys. deletewallet deletes a whole wallet"
        `shouldBe` "Error: portfolio0 is never deleted: a wallet always keeps it, and a new one\n\
                   \would be the same portfolio, with the same keys. deletewallet deletes a whole\n\
                   \wallet"

    -- The answer is typed after the space
    it "keeps the space a question ends with, after its last word" $
      wrapText "The database holds 1 wallet and 2 portfolios. The copy holds 1 wallet and 1 portfolio, and replaces all of it. hw copies the database into auto-backups first. Replace the database? [y/N] "
        `shouldBe` "The database holds 1 wallet and 2 portfolios. The copy holds 1 wallet and 1\n\
                   \portfolio, and replaces all of it. hw copies the database into auto-backups\n\
                   \first. Replace the database? [y/N] "

    it "keeps the lines and the empty lines the text has" $
      wrapText "It is signed, and was not sent.\n\nDelete the pending transaction? [y/N] "
        `shouldBe` "It is signed, and was not sent.\n\nDelete the pending transaction? [y/N] "

    -- A command to copy: the line has 127 characters
    it "keeps a line that starts with a space whole, however long" $
      wrapText "To put the newest backup in its place, run:\n\n  hw restoredatabase /home/someone/.hw/auto-backups/portfolios-version-5-2026-11-17T19-47-49.032Z-wallets-3-portfolios-4.sqlite"
        `shouldBe` "To put the newest backup in its place, run:\n\n  hw restoredatabase /home/someone/.hw/auto-backups/portfolios-version-5-2026-11-17T19-47-49.032Z-wallets-3-portfolios-4.sqlite"

    -- The path has 95 characters: it is copied in one piece
    it "gives a word wider than the screen a line of its own, and does not cut it" $
      wrapText "The file /home/someone/a-directory-with-a-long-name/another-directory-with-a-long-name/portfolios.sqlite could not be read"
        `shouldBe` "The file\n\
                   \/home/someone/a-directory-with-a-long-name/another-directory-with-a-long-name/portfolios.sqlite\n\
                   \could not be read"
