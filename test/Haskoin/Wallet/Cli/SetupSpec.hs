{-# LANGUAGE OverloadedStrings #-}

-- | The setup of hw, once its four questions are answered: what it
-- writes and what it says. "Haskoin.Wallet.Workflow.SetupSpec" runs the
-- setup itself, with keys pressed and lines typed.
-- "Haskoin.Wallet.Cli.Question.SettingsSpec" has its questions, and
-- "Haskoin.Wallet.Cli.Question.MenuSpec" the rules of the menus.
module Haskoin.Wallet.Cli.SetupSpec (spec) where

import qualified Data.ByteString as BS
import Haskoin (bch, btc, btcTest4)
import Haskoin.Wallet.Cli.Setup
import Haskoin.Wallet.Cli.Style (Output (..), putDoc, renderPlain)
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils (inScratchDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (..), withFile)
import Test.Hspec
import qualified Toml

spec :: Spec
spec = writtenSpec

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
                     "To change them later, run hw setconfig, or edit that file. It lists every setting of hw, with its default."
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
    it "names setconfig in the colour of a command" $
      inScratchDirectory $ \dir -> do
        withFile (dir </> "said") WriteMode $ \h ->
          putDoc h Styled (doneDoc "/home/someone/.hw/config.toml" "/home/someone/.hw/portfolios.ledger" choices)
        said <- BS.readFile (dir </> "said")
        said `shouldSatisfy` BS.isInfixOf "\ESC[0;36mhw setconfig\ESC[0m"
