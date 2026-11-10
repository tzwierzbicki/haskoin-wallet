{-# LANGUAGE OverloadedStrings #-}

-- | The memory of hw while a secret is typed. It ends up in no core file,
-- and no other program of the user reads it.
--
-- The tests run the hw binary of this build. They look at its process
-- while it waits for a mnemonic. They read what Linux tells of a process
-- under @/proc@, so they run on Linux only.
module Haskoin.Wallet.MemorySpec (spec) where

import Data.List (isPrefixOf)
import Haskoin.Wallet.HwTestUtils
import System.IO (IOMode (ReadMode), withFile)
import System.IO.Error (isPermissionError)
import System.Info (os)
import System.Posix.Types (ProcessID)
import System.Posix.User (getEffectiveUserID)
import Test.Hspec

spec :: Spec
spec =
  describe "The memory of hw, while it waits for a mnemonic" $ do
    -- The two zeros are the soft limit and the hard limit. A process can
    -- raise its soft limit up to the hard one, and never the hard one.
    it "leaves no core file: the limit on its size is zero, for good" $
      onLinux $ whileHwAsksForMnemonic $ \pid -> do
        limits <- readFile ("/proc/" <> show pid <> "/limits")
        [words line | line <- lines limits, "Max core file size" `isPrefixOf` line]
          `shouldBe` [["Max", "core", "file", "size", "0", "0", "bytes"]]

    it "cannot be opened by another program of the user" $
      onLinux $ notAsRoot $ whileHwAsksForMnemonic $ \pid ->
        withFile ("/proc/" <> show pid <> "/mem") ReadMode (\_ -> return ())
          `shouldThrow` isPermissionError

-- | Look at the process of hw while it asks for the mnemonic of a wallet
-- the user has already, the first thing newportfolio --existing does
whileHwAsksForMnemonic :: (ProcessID -> IO a) -> IO a
whileHwAsksForMnemonic look =
  withHome $ \home -> whileHwWaitsAt home ["newportfolio", "--existing"] "Mnemonic, or a share of a backup: " look

-- | A test of what Linux alone shows of a process
onLinux :: Expectation -> Expectation
onLinux test
  | os == "linux" = test
  | otherwise = pendingWith "Only Linux shows the limits and the memory of a process under /proc"

-- | A test that root would fail: root opens the memory of any process
notAsRoot :: Expectation -> Expectation
notAsRoot test = do
  uid <- getEffectiveUserID
  if uid == 0 then pendingWith "Root opens the memory of any process" else test
