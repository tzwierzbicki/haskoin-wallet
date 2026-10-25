{-# LANGUAGE OverloadedStrings #-}

-- | The entropy of a new mnemonic, as "Haskoin.Wallet.Entropy" makes it:
-- the bits of the dice rolls a user may add to the entropy of the system,
-- the exclusive or that mixes entropies, and the parts a mnemonic is split
-- into and merged back from. Each part alone says nothing of the mnemonic:
-- all of them are needed to merge it back.
module Haskoin.Wallet.EntropySpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (toMnemonic)
import Haskoin.Util.Arbitrary (arbitraryBSn)
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.TestUtils (expectRight)
import System.Directory (doesFileExist)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  diceBitsSpec
  rolldiceSpec
  xorSpec
  splitSpec
  mergeSpec
  systemSpec
  generateSpec

{- Fixtures -}

-- | Mnemonics of BIP39's official test vectors (vectors.json of
-- trezor/python-mnemonic), named by their first and last words. Their
-- entropies repeat a byte: 0x7f, 0x80, 0xff or 0x00. Each byte of 0x7f xor
-- 0x80 is 0xff, and of 0x7f xor 0x80 xor 0xff is 0x00, so their parts
-- merge into one another.
legalYellow, letterAbove, zooWrong, abandonAbout :: Text
legalYellow = "legal winner thank year wave sausage worth useful legal winner thank yellow"
letterAbove = "letter advice cage absurd amount doctor acoustic avoid letter advice cage above"
zooWrong = "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong"
abandonAbout = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

-- | The same vectors of 32 bytes of entropy, in 24 words
legalTitle, letterBless, zooVote :: Text
legalTitle = "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth title"
letterBless = "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic bless"
zooVote = "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote"

-- | The sizes of entropy, in bytes, that a mnemonic can have
entropySizes :: [Int]
entropySizes = [16, 20, 24, 28, 32]

{- The bits of dice rolls -}

diceBitsSpec :: Spec
diceBitsSpec = do
  describe "The bits of dice rolls" $ do
    it "are two for the faces 6, 1, 2 and 3, and one for the faces 4 and 5" $ do
      base6ToWord8 [6] [] `shouldBe` ([], [False, False])
      base6ToWord8 [1] [] `shouldBe` ([], [False, True])
      base6ToWord8 [2] [] `shouldBe` ([], [True, False])
      base6ToWord8 [3] [] `shouldBe` ([], [True, True])
      base6ToWord8 [4] [] `shouldBe` ([], [False])
      base6ToWord8 [5] [] `shouldBe` ([], [True])

    -- A die shows each face with the same chance. So the four faces of two
    -- bits must give each pair of bits once, and the two faces of one bit
    -- each bit once: then every bit is a zero or a one with the same chance.
    it "are a zero or a one with the same chance" $ do
      sort (filter ((== 2) . length) (map base6ToBool [1 .. 6]))
        `shouldBe` [[False, False], [False, True], [True, False], [True, True]]
      sort (filter ((== 1) . length) (map base6ToBool [1 .. 6]))
        `shouldBe` [[False], [True]]

    it "are kept while they make less than a byte" $ do
      base6ToWord8 [5, 6] [] `shouldBe` ([], [True, False, False])
      base6ToWord8 [6, 6, 6, 5] [] `shouldBe` ([], [False, False, False, False, False, False, True])

    it "make a byte of every eight, the first one the highest" $ do
      base6ToWord8 [6, 6, 6, 6] [] `shouldBe` ([0x00], [])
      base6ToWord8 [6, 6, 6, 1] [] `shouldBe` ([0x01], [])
      base6ToWord8 [3, 3, 3, 3] [] `shouldBe` ([0xff], [])

    it "make the bytes in the order of the rolls" $
      base6ToWord8 [1, 1, 1, 1, 3, 3, 3, 3] [] `shouldBe` ([0x55, 0xff], [])

    it "make a byte of the last eight, and keep the ones before for the next byte" $
      base6ToWord8 [4, 3, 3, 3, 3] [] `shouldBe` ([0xff], [False])

    -- hw passes each new roll with the bits kept from the rolls before. The
    -- order of the bits changes nothing to their chances, but the same rolls
    -- must always give the same entropy.
    it "start with those of the new roll, then come those kept from the rolls before" $ do
      base6ToWord8 [1] [True] `shouldBe` ([], [False, True, True])
      base6ToWord8 [5] [False, False, False, False, False, False, False] `shouldBe` ([0x80], [])

    -- hw asks for a roll again until it is a face of a die, so no command
    -- reaches this error
    it "stop the program for a face that no die has" $ do
      evaluate (base6ToBool 0) `shouldThrow` errorCall "Invalid base6ToBool"
      evaluate (base6ToBool 7) `shouldThrow` errorCall "Invalid base6ToBool"

  describe "The bits of a byte" $ do
    it "make the byte, the first bit the highest" $ do
      boolsToWord8 [True, False, False, False, False, False, False, False] `shouldBe` 0x80
      boolsToWord8 [False, False, False, False, False, False, False, True] `shouldBe` 0x01
      boolsToWord8 [False, True, False, True, False, True, False, True] `shouldBe` 0x55

    it "stop the program when they are not eight" $ do
      evaluate (boolsToWord8 [True, True, True, True, True, True, True]) `shouldThrow` errorCall "Invalid boolsToWord8"
      evaluate (boolsToWord8 [True, True, True, True, True, True, True, True, True]) `shouldThrow` errorCall "Invalid boolsToWord8"

    -- No code uses packBools. The tests keep it exact while it stays in the
    -- module.
    it "pack into bytes by eight, in order" $ do
      packBools [] `shouldBe` BS.empty
      packBools [False, False, False, False, False, False, False, False, True, True, True, True, True, True, True, True]
        `shouldBe` BS.pack [0x00, 0xff]

    it "stop the program when they do not pack into whole bytes" $
      evaluate (packBools [True, True, True, True, True, True, True]) `shouldThrow` errorCall "Invalid packBools length"

{- The dice rolls of a byte -}

-- No command uses this mapping since the rolldice command was removed. The
-- tests keep it exact while it stays in the module.
rolldiceSpec :: Spec
rolldiceSpec =
  describe "The dice rolls of a random byte, as the former rolldice command gave them" $ do
    it "are three for a byte up to 0xd7: its digits in base 6, the face 6 for zero" $ do
      word8ToBase6 0x00 `shouldBe` [6, 6, 6]
      word8ToBase6 0x01 `shouldBe` [6, 6, 1]
      word8ToBase6 0xd7 `shouldBe` [5, 5, 5]
      map word8ToBase6 [0x00 .. 0xd7] `shouldBe` threeRolls

    it "are two for a byte from 0xd8 to 0xfb: the digits of its distance from 0xd8" $ do
      word8ToBase6 0xd8 `shouldBe` [6, 6]
      word8ToBase6 0xd9 `shouldBe` [6, 1]
      word8ToBase6 0xfb `shouldBe` [5, 5]
      map word8ToBase6 [0xd8 .. 0xfb] `shouldBe` twoRolls

    it "are none for a byte above 0xfb, which would make some rolls likelier than others" $
      map word8ToBase6 [0xfc .. 0xff] `shouldBe` [[], [], [], []]
  where
    -- The faces in the order of their value in base 6, the face 6 for zero
    faces = [6, 1, 2, 3, 4, 5]
    -- Every three rolls once, in order: the 216 bytes up to 0xd7 give each
    -- of them with the same chance. The 36 bytes from 0xd8 give every two
    -- rolls once.
    threeRolls = [[a, b, c] | a <- faces, b <- faces, c <- faces]
    twoRolls = [[a, b] | a <- faces, b <- faces]

{- The exclusive or of entropies -}

xorSpec :: Spec
xorSpec = do
  describe "The exclusive or of two entropies" $ do
    it "gives each bit the exclusive or of the two bits" $ do
      xorBytes (BS.pack [0x00]) (BS.pack [0x00]) `shouldBe` BS.pack [0x00]
      xorBytes (BS.pack [0x00]) (BS.pack [0xff]) `shouldBe` BS.pack [0xff]
      xorBytes (BS.pack [0xff]) (BS.pack [0x00]) `shouldBe` BS.pack [0xff]
      xorBytes (BS.pack [0xff]) (BS.pack [0xff]) `shouldBe` BS.pack [0x00]
      xorBytes (BS.pack [0xaa]) (BS.pack [0x55]) `shouldBe` BS.pack [0xff]

    it "goes byte by byte, in order" $ do
      xorBytes (BS.pack [0x55, 0xaa]) (BS.pack [0xaa, 0x55]) `shouldBe` BS.pack [0xff, 0xff]
      xorBytes (BS.pack [0x7a, 0x54]) (BS.pack [0xd3, 0x8e]) `shouldBe` BS.pack [0xa9, 0xda]

    -- The error stops the program: a caller must check the lengths first
    it "stops the program for entropies of different lengths" $ do
      evaluate (xorBytes BS.empty (BS.pack [0x00])) `shouldThrow` errorCall "Entropy is not of the same length"
      evaluate (xorBytes (BS.pack [0x00]) BS.empty) `shouldThrow` errorCall "Entropy is not of the same length"
      evaluate (xorBytes (BS.pack [0x00, 0x00]) (BS.pack [0x00])) `shouldThrow` errorCall "Entropy is not of the same length"
      evaluate (xorBytes (BS.pack [0x00]) (BS.pack [0x00, 0x00])) `shouldThrow` errorCall "Entropy is not of the same length"

  describe "The exclusive or of several entropies" $ do
    it "combines them all" $
      xorAll [BS.pack [0x7f, 0x01], BS.pack [0x80, 0x02], BS.pack [0xff, 0x04]] `shouldBe` BS.pack [0x00, 0x07]

    it "is the entropy itself for one entropy" $
      xorAll [BS.pack [0x7f, 0x01]] `shouldBe` BS.pack [0x7f, 0x01]

    it "is empty for no entropy" $
      xorAll [] `shouldBe` BS.empty

{- Splitting the entropy -}

-- | A secret of 32 bytes, and random parts of its length that split it into
-- the given number of parts
splitOf :: Int -> Gen (BS.ByteString, [BS.ByteString])
splitOf parts = (,) <$> arbitraryBSn 32 <*> vectorOf (parts - 1) (arbitraryBSn 32)

-- | A secret of 16 bytes
secret16 :: BS.ByteString
secret16 = BS.pack [0x00 .. 0x0f]

splitSpec :: Spec
splitSpec =
  describe "A secret split into parts" $ do
    it "is the secret alone without random parts" $
      splitEntropyWith (BS.pack [0x00]) [] `shouldBe` [BS.pack [0x00]]

    it "starts with the part that makes the secret with the random parts" $ do
      splitEntropyWith (BS.pack [0x00]) [BS.pack [0x00]] `shouldBe` [BS.pack [0x00], BS.pack [0x00]]
      splitEntropyWith (BS.pack [0x55]) [BS.pack [0xaa]] `shouldBe` [BS.pack [0xff], BS.pack [0xaa]]
      splitEntropyWith (BS.pack [0x55, 0xaa]) [BS.pack [0xaa, 0x55]] `shouldBe` [BS.pack [0xff, 0xff], BS.pack [0xaa, 0x55]]

    -- These two rules make the first part the secret xor the random parts:
    -- the only part that gives the secret back with them
    forM_ [2, 3] $ \parts -> do
      prop ("gives out the random parts as they are, after the first part, in " <> show parts <> " parts") $
        forAll (splitOf parts) $ \(secret, randoms) ->
          drop 1 (splitEntropyWith secret randoms) `shouldBe` randoms

      prop ("gives back the secret from the exclusive or of its " <> show parts <> " parts") $
        forAll (splitOf parts) $ \(secret, randoms) ->
          xorAll (splitEntropyWith secret randoms) `shouldBe` secret

    it "comes in the number of parts asked for, each of the length of the secret" $ do
      (map BS.length <$> splitEntropy 2 secret16) `shouldReturn` [16, 16]
      (map BS.length <$> splitEntropy 3 secret16) `shouldReturn` [16, 16, 16]
      (map BS.length <$> splitEntropy 5 secret16) `shouldReturn` [16, 16, 16, 16, 16]

    it "comes in parts that give back the secret" $ do
      parts <- splitEntropy 3 secret16
      xorAll parts `shouldBe` secret16

    -- Two splits with the same parts would take the same 16 random bytes
    -- twice: one chance in 2^128
    it "comes in new parts at every split" $ do
      first <- splitEntropy 2 secret16
      second <- splitEntropy 2 secret16
      first `shouldNotBe` second

    -- hw splits a new mnemonic only into two parts or more, so no command
    -- reaches this error
    it "stops the program for fewer than two parts" $ do
      splitEntropy 1 secret16 `shouldThrow` errorCall "splitEntropy invalid number"
      splitEntropy 0 secret16 `shouldThrow` errorCall "splitEntropy invalid number"

{- Merging the parts of a mnemonic -}

mergeSpec :: Spec
mergeSpec =
  describe "The parts of a mnemonic" $ do
    it "merge into the mnemonic of the exclusive or of their entropies" $
      mergeMnemonicParts [legalYellow, letterAbove] `shouldBe` Right zooWrong

    it "merge by three" $
      mergeMnemonicParts [legalYellow, letterAbove, zooWrong] `shouldBe` Right abandonAbout

    it "merge with 24 words" $
      mergeMnemonicParts [legalTitle, letterBless] `shouldBe` Right zooVote

    -- hw asks for "part 1 of 2", but the user may have written the parts
    -- down in any order
    it "merge whatever their order" $ do
      mergeMnemonicParts [letterAbove, legalYellow] `shouldBe` Right zooWrong
      mergeMnemonicParts [zooWrong, legalYellow, letterAbove] `shouldBe` Right abandonAbout

    prop "merge back into the mnemonic of the entropy they were split from, whatever its size" $
      forAll (elements entropySizes >>= arbitraryBSn) $ \secret ->
        forAll (arbitraryBSn (BS.length secret)) $ \random -> do
          parts <- expectRight $ mapM toMnemonic (splitEntropyWith secret [random])
          mergeMnemonicParts parts `shouldBe` toMnemonic secret

    it "are refused when there are fewer than two" $ do
      mergeMnemonicParts [] `shouldBe` Left "Only one mnemonic provided"
      mergeMnemonicParts [legalYellow] `shouldBe` Left "Only one mnemonic provided"

    -- Two equal parts would cancel each other out of the merge
    it "are refused when two of them are the same" $ do
      mergeMnemonicParts [legalYellow, legalYellow] `shouldBe` Left "Two mnemonics are identical"
      mergeMnemonicParts [legalYellow, letterAbove, legalYellow] `shouldBe` Left "Two mnemonics are identical"

    -- The message is haskoin-core's. The user never sees it: hw reads each
    -- part before the merge, and tells of a mistyped part then.
    it "are refused when one of them is not a mnemonic" $
      mergeMnemonicParts [legalYellow, "legal winner thank year wave sausage worth useful legal winner thank zoo"]
        `shouldSatisfy` isLeft

{- The entropy of the system -}

systemSpec :: Spec
systemSpec =
  describe "The entropy of the system" $ do
    it "has the number of bytes asked for" $
      forM_ entropySizes $ \bytes -> do
        (_, entropy) <- systemEntropy (fromIntegral bytes)
        BS.length entropy `shouldBe` bytes

    -- Two readings of the same 32 bytes: one chance in 2^256
    it "is new at every reading" $ do
      (_, first) <- systemEntropy 32
      (_, second) <- systemEntropy 32
      first `shouldNotBe` second

    -- The machine that runs the tests decides which source they see
    it "comes from /dev/random where the machine has it, and from the entropy package elsewhere" $ do
      hasDevRandom <- doesFileExist "/dev/random"
      (fst <$> systemEntropy 16)
        `shouldReturn` (if hasDevRandom then "/dev/random" else "System.Entropy.getEntropy")

    it "reads the number of bytes asked for from /dev/random" $ do
      hasDevRandom <- doesFileExist "/dev/random"
      if hasDevRandom
        then (BS.length <$> devRandom 32) `shouldReturn` 32
        else pendingWith "This machine has no /dev/random"

{- Generating a mnemonic -}

-- | A new mnemonic of the entropy of the system, without dice rolls, which
-- hw asks for on the terminal; with its parts when it is split
generated :: Int -> Int -> IO (Text, [Text])
generated bytes parts = do
  (_, mnem, splitMnems) <- expectRight =<< runExceptT (genMnemonic (fromIntegral bytes) False (fromIntegral parts))
  return (mnem, splitMnems)

generateSpec :: Spec
generateSpec =
  describe "A new mnemonic" $ do
    it "has 12, 15, 18, 21 or 24 words for 16, 20, 24, 28 or 32 bytes of entropy" $
      forM_ (zip entropySizes [12, 15, 18, 21, 24]) $ \(bytes, wordCount) -> do
        (mnem, _) <- generated bytes 1
        length (Text.words mnem) `shouldBe` wordCount

    -- Two mnemonics of the same 16 random bytes: one chance in 2^128
    it "is new every time" $ do
      (first, _) <- generated 16 1
      (second, _) <- generated 16 1
      first `shouldNotBe` second

    -- hw shows the source with the mnemonic
    it "names the source of its entropy, the one of the system" $ do
      (source, _) <- systemEntropy 16
      (named, _, _) <- expectRight =<< runExceptT (genMnemonic 16 False 1)
      named `shouldBe` source

    it "comes in parts of as many words as it has" $ do
      (_, parts) <- generated 32 2
      map (length . Text.words) parts `shouldBe` [24, 24]

    it "comes without parts when it is not split" $ do
      (_, parts) <- generated 16 1
      parts `shouldBe` []

    it "comes with the number of parts asked for, which merge into it" $ do
      (mnem, parts) <- generated 16 3
      length parts `shouldBe` 3
      mergeMnemonicParts parts `shouldBe` Right mnem

    it "is refused for another size of entropy" $
      forM_ [12, 17, 36] $ \bytes ->
        runExceptT (genMnemonic bytes False 1) `shouldReturn` Left "The entropy value can only be in [16,20..32]"

    it "is refused when split into no part" $
      runExceptT (genMnemonic 16 False 0) `shouldReturn` Left "Invalid split option"
