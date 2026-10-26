{-# LANGUAGE OverloadedStrings #-}

-- | The entropy of a new mnemonic, as "Haskoin.Wallet.Entropy" makes it:
-- the bits of the dice rolls a user may add, the exclusive or that mixes
-- entropies, the entropy of the system, and the parts a mnemonic is split
-- into and merged back from. Each part alone says nothing of the mnemonic:
-- all of them are needed to merge it back.
module Haskoin.Wallet.EntropySpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Either (isLeft)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (toMnemonic)
import Haskoin.Util.Arbitrary (arbitraryBSn)
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.TestUtils (expectRight, inScratchDirectory)
import Numeric.Natural (Natural)
import System.Directory (doesPathExist)
import System.FilePath ((</>))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  diceSpec
  xorSpec
  splitSpec
  mergeSpec
  systemSpec
  newMnemonicSpec
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

-- | The entropies of 16 bytes of the four mnemonics above
entropy7f, entropy80, entropyFf, entropy00 :: BS.ByteString
entropy7f = BS.replicate 16 0x7f
entropy80 = BS.replicate 16 0x80
entropyFf = BS.replicate 16 0xff
entropy00 = BS.replicate 16 0x00

-- | The same vectors of 32 bytes of entropy, in 24 words
legalTitle, letterBless, zooVote :: Text
legalTitle = "legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth useful legal winner thank year wave sausage worth title"
letterBless = "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic bless"
zooVote = "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote"

-- | The sizes of entropy, in bytes, that a mnemonic can have
entropySizes :: [Int]
entropySizes = [16, 20, 24, 28, 32]

{- The entropy of dice rolls -}

diceSpec :: Spec
diceSpec =
  describe "The entropy of dice rolls" $ do
    it "takes two bits from the faces 6, 1, 2 and 3, and one bit from the faces 4 and 5" $ do
      diceBits [6] `shouldBe` [False, False]
      diceBits [1] `shouldBe` [False, True]
      diceBits [2] `shouldBe` [True, False]
      diceBits [3] `shouldBe` [True, True]
      diceBits [4] `shouldBe` [False]
      diceBits [5] `shouldBe` [True]

    -- A die shows each face with the same chance. So the four faces of two
    -- bits must give each pair of bits once, and the two faces of one bit
    -- each bit once: then every bit is a zero or a one with the same chance.
    it "takes bits that are a zero or a one with the same chance" $ do
      sort (filter ((== 2) . length) (map (diceBits . pure) [1 .. 6]))
        `shouldBe` [[False, False], [False, True], [True, False], [True, True]]
      sort (filter ((== 1) . length) (map (diceBits . pure) [1 .. 6]))
        `shouldBe` [[False], [True]]

    it "takes the bits in the order of the rolls" $
      diceBits [6, 5, 1, 4] `shouldBe` [False, False, True, False, True, False]

    it "is not complete while the rolls give fewer bits than its bytes have" $ do
      diceEntropy 1 [5, 6, 6, 6] `shouldBe` Nothing
      diceEntropy 16 (replicate 63 6) `shouldBe` Nothing

    it "makes a byte of every eight bits, the first one the highest" $ do
      diceEntropy 1 [6, 6, 6, 6] `shouldBe` Just (BS.pack [0x00])
      diceEntropy 1 [6, 6, 6, 1] `shouldBe` Just (BS.pack [0x01])
      diceEntropy 1 [5, 6, 6, 6, 4] `shouldBe` Just (BS.pack [0x80])
      diceEntropy 1 [4, 5, 4, 5, 4, 5, 4, 5] `shouldBe` Just (BS.pack [0x55])
      diceEntropy 1 [3, 3, 3, 3] `shouldBe` Just (BS.pack [0xff])

    it "makes the bytes in the order of the rolls" $
      diceEntropy 2 [1, 1, 1, 1, 3, 3, 3, 3] `shouldBe` Just (BS.pack [0x55, 0xff])

    -- The last roll of 6 gives two zeros: the first ends the byte, the
    -- second is dropped
    it "drops the bits beyond its bytes" $ do
      diceEntropy 1 [3, 3, 3, 3, 1] `shouldBe` Just (BS.pack [0xff])
      diceEntropy 1 [3, 3, 3, 5, 6] `shouldBe` Just (BS.pack [0xfe])

    it "has the 16 bytes of a mnemonic of 12 words after 128 bits" $ do
      diceEntropy 16 (replicate 64 6) `shouldBe` Just entropy00
      diceEntropy 16 (replicate 128 5) `shouldBe` Just entropyFf

    -- hw reads the faces of a line before it takes any bit of them, so no
    -- command reaches this error
    it "stops the program for a face that no die has" $ do
      evaluate (diceBits [0]) `shouldThrow` errorCall "base6ToBool: a die has the faces 1 to 6 only"
      evaluate (diceBits [7]) `shouldThrow` errorCall "base6ToBool: a die has the faces 1 to 6 only"

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

    -- Zipping entropies of different lengths would drop bytes, so the
    -- program stops: the callers check the lengths first
    it "stops the program for entropies of different lengths" $ do
      evaluate (xorBytes BS.empty (BS.pack [0x00])) `shouldThrow` errorCall "xorBytes: the entropies do not have the same length"
      evaluate (xorBytes (BS.pack [0x00]) BS.empty) `shouldThrow` errorCall "xorBytes: the entropies do not have the same length"
      evaluate (xorBytes (BS.pack [0x00, 0x00]) (BS.pack [0x00])) `shouldThrow` errorCall "xorBytes: the entropies do not have the same length"
      evaluate (xorBytes (BS.pack [0x00]) (BS.pack [0x00, 0x00])) `shouldThrow` errorCall "xorBytes: the entropies do not have the same length"

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
      mergeMnemonicParts [] `shouldBe` Left "A mnemonic in parts has two parts or more"
      mergeMnemonicParts [legalYellow] `shouldBe` Left "A mnemonic in parts has two parts or more"

    -- Their entropies would not have the same length: merging them used to
    -- stop the program
    it "are refused when they do not have the same number of words" $
      mergeMnemonicParts [legalYellow, legalTitle] `shouldBe` Left "The parts do not have the same number of words"

    -- Two equal parts would cancel each other out of the merge
    it "are refused when two of them are the same" $ do
      mergeMnemonicParts [legalYellow, legalYellow] `shouldBe` Left "Two of the parts are the same, but every part of a mnemonic is different"
      mergeMnemonicParts [legalYellow, letterAbove, legalYellow] `shouldBe` Left "Two of the parts are the same, but every part of a mnemonic is different"

    -- The words of a part are secret: the message does not repeat them
    it "are refused when one of them is not a mnemonic, named by its position only" $ do
      mergeMnemonicParts [legalYellow, "legal winner thank year wave sausage worth useful legal winner thank zoo"]
        `shouldBe` Left "Part 2 is not a mnemonic"
      mergeMnemonicParts ["legal winner thank year wave sausage worth useful legal winner thank notaword", legalYellow]
        `shouldBe` Left "Part 1 is not a mnemonic"

{- The entropy of the system -}

-- | A test that needs a device of the machine, pending where there is none
withDevice :: FilePath -> Expectation -> Expectation
withDevice path test = do
  exists <- doesPathExist path
  if exists then test else pendingWith ("This machine has no " <> path)

-- | The source of entropy read at a path, and the number of its bytes
readAt :: FilePath -> Natural -> IO (Either String (Text, Int))
readAt path bytes = fmap (fmap BS.length) <$> runExceptT (entropyAt path bytes)

-- | The refusal of what is not a device at a path
notDevice :: FilePath -> Either String a
notDevice path = Left $ path <> " is not a device, so hw does not read it: check this machine before you make a mnemonic on it"

systemSpec :: Spec
systemSpec = do
  describe "The entropy of the system" $ do
    it "has the number of bytes asked for" $
      forM_ entropySizes $ \bytes ->
        (fmap (BS.length . snd) <$> runExceptT (systemEntropy (fromIntegral bytes))) `shouldReturn` Right bytes

    -- Two readings of the same 32 bytes: one chance in 2^256
    it "is new at every reading" $ do
      (_, first) <- expectRight =<< runExceptT (systemEntropy 32)
      (_, second) <- expectRight =<< runExceptT (systemEntropy 32)
      first `shouldNotBe` second

    it "comes from /dev/random where the machine has it" $
      withDevice "/dev/random" $
        (fmap fst <$> runExceptT (systemEntropy 16)) `shouldReturn` Right "/dev/random"

  describe "Entropy read at a path" $ do
    it "comes from the device there" $
      withDevice "/dev/random" $
        readAt "/dev/random" 16 `shouldReturn` Right ("/dev/random", 16)

    it "comes from the entropy package when nothing is there" $
      inScratchDirectory $ \dir ->
        readAt (dir </> "random") 16 `shouldReturn` Right ("System.Entropy.getEntropy", 16)

    -- As a container or a chroot may have one at /dev/random: hw read it
    -- before, and gave every mnemonic its bytes
    it "is refused from a plain file" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "random"
        BS.writeFile file (BS.pack [0x00 .. 0x3f])
        readAt file 16 `shouldReturn` notDevice file

    it "is refused from a directory" $
      inScratchDirectory $ \dir ->
        readAt dir 16 `shouldReturn` notDevice dir

    -- /dev/zero is a device, but it gives zeros only
    it "is refused from a device that gives the same byte over and over" $
      withDevice "/dev/zero" $
        readAt "/dev/zero" 16 `shouldReturn` Left patterned

    -- /dev/null is a device, but it ends at once
    it "is refused from a device that gives fewer bytes than asked for" $
      withDevice "/dev/null" $
        readAt "/dev/null" 16 `shouldReturn` Left "The operating system gave 0 bytes of entropy, not 16"

  describe "Bytes of entropy from the system" $ do
    it "are taken when there are as many as asked for" $
      checkEntropy 4 (BS.pack [0x3a, 0x00, 0xc1, 0x7f]) `shouldBe` Right (BS.pack [0x3a, 0x00, 0xc1, 0x7f])

    it "are refused when there are fewer or more than asked for" $ do
      checkEntropy 16 (BS.pack [0x00 .. 0x0e]) `shouldBe` Left "The operating system gave 15 bytes of entropy, not 16"
      checkEntropy 16 (BS.pack [0x00 .. 0x10]) `shouldBe` Left "The operating system gave 17 bytes of entropy, not 16"

  -- A rule guesses each byte from the ones before, right by chance once in
  -- 256 times for random bytes: 8 right guesses of the 15 a rule makes in
  -- 16 bytes are too many, as one byte in 9 of the 16 places
  describe "Bytes of entropy with a pattern" $ do
    it "are refused when they are one byte over and over" $ do
      checkEntropy 16 entropy00 `shouldBe` Left patterned
      checkEntropy 16 entropyFf `shouldBe` Left patterned

    it "are refused when one byte follows another over and over" $ do
      checkEntropy 16 (BS.pack [0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]) `shouldBe` Left patterned
      checkEntropy 16 (BS.pack [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01]) `shouldBe` Left patterned

    it "are refused when they count up or down, by any step" $ do
      checkEntropy 16 (BS.pack [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]) `shouldBe` Left patterned
      checkEntropy 16 (BS.pack [0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00]) `shouldBe` Left patterned
      checkEntropy 16 (BS.pack [0x00, 0x05, 0x0a, 0x0f, 0x14, 0x19, 0x1e, 0x23, 0x28, 0x2d, 0x32, 0x37, 0x3c, 0x41, 0x46, 0x4b]) `shouldBe` Left patterned

    it "are refused when they count past 0xff, back from 0x00" $
      checkEntropy 16 (BS.pack [0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]) `shouldBe` Left patterned

    it "are refused when they count in words of several bytes" $ do
      checkEntropy 16 (BS.pack [0x00, 0x00, 0x00, 0x01, 0x00, 0x02, 0x00, 0x03, 0x00, 0x04, 0x00, 0x05, 0x00, 0x06, 0x00, 0x07]) `shouldBe` Left patterned
      checkEntropy 16 (BS.pack [0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00]) `shouldBe` Left patterned

    it "are refused when a pattern repeats" $ do
      checkEntropy 16 (C8.pack "passwordpassword") `shouldBe` Left patterned
      checkEntropy 16 (BS.pack [0xa7, 0x3c, 0x91, 0x0e, 0xa7, 0x3c, 0x91, 0x0e, 0xa7, 0x3c, 0x91, 0x0e, 0xa7, 0x3c, 0x91, 0x0e]) `shouldBe` Left patterned

    it "are refused when one byte fills most of their places" $
      checkEntropy 16 (BS.pack [0x00, 0x7a, 0x00, 0x00, 0x3c, 0x00, 0x91, 0x00, 0x00, 0xe4, 0x00, 0x5b, 0x00, 0x00, 0xc8, 0x00]) `shouldBe` Left patterned

    -- The pattern of 8 bytes repeats 7 of them: random bytes do so once in
    -- 2^53 times, once in 2^49 with the 13 tests. With 6 of them, once in
    -- 2^43 times, but once in 2^39.5 with the 13 tests: short of the 2^40
    -- of a refusal.
    it "are refused when a pattern repeats 7 of its 8 bytes, but taken with 6" $ do
      checkEntropy 16 (BS.pack [0x3a, 0x91, 0xc4, 0x0e, 0x7b, 0xe2, 0x58, 0xd6, 0x3a, 0x91, 0xc4, 0x0e, 0x7b, 0xe2, 0x58, 0x17]) `shouldBe` Left patterned
      checkEntropy 16 (BS.pack [0x3a, 0x91, 0xc4, 0x0e, 0x7b, 0xe2, 0x58, 0xd6, 0x3a, 0x91, 0xc4, 0x0e, 0x7b, 0xe2, 0xa5, 0x17])
        `shouldBe` Right (BS.pack [0x3a, 0x91, 0xc4, 0x0e, 0x7b, 0xe2, 0x58, 0xd6, 0x3a, 0x91, 0xc4, 0x0e, 0x7b, 0xe2, 0xa5, 0x17])

  describe "Random bytes of entropy" $ do
    -- Read from /dev/random once, on 2026-10-26
    it "are taken" $ do
      checkEntropy 16 (BS.pack [0x8b, 0x8d, 0x1e, 0x7b, 0x36, 0x6f, 0xaf, 0xbe, 0xc9, 0xe4, 0x56, 0xdd, 0x26, 0xeb, 0xf9, 0x09])
        `shouldBe` Right (BS.pack [0x8b, 0x8d, 0x1e, 0x7b, 0x36, 0x6f, 0xaf, 0xbe, 0xc9, 0xe4, 0x56, 0xdd, 0x26, 0xeb, 0xf9, 0x09])
      checkEntropy 32 randomBytes32 `shouldBe` Right randomBytes32

    -- One byte twice in a row, and one byte three times: random bytes have
    -- such coincidences often
    it "are taken with the coincidences random bytes have" $
      checkEntropy 16 (BS.pack [0x3a, 0x3a, 0xc1, 0x7f, 0x08, 0xe2, 0x5d, 0x91, 0xc1, 0x4b, 0x07, 0xc1, 0xee, 0x26, 0xb0, 0x19])
        `shouldBe` Right (BS.pack [0x3a, 0x3a, 0xc1, 0x7f, 0x08, 0xe2, 0x5d, 0x91, 0xc1, 0x4b, 0x07, 0xc1, 0xee, 0x26, 0xb0, 0x19])

    -- Each byte chosen from the 256 with the same chance; a false alarm
    -- comes once in 2^40 times or less
    modifyMaxSuccess (const 1000) $
      prop "are taken, whatever their size" $
        forAll (elements entropySizes >>= uniformBytes) $ \entropy ->
          checkEntropy (fromIntegral (BS.length entropy)) entropy `shouldBe` Right entropy

-- | The refusal of bytes with a pattern
patterned :: String
patterned = "The operating system gave bytes with a pattern that random bytes about never have: its entropy is broken"

-- | Random bytes, read from /dev/random once, on 2026-10-26
randomBytes32 :: BS.ByteString
randomBytes32 =
  BS.pack
    [ 0x8a, 0x19, 0x1f, 0xe9, 0x25, 0x26, 0x35, 0x0a, 0x2b, 0x5d, 0xe2, 0xd4, 0x3d, 0x8d, 0xdd, 0x9f,
      0x6e, 0xba, 0xcf, 0x30, 0xd8, 0x26, 0x75, 0x9f, 0xc6, 0xb5, 0x7b, 0xf5, 0xf8, 0x34, 0x3b, 0x08
    ]

-- | Bytes each chosen from the 256 with the same chance. The arbitrary
-- bytes of QuickCheck favour small values, and random bytes do not.
uniformBytes :: Int -> Gen BS.ByteString
uniformBytes size = BS.pack <$> vectorOf size (chooseEnum (0, 255))

{- A new mnemonic from its entropies -}

-- | The message of a new mnemonic that fails its check
failedCheck :: String
failedCheck = "The new mnemonic failed its check, so hw does not show it: please report this bug"

newMnemonicSpec :: Spec
newMnemonicSpec =
  describe "A new mnemonic from the entropies hw gathered" $ do
    it "is the mnemonic of the entropy of the system without dice rolls" $
      newMnemonic entropy7f Nothing [] `shouldBe` Right (legalYellow, [])

    it "mixes the entropy of the dice rolls with the system's by exclusive or" $
      newMnemonic entropy7f (Just entropy80) [] `shouldBe` Right (zooWrong, [])

    it "comes in parts: the one that makes it with the random parts, then the random parts" $ do
      newMnemonic entropyFf Nothing [entropy80] `shouldBe` Right (zooWrong, [legalYellow, letterAbove])
      newMnemonic entropy00 Nothing [entropy7f, entropy80] `shouldBe` Right (abandonAbout, [zooWrong, legalYellow, letterAbove])

    it "splits the entropy mixed with the dice rolls" $
      newMnemonic entropy7f (Just entropy80) [entropy7f] `shouldBe` Right (zooWrong, [letterAbove, legalYellow])

    it "is refused when the dice rolls do not give as many bytes as the system" $
      newMnemonic entropy7f (Just (BS.replicate 15 0x80)) []
        `shouldBe` Left "The dice rolls did not give as many bytes as the entropy of the system"

    -- Two equal random parts would give two equal parts, which no merge
    -- takes: the funds of the mnemonic would be lost
    it "is refused when its parts would not merge back into it" $
      newMnemonic entropy7f Nothing [entropy80, entropy80] `shouldBe` Left failedCheck

    -- The message is haskoin-core's: hw makes mnemonics of the sizes BIP39
    -- allows only
    it "is refused for an entropy that no mnemonic has" $
      newMnemonic (BS.replicate 15 0x7f) Nothing [] `shouldSatisfy` isLeft

{- Generating a mnemonic -}

-- | A new mnemonic of the entropy of the system, without dice rolls; with
-- its parts when it is split
generated :: Int -> Int -> IO (Text, [Text])
generated bytes parts = do
  (_, mnem, splitMnems) <- expectRight =<< runExceptT (genMnemonic (fromIntegral bytes) Nothing (fromIntegral parts))
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
    it "names the source of its entropy, /dev/random where the machine has it" $
      withDevice "/dev/random" $ do
        (source, _, _) <- expectRight =<< runExceptT (genMnemonic 16 Nothing 1)
        source `shouldBe` "/dev/random"

    it "comes without parts when it is not split" $ do
      (_, parts) <- generated 16 1
      parts `shouldBe` []

    it "comes with the number of parts asked for, which merge into it" $ do
      (mnem, parts) <- generated 16 3
      length parts `shouldBe` 3
      mergeMnemonicParts parts `shouldBe` Right mnem

    it "comes in parts of as many words as it has" $ do
      (_, parts) <- generated 32 2
      map (length . Text.words) parts `shouldBe` [24, 24]

    it "asks for the dice rolls of its number of bytes" $ do
      asked <- newIORef Nothing
      let askDice bytes = liftIO (writeIORef asked (Just bytes)) >> return (BS.replicate (fromIntegral bytes) 0x80)
      _ <- expectRight =<< runExceptT (genMnemonic 24 (Just askDice) 1)
      readIORef asked `shouldReturn` Just 24

    -- Rolls that replaced the entropy of the system would give the
    -- mnemonic of their own entropy, the same every time
    it "mixes the dice rolls with the entropy of the system: the same rolls give a new mnemonic" $ do
      let askDice _ = return entropy80
      (_, first, _) <- expectRight =<< runExceptT (genMnemonic 16 (Just askDice) 1)
      (_, second, _) <- expectRight =<< runExceptT (genMnemonic 16 (Just askDice) 1)
      first `shouldNotBe` second
      first `shouldNotBe` letterAbove

    it "is not made when the dice rolls are cancelled" $
      runExceptT (genMnemonic 16 (Just (\_ -> throwError "Cancelled: the input ended before the answer")) 1)
        `shouldReturn` Left "Cancelled: the input ended before the answer"

    -- The system gives its bytes before it sees a roll, so it cannot choose
    -- them to cancel the rolls; and a source that fails fails before the
    -- user rolls a die
    it "reads the entropy of the system before it asks for the dice rolls" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "random"
        BS.writeFile file (BS.pack [0x00 .. 0x3f])
        asked <- newIORef False
        let askDice bytes = liftIO (writeIORef asked True) >> return (BS.replicate (fromIntegral bytes) 0x80)
        runExceptT (genMnemonicAt file 16 (Just askDice) 1) `shouldReturn` notDevice file
        readIORef asked `shouldReturn` False

    it "is refused for another size of entropy, before any dice roll" $ do
      asked <- newIORef False
      let askDice bytes = liftIO (writeIORef asked True) >> return (BS.replicate (fromIntegral bytes) 0x80)
      forM_ [12, 17, 36] $ \bytes ->
        runExceptT (genMnemonic bytes (Just askDice) 1)
          `shouldReturn` Left ("A mnemonic has 16, 20, 24, 28 or 32 bytes of entropy, not " <> show bytes)
      readIORef asked `shouldReturn` False

    it "is refused in no part" $
      runExceptT (genMnemonic 16 Nothing 0) `shouldReturn` Left "A mnemonic comes in one part or more, not 0"
