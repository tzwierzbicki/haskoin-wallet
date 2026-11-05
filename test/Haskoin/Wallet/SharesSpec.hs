{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A backup in shares, as "Haskoin.Wallet.Shares" makes and reads it: the
-- number of shares and the threshold that --shares and --threshold give,
-- the shares of the entropy hw gathers, with the dice rolls mixed in, and
-- the shares a user types, followed until they rebuild a backup.
--
-- The shares expected of given entropies were made apart from hw, by the
-- SLIP-39 verifier of haskoin-core (slip39_verify.py), an implementation
-- of the specification of its own: one group, the extendable flag set, an
-- iteration exponent of 1 and an empty passphrase. The shares typed are
-- those of the device tests of a hardware wallet: see 'testShares'.
module Haskoin.Wallet.SharesSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import Data.List (nub, subsequences)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (btc, encodeHex, makeXPrvKey, xPrvExport)
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret (..), Share (..), combineMnemonics, decodeShare, decryptMasterSecret)
import Haskoin.Wallet.Shares
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  schemeSpec
  sizesSpec
  newSharesSpec
  generateSpec
  readingSpec
  typedSpec

{- Fixtures -}

-- | The entropy 00 to 0f, and the random part 20 to 2b of a split of it at
-- a threshold of 2
seed16, part12 :: BS.ByteString
seed16 = BS.pack [0x00 .. 0x0f]
part12 = BS.pack [0x20 .. 0x2b]

-- | The three shares of 'seed16' at a threshold of 2 with the identifier
-- 42 and the random part 'part12'
twoOfThree :: [Text]
twoOfThree = [firstOfThree, secondOfThree, thirdOfThree]

firstOfThree, secondOfThree, thirdOfThree :: Text
firstOfThree = "acid fawn academic acid axis auction endless award answer random spew dryer again public civil unwrap scandal pleasure extra member"
secondOfThree = "acid fawn academic agency damage stilt math warmth satisfy step gravity prospect elder swing flexible explain subject sugar holy sled"
thirdOfThree = "acid fawn academic always crisis research spirit eyebrow often hunting reaction diploma envelope have response cargo warn violence smirk symbolic"

-- | The first share of the backup of the device tests
firstTestShare :: Text
firstTestShare = "extra extend academic bishop cricket bundle tofu goat apart victim enlarge program behavior permit course armed jerky faint language modern"

-- | Every choice of the given number of elements
choices :: Int -> [a] -> [[a]]
choices n xs = [set | set <- subsequences xs, length set == n]

{- The number of shares and the threshold -}

schemeSpec :: Spec
schemeSpec =
  describe "The shares and the threshold of a new backup" $ do
    it "are one share that is the whole backup, without a threshold given" $
      shareScheme 1 Nothing `shouldBe` Right (1, 1)

    it "take a threshold of 1 for one share" $
      shareScheme 1 (Just 1) `shouldBe` Right (1, 1)

    it "are the threshold and the number of shares given, threshold first" $ do
      shareScheme 3 (Just 2) `shouldBe` Right (2, 3)
      shareScheme 5 (Just 5) `shouldBe` Right (5, 5)
      shareScheme 16 (Just 16) `shouldBe` Right (16, 16)

    it "need a threshold for more than one share" $
      shareScheme 3 Nothing `shouldBe` Left "With more than one share, give --threshold: the number of shares that rebuild the seed"

    -- The shares would differ by their number alone
    it "refuse a threshold of 1 for more than one share" $
      shareScheme 3 (Just 1) `shouldBe` Left "With a threshold of 1 every share would be the whole backup: make one share, and copy it"

    it "refuse a threshold above the number of shares" $ do
      shareScheme 3 (Just 4) `shouldBe` Left "The threshold cannot be greater than the number of shares, 3"
      shareScheme 1 (Just 2) `shouldBe` Left "The threshold cannot be greater than the number of shares, 1"

    it "refuse a threshold of 0" $
      shareScheme 3 (Just 0) `shouldBe` Left "The threshold is at least 1"

    it "refuse no share, and more than 16" $ do
      shareScheme 0 Nothing `shouldBe` Left "A backup comes in 1 to 16 shares"
      shareScheme 17 (Just 2) `shouldBe` Left "A backup comes in 1 to 16 shares"

{- The random values of a split -}

sizesSpec :: Spec
sizesSpec =
  describe "The random values that split a seed" $ do
    it "are none at a threshold of 1, where the share is the backup itself" $
      splitValueSizes 1 16 `shouldBe` []

    -- 16 - 4 bytes
    it "are the random part of the digest alone at a threshold of 2, four bytes shorter than the seed" $ do
      splitValueSizes 2 16 `shouldBe` [12]
      splitValueSizes 2 32 `shouldBe` [28]

    it "are one more, as long as the seed, for each share the threshold needs beyond two, before that part" $ do
      splitValueSizes 3 16 `shouldBe` [16, 12]
      splitValueSizes 5 32 `shouldBe` [32, 32, 32, 28]

{- New shares from the values hw gathered -}

-- | The refusal of dice that do not cover the entropy, byte for byte
diceShort :: Either String a
diceShort = Left "The dice rolls did not give as many bytes as the entropy of the system"

newSharesSpec :: Spec
newSharesSpec =
  describe "The shares of a new backup from the entropies hw gathered" $ do
    it "are one share of 20 words for 16 bytes of entropy at a threshold of 1" $
      newShares 1 1 1234 seed16 [] Nothing
        `shouldBe` Right ["analysis morning academic academic center intend saver petition romp expand dryer smirk award estate sunlight deny mortgage debut dictate rocky"]

    it "are the shares of the entropy of the system and of its random part, without dice rolls" $
      newShares 2 3 42 seed16 [part12] Nothing `shouldBe` Right twoOfThree

    -- A system that gave zeros leaves the dice as they are: the first 16
    -- bytes are the seed, and the next 12 the random part
    it "mix a piece of the dice into the entropy, and the next piece into the random part" $
      newShares 2 3 42 (BS.replicate 16 0x00) [BS.replicate 12 0x00] (Just (seed16 <> part12)) `shouldBe` Right twoOfThree

    -- 0x0f xor 0x0f, 0x0e, 0x0d... is 00, 01, 02..., and 0xf0 xor 0xd0,
    -- 0xd1... is 20, 21...
    it "mix the dice with the system's values by exclusive or" $
      newShares 2 3 42 (BS.replicate 16 0x0f) [BS.replicate 12 0xf0] (Just (BS.pack [0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00] <> BS.pack [0xd0 .. 0xdb]))
        `shouldBe` Right twoOfThree

    -- A random share 40 to 4f, which is the first share as it is, then the
    -- random part: with zeros from the system, bytes 16 to 31 of the dice
    -- and bytes 32 to 43
    it "mix a piece of the dice into each random share as well, at a threshold of 3" $
      newShares 3 4 42 (BS.replicate 16 0x00) [BS.replicate 16 0x00, BS.replicate 12 0x00] (Just (seed16 <> BS.pack [0x40 .. 0x4f] <> part12))
        `shouldBe` Right
          [ "acid fawn academic acne away editor amount desert beaver endless camera lips famous evaluate decision spill moisture goat spelling lungs",
            "acid fawn academic agree dance mule salon says stilt critical premium amuse black cluster estate infant material best unhappy shrimp",
            "acid fawn academic amazing capital short viral frozen upstairs making idea academic literary inherit privacy payroll careful industry romp usual",
            "acid fawn academic arcade center campus duke plastic disaster tracks roster math satisfy briefing spit advance activity aide worthy quiet"
          ]

    -- Dice for the entropy alone would have to go into the random part a
    -- second time, as a one-time pad never does
    it "use no byte of the dice twice: dice that cover the entropy alone are refused with a random part" $
      newShares 2 3 42 seed16 [part12] (Just (BS.replicate 16 0x80)) `shouldBe` diceShort

    it "are refused when the dice give more bytes than the entropy and its random values" $
      newShares 2 3 42 seed16 [part12] (Just (BS.replicate 29 0x80)) `shouldBe` diceShort

    it "have 33 words each for 32 bytes of entropy" $
      newShares 2 2 0 (BS.pack [0x00 .. 0x1f]) [BS.pack [0x60 .. 0x7b]] Nothing
        `shouldBe` Right
          [ "academic agency academic acid airport idle resident emphasis nail dwarf crunch cards adjust bundle impact election activity modify tension move fitness terminal emerald stick burning quantity payment body strike guitar emphasis enjoy flexible",
            "academic agency academic agency amount weapon level famous column slavery closet furl emission industry pupal acquire parcel exceed slap smell observe airline froth divorce endless exercise physics pumps sheriff tendency grin kernel behavior"
          ]

    it "rebuild the entropy as the seed without a passphrase, from any threshold of them" $
      forM_ (choices 2 twoOfThree) $ \two ->
        combineMnemonics "" two `shouldBe` Right seed16

    it "are refused for a size of entropy that no backup in shares has, with its size" $ do
      newShares 1 1 42 (BS.replicate 20 0x7f) [] Nothing `shouldBe` Left "A backup in shares has 16 or 32 bytes of entropy, not 20"
      newShares 1 1 42 (BS.replicate 15 0x7f) [] Nothing `shouldBe` Left "A backup in shares has 16 or 32 bytes of entropy, not 15"

    it "are refused when the random values do not have the sizes the split draws" $ do
      newShares 2 3 42 seed16 [] Nothing `shouldBe` Left "The random values of the split do not have the sizes it draws"
      newShares 2 3 42 seed16 [BS.replicate 16 0x00] Nothing `shouldBe` Left "The random values of the split do not have the sizes it draws"
      newShares 1 1 42 seed16 [part12] Nothing `shouldBe` Left "The random values of the split do not have the sizes it draws"

    -- Without dice the seed is the entropy of the system as it came
    prop "rebuild the entropy of the system from any threshold of them, without dice" $
      forAll ((,,) <$> bytesOf 16 <*> bytesOf 16 <*> bytesOf 12) $ \(system, randomShare, randomPart) -> do
        shares <- expectRight $ newShares 3 4 7 system [randomShare, randomPart] Nothing
        forM_ (choices 3 shares) $ \three ->
          combineMnemonics "" three `shouldBe` Right system

    prop "rebuild one same seed from any threshold of them, with dice" $
      forAll ((,,,) <$> bytesOf 16 <*> bytesOf 16 <*> bytesOf 12 <*> bytesOf 44) $ \(system, randomShare, randomPart, dice) -> do
        shares <- expectRight $ newShares 3 4 7 system [randomShare, randomPart] (Just dice)
        seeds <- mapM (expectRight . combineMnemonics "") (choices 3 shares)
        length (nub seeds) `shouldBe` 1
  where
    bytesOf n = BS.pack <$> vectorOf n (chooseEnum (0, 255))

{- Generating shares -}

-- | New shares of fixed entropies and dice, with the log of what the
-- sources said and did
fromFixed :: [BS.ByteString] -> Maybe BS.ByteString -> Natural -> Natural -> Maybe Natural -> IO (Either String (Text, Int, [Text]), [String])
fromFixed entropies diceM bytes count thresholdM = do
  logged <- newIORef []
  sources <- fixedSources logged entropies diceM
  result <- runExceptT (genShares sources bytes count thresholdM)
  (,) result <$> readIORef logged

generateSpec :: Spec
generateSpec =
  describe "New shares" $ do
    -- After the sample, the system gives the entropy, the two bytes of the
    -- identifier and the random part. The identifier has 15 bits: 0x802a
    -- without its highest bit is 42.
    it "are those of the entropy, the identifier and the random part read after the sample, in that order" $
      fromFixed [goodSample, seed16, BS.pack [0x80, 0x2a], part12] Nothing 16 3 (Just 2)
        `shouldReturn` (Right ("fixed", 2, twoOfThree), [readingSystemMessage, "system 64", "system 16", "system 2", "system 12"])

    -- 16 bytes for the entropy and 12 for the random part: 28
    it "ask for the dice once, for the entropy and every random value together, after the system is read" $
      fromFixed [goodSample, BS.replicate 16 0x00, BS.pack [0x00, 0x2a], BS.replicate 12 0x00] (Just (seed16 <> part12)) 16 3 (Just 2)
        `shouldReturn` (Right ("fixed", 2, twoOfThree), [readingSystemMessage, "system 64", "system 16", "system 2", "system 12", "dice 28"])

    it "ask for the dice of the entropy alone for one share" $ do
      (_, logged) <- fromFixed [goodSample, seed16, BS.pack [0x04, 0xd2]] (Just (BS.replicate 16 0x00)) 16 1 Nothing
      logged `shouldBe` [readingSystemMessage, "system 64", "system 16", "system 2", "dice 16"]

    -- 0x04d2 is 1234
    it "are one share for --shares 1, the whole backup" $
      (fst <$> fromFixed [goodSample, seed16, BS.pack [0x04, 0xd2]] Nothing 16 1 Nothing)
        `shouldReturn` Right ("fixed", 1, ["analysis morning academic academic center intend saver petition romp expand dryer smirk award estate sunlight deny mortgage debut dictate rocky"])

    it "come as many as asked, any threshold of which rebuilds the same seed" $ do
      (_, threshold, shares) <- expectRight =<< runExceptT (genShares systemSources 16 5 (Just 3))
      (threshold, length shares) `shouldBe` (3, 5)
      -- 5 choose 3
      seeds <- mapM (expectRight . combineMnemonics "") (choices 3 shares)
      (length seeds, length (nub seeds)) `shouldBe` (10, 1)

    -- Two backups of 16 random bytes: one chance in 2^128
    it "are new every time" $ do
      (_, _, first) <- expectRight =<< runExceptT (genShares systemSources 16 1 Nothing)
      (_, _, second) <- expectRight =<< runExceptT (genShares systemSources 16 1 Nothing)
      first `shouldNotBe` second

    it "are not made when the sample of the source has a pattern" $
      fromFixed [BS.replicate 64 0x00, seed16] Nothing 16 3 (Just 2)
        `shouldReturn` (Left "The operating system gave bytes with a pattern that random bytes about never have: its entropy is broken", [readingSystemMessage, "system 64"])

    it "are refused for 20, 24 or 28 bytes of entropy, before any read" $
      forM_ [20, 24, 28] $ \bytes ->
        fromFixed [goodSample, seed16] Nothing bytes 3 (Just 2)
          `shouldReturn` (Left ("A backup in shares has 16 or 32 bytes of entropy, not " <> show bytes), [])

    it "are refused without a threshold for several shares, before any read" $
      fromFixed [goodSample, seed16] Nothing 16 3 Nothing
        `shouldReturn` (Left "With more than one share, give --threshold: the number of shares that rebuild the seed", [])

{- A share as typed -}

readingSpec :: Spec
readingSpec =
  describe "A share as typed" $ do
    it "is read with what it says of its backup: the identifier, the threshold and its number" $ do
      share <- expectRight $ readShare firstOfThree
      (share.identifier, share.extendable, share.iterationExponent) `shouldBe` (42, True, 1)
      (share.groupIndex, share.groupThreshold, share.groupCount) `shouldBe` (0, 1, 1)
      (share.memberIndex, share.memberThreshold) `shouldBe` (0, 2)
      encodeHex share.value `shouldBe` "410e5190fc2db5b513dc10b0c8eeff10"

    it "is read in any case, with any spaces between its words" $
      readShare ("  " <> Text.toUpper (Text.replace " " "   " firstOfThree) <> " ") `shouldBe` decodeShare firstOfThree

    it "is refused with a wrong number of words, without repeating them" $ do
      readShare (Text.unwords (take 19 (Text.words firstOfThree))) `shouldBe` Left "A share of a backup has 20 or 33 words, not 19"
      readShare (Text.unwords (Text.words firstOfThree <> ["academic"])) `shouldBe` Left "A share of a backup has 20 or 33 words, not 21"

    it "is refused with a word that is not in the list, named by its position only" $
      readShare (Text.replace "axis" "axes" firstOfThree) `shouldBe` Left "Word 5 is not a word of the share list"

    -- "always" for "acid", the fourth word
    it "is refused when its words do not check out" $
      readShare "acid fawn academic always axis auction endless award answer random spew dryer again public civil unwrap scandal pleasure extra member"
        `shouldBe` Left "The words of the share do not check out: one of them is mistyped"

{- The shares of a backup as they are typed -}

typedSpec :: Spec
typedSpec =
  describe "The shares typed of a backup" $ do
    let typed = foldl (\set mnemonic -> set >>= \s -> readShare mnemonic >>= addShare s) (Right noShares)
        progressOf = fmap progress . typed
        needs = Right . Needs
    it "need the threshold of them: three for the backup of the device tests" $ do
      progressOf (take 1 testShares) `shouldBe` needs "Share 1 of the 3 needed"
      progressOf (take 2 testShares) `shouldBe` needs "Share 2 of the 3 needed"

    it "are enough at the threshold, and rebuild the backup, which the passphrase decrypts" $ do
      Enough shares <- expectRight $ progressOf testShares
      rebuilt shares `shouldBe` Right testSharesBackup
      -- the seed of the master key xprv9s21ZrQH143K2pMW..., as the tests of the device give it
      fmap encodeHex (rebuilt shares >>= decryptMasterSecret testSharesPassphrase) `shouldBe` Right "578d685d20b602683dc5171df411d3e2"

    it "are enough in any order" $ do
      Enough shares <- expectRight $ progressOf (reverse testShares)
      rebuilt shares `shouldBe` Right testSharesBackup

    it "is one share, enough by itself, for a backup of one share" $ do
      Enough shares <- expectRight $ progressOf ["analysis morning academic academic center intend saver petition romp expand dryer smirk award estate sunlight deny mortgage debut dictate rocky"]
      -- the entropy 00 to 0f encrypted with the empty passphrase
      fmap encodeHex (rebuilt shares >>= decryptMasterSecret "") `shouldBe` Right "000102030405060708090a0b0c0d0e0f"

    it "refuse a share typed twice" $
      fmap (const ()) (typed [firstTestShare, firstTestShare]) `shouldBe` Left "This share was typed already"

    it "refuse a share of another backup" $
      fmap (const ()) (typed [firstTestShare, firstOfThree]) `shouldBe` Left "This share is of another backup than the ones before"

    -- The first share is a group of one; the others are of a group that
    -- needs three of its five
    it "need the group threshold of groups, each with the threshold of its shares" $ do
      progressOf (take 1 testGroupShares) `shouldBe` needs "Groups complete: 1 of the 2 needed"
      progressOf (take 2 testGroupShares) `shouldBe` needs "Groups complete: 1 of the 2 needed; group 3 has 1 of its 3 shares"
      progressOf (take 3 testGroupShares) `shouldBe` needs "Groups complete: 1 of the 2 needed; group 3 has 2 of its 3 shares"
      progressOf (drop 1 testGroupShares) `shouldBe` needs "Groups complete: 1 of the 2 needed"

    it "rebuild a backup of several groups once enough groups are complete" $ do
      Enough shares <- expectRight $ progressOf testGroupShares
      length shares `shouldBe` 4
      backup <- expectRight $ rebuilt shares
      (backup.identifier, backup.extendable, backup.iterationExponent) `shouldBe` (9497, False, 0)
      -- The master key the tests of the device give for the passphrase
      fmap (xPrvExport btc . makeXPrvKey) (decryptMasterSecret "TREZOR" backup) `shouldBe` Right testGroupSharesMasterKey

    -- Two shares of one 2-of-3 backup and one made the same way from
    -- another seed, with the same identifier: the digest tells
    it "do not rebuild a backup when one of them was made from another seed, however well formed" $ do
      others <- expectRight $ newShares 2 3 42 (BS.pack [0x10 .. 0x1f]) [part12] Nothing
      Enough shares <- expectRight $ progressOf (firstOfThree : take 1 (drop 1 others))
      rebuilt shares
        `shouldBe` Left "These shares do not rebuild a backup: one of them is of another backup, or was altered (The digest of the secret does not match)"
