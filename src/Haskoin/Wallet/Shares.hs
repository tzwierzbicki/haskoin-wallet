{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A backup in shares, as SLIP-39 describes it: a seed split so that a
-- chosen number of its shares rebuild it, and fewer tell nothing of it.
-- Each share is a list of words with a checksum. hw makes the shares of a
-- new seed here, from the entropy "Haskoin.Wallet.Entropy" reads, and
-- follows the shares a user types until they rebuild a backup. The format,
-- the sharing and its checks are those of "Haskoin.Crypto.Keys.Slip39".
--
-- Such a backup is not a BIP39 mnemonic. Its seed is the one SLIP-39
-- defines, from which the wallets that read the format derive the same
-- accounts. hw makes plain T-of-N backups, one group in the terms of the
-- format, and reads any, those of several groups included.
module Haskoin.Wallet.Shares
  ( -- * New shares
    shareScheme,
    shareEntropySizes,
    splitValueSizes,
    newShares,
    genShares,

    -- * Shares as typed
    readShare,
    ShareSet,
    noShares,
    addShare,
    Progress (..),
    progress,
    rebuilt,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import Data.List (elemIndex, intercalate, subsequences)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin (Mnemonic)
import Haskoin.Crypto.Keys.Slip39
import Haskoin.Wallet.Entropy (Sources (..), checkSource, mixDice, readingSystem)
import Numeric.Natural (Natural)

{- New shares -}

-- | The sizes of entropy, in bytes, that a backup in shares can have: the
-- two that the format asks every wallet to read
shareEntropySizes :: [Natural]
shareEntropySizes = [16, 32]

-- | The iteration exponent of new shares, which sets the rounds of PBKDF2
-- a passphrase goes through to 20000: the choice of the reference
-- implementation
newIterationExponent :: Int
newIterationExponent = 1

-- | The threshold and the number of shares of a new backup, from what
-- --shares and --threshold give. One share needs no threshold: it is 1.
-- More shares need one from 2 up. At a threshold of 1 every share would be
-- the whole backup, and the format refuses several of them.
shareScheme :: Natural -> Maybe Natural -> Either String (Int, Int)
shareScheme count thresholdM = do
  when (count < 1 || count > 16) $ Left "A backup comes in 1 to 16 shares"
  threshold <- case (count, thresholdM) of
    (1, Nothing) -> Right 1
    (_, Nothing) -> Left "With more than one share, give --threshold: the number of shares that rebuild the seed"
    (_, Just t) -> Right t
  when (threshold < 1) $ Left "The threshold is at least 1"
  when (threshold > count) $
    Left ("The threshold cannot be greater than the number of shares, " <> show count)
  when (threshold == 1 && count > 1) $
    Left "With a threshold of 1 every share would be the whole backup: make one share, and copy it"
  return (fromIntegral threshold, fromIntegral count)

-- | The sizes, in bytes, of the random values that the split of a seed
-- draws at a threshold, in the order they are drawn: one as long as the
-- seed for each share the threshold needs beyond two, then one four bytes
-- shorter, the random part of the digest that checks a set of shares. None
-- at a threshold of 1, where the share is the backup itself.
splitValueSizes :: Int -> Int -> [Int]
splitValueSizes threshold seedBytes = replicate count size <> [digestSize | threshold > 1]
  where
    (count, size, digestSize) = splitRandomSizes threshold seedBytes

-- | The shares of a new backup at a threshold, from the values hw
-- gathered: the identifier of the backup, the entropy of the system, the
-- random values of the split as the system gave them ('splitValueSizes'),
-- and the dice rolls if any.
--
-- The dice are mixed into the entropy and into each random value of the
-- split, a piece into each ('mixDice'): no byte of them is used twice, or
-- for anything but the exclusive or with a byte of the system's entropy.
-- The identifier is no secret, and takes none. The mixed entropy is the
-- seed of the backup without a passphrase. The shares carry it encrypted
-- with the empty passphrase, so that any passphrase typed later gives a
-- seed of its own, as the format intends.
--
-- The shares are checked before the user writes them down: every set of
-- them that meets the threshold, read back from its words, must rebuild
-- the same backup, and the backup must give the seed back. A failure
-- would lose the funds sent to the accounts of the seed.
newShares :: Int -> Int -> Int -> BS.ByteString -> [BS.ByteString] -> Maybe BS.ByteString -> Either String [Mnemonic]
newShares threshold count identifier system randoms diceM = do
  unless (fromIntegral (BS.length system) `elem` shareEntropySizes) $
    Left ("A backup in shares has 16 or 32 bytes of entropy, not " <> show (BS.length system))
  unless (map BS.length randoms == splitValueSizes threshold (BS.length system)) $
    Left "The random values of the split do not have the sizes it draws"
  mixed <- mixDice diceM (system : randoms)
  (seed, splitRandom) <- case mixed of
    seed : values -> Right (seed, splitRandomOf values)
    [] -> Left failedCheck
  -- One group: the split among groups, at a threshold of 1, draws nothing
  groups <- generateSharesWith (Scheme 1 [(threshold, count)]) identifier True newIterationExponent "" seed [SplitRandom [] BS.empty, splitRandom]
  shares <- case groups of
    [shares] -> Right shares
    _ -> Left failedCheck
  mnemonics <- mapM encodeShare shares
  typed <- mapM decodeShare mnemonics
  backups <- mapM combineShares [set | set <- subsequences typed, length set == threshold]
  case backups of
    backup : others
      | all (== backup) others, decryptMasterSecret "" backup == Right seed -> Right mnemonics
    _ -> Left failedCheck
  where
    -- The last value is the random part of the digest, the ones before the
    -- random shares
    splitRandomOf values = case reverse values of
      digestPart : randomShares -> SplitRandom (reverse randomShares) digestPart
      [] -> SplitRandom [] BS.empty
    failedCheck = "The new shares failed their check, so hw does not show them: please report this bug"

-- | New shares of a backup of the given number of bytes of entropy, their
-- number and their threshold as --shares and --threshold give them: the
-- name of the source of the entropy, the threshold, and the shares.
--
-- As for a mnemonic, the source is checked first, on a sample that is
-- thrown away, and everything the system gives is read before the dice
-- rolls: the entropy, the identifier of the backup and the random values
-- of the split. The dice are asked once, for the entropy and every random
-- value of the split together.
genShares :: (Monad m) => Sources m -> Natural -> Natural -> Maybe Natural -> ExceptT String m (Text, Int, [Mnemonic])
genShares sources bytes count thresholdM = do
  (threshold, shares) <- liftEither $ shareScheme count thresholdM
  unless (bytes `elem` shareEntropySizes) $
    throwError ("A backup in shares has 16 or 32 bytes of entropy, not " <> show bytes)
  lift $ say sources readingSystem
  checkSource (readSystem sources)
  (source, system) <- readSystem sources bytes
  (_, identifierBytes) <- readSystem sources 2
  let sizes = splitValueSizes threshold (fromIntegral bytes)
  randoms <- mapM (fmap snd . readSystem sources . fromIntegral) sizes
  diceM <- traverse ($ (bytes + fromIntegral (sum sizes))) (askDice sources)
  mnemonics <- liftEither $ newShares threshold shares (identifierOf identifierBytes) system randoms diceM
  return (source, threshold, mnemonics)
  where
    -- The identifier has 15 bits: the two bytes as a number, without its
    -- highest bit
    identifierOf = (.&. 0x7fff) . BS.foldl' (\n b -> n * 256 + fromIntegral b) 0

{- Shares as typed -}

-- | A share as typed; or what is wrong with it, without repeating any of
-- its words, which are secret. The words are matched in lower case.
readShare :: Text -> Either String Share
readShare typed
  | count < 20 || paddingBits > 8 = Left ("A share of a backup has 20 or 33 words, not " <> show count)
  | Just position <- elemIndex Nothing indices = Left ("Word " <> show (position + 1) <> " is not a word of the share list")
  | not checked = Left "The words of the share do not check out: one of them is mistyped"
  | otherwise = first ("This is not a share hw can read: " <>) (decodeShare (Text.unwords ws))
  where
    ws = Text.words (Text.toLower typed)
    count = length ws
    -- Past the four words of the header and before the three of the
    -- checksum, the words hold the value: a multiple of 16 bits, behind
    -- fewer than 10 bits of padding
    paddingBits = (10 * (count - 7)) `mod` 16
    indices = map (`Map.lookup` slip39WordListMap) ws
    -- The checksum is salted by the extendable flag, bit 4 of the second
    -- word
    checked = case sequence indices of
      Just found@(_ : second : _) -> rs1024Verify (customizationString (odd (second `div` 16))) found
      _ -> False

-- | The shares of one backup typed so far, in the order they were typed
newtype ShareSet = ShareSet [Share]

-- | No share yet
noShares :: ShareSet
noShares = ShareSet []

-- | The set with one more share; or why the share does not go with the
-- others: it is of another backup, it was typed already, or its group
-- gives it another threshold
addShare :: ShareSet -> Share -> Either String ShareSet
addShare (ShareSet shares) share
  | not (all sameBackup shares) = Left "This share is of another backup than the ones before"
  | any sameMember shares = Left "This share was typed already"
  | any otherThreshold shares = Left "This share does not go with the others of its group"
  | otherwise = Right (ShareSet (shares <> [share]))
  where
    sameBackup, sameMember, otherThreshold :: Share -> Bool
    sameBackup s = common s == common share
    sameMember s = s.groupIndex == share.groupIndex && s.memberIndex == share.memberIndex
    otherThreshold s = s.groupIndex == share.groupIndex && s.memberThreshold /= share.memberThreshold
    -- What every share of a backup carries the same
    common :: Share -> (Int, Bool, Int, Int, Int, Int)
    common s = (s.identifier, s.extendable, s.iterationExponent, s.groupThreshold, s.groupCount, BS.length s.value)

-- | How far a set of shares is from rebuilding its backup
data Progress
  = -- | Enough shares are typed: the ones that rebuild the backup
    Enough [Share]
  | -- | What is typed of what is needed, as hw tells it before the next
    -- share
    Needs String
  deriving (Eq, Show)

-- | Whether the shares typed are enough. A backup of one group needs the
-- threshold of its shares. One of several groups needs as many groups as
-- its group threshold, each with the threshold of its own shares: the
-- first groups to be complete are taken, and the shares beyond are left
-- out.
progress :: ShareSet -> Progress
progress (ShareSet shares) = case shares of
  [] -> Needs "No share yet"
  one : _
    | length complete >= one.groupThreshold -> Enough (concat (take one.groupThreshold complete))
    | one.groupCount == 1 -> Needs ("Share " <> show (length shares) <> " of the " <> show one.memberThreshold <> " needed")
    | otherwise -> Needs (intercalate "; " (("Groups complete: " <> show (length complete) <> " of the " <> show one.groupThreshold <> " needed") : started))
  where
    groups = Map.toList (Map.fromListWith (flip (<>)) [(s.groupIndex, [s]) | s <- shares])
    complete = [take m.memberThreshold members | (_, members@(m : _)) <- groups, length members >= m.memberThreshold]
    started =
      [ "group " <> show (index + 1) <> " has " <> show (length members) <> " of its " <> show m.memberThreshold <> " shares"
        | (index, members@(m : _)) <- groups,
          length members < m.memberThreshold
      ]

-- | The backup that enough shares rebuild; or that they do not. The digest
-- the shares carry catches a share of another backup, or an altered one,
-- however well formed.
rebuilt :: [Share] -> Either String EncryptedMasterSecret
rebuilt shares = first (\why -> "These shares do not rebuild a backup: one of them is of another backup, or was altered (" <> why <> ")") (combineShares shares)
