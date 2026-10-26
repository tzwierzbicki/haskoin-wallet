{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The entropy of a new mnemonic: the entropy of the operating system,
-- mixed with the entropy of dice rolls when the user rolls dice, and the
-- parts a mnemonic can be split into, all of which are needed to merge it
-- back.
module Haskoin.Wallet.Entropy
  ( diceBits,
    diceEntropy,
    xorBytes,
    xorAll,
    splitEntropyWith,
    mergeMnemonicParts,
    systemEntropy,
    entropyAt,
    checkSource,
    hasPattern,
    newMnemonic,
    Sources (..),
    genMnemonic,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (replicateM, unless, when, zipWithM)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Data.Bifunctor (first)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.List (group, nub, sort)
import Data.List.Split (chunksOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin.Crypto (Mnemonic, fromMnemonic, toMnemonic)
import Numeric.Natural (Natural)
import System.Directory (doesPathExist)
import System.Entropy (getEntropy)
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getFileStatus, getSymbolicLinkStatus, isCharacterDevice)

{- Dice rolls -}

-- Produce uniformly distributed bits from dice rolls
-- 6 -> [0,0]
-- 1 -> [0,1]
-- 2 -> [1,0]
-- 3 -> [1,1]
-- 4 -> [0]
-- 5 -> [1]
base6ToBool :: Natural -> [Bool]
base6ToBool =
  \case
    6 -> [False, False]
    1 -> [False, True]
    2 -> [True, False]
    3 -> [True, True]
    4 -> [False]
    5 -> [True]
    _ -> error "base6ToBool: a die has the faces 1 to 6 only"

-- | The bits of dice rolls, in the order of the rolls
diceBits :: [Natural] -> [Bool]
diceBits = concatMap base6ToBool

-- | The entropy of the given number of bytes that dice rolls give, once
-- they are enough: their first bits, eight to a byte, the first bit the
-- highest. The bits beyond are not used.
diceEntropy :: Natural -> [Natural] -> Maybe BS.ByteString
diceEntropy bytes rolls
  | length bits < needed = Nothing
  | otherwise = Just $ BS.pack $ map toByte $ chunksOf 8 (take needed bits)
  where
    bits = diceBits rolls
    needed = 8 * fromIntegral bytes
    toByte = foldl' (\byte bit -> 2 * byte + if bit then 1 else 0) 0

{- The exclusive or of entropies -}

-- | The exclusive or of two entropies, byte by byte. Entropies of different
-- lengths stop the program: zipping them would drop the bytes of the longer
-- one, so the callers check the lengths first.
xorBytes :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBytes a b
  | BS.length a == BS.length b = BS.pack (BS.zipWith xor a b)
  | otherwise = error "xorBytes: the entropies do not have the same length"

-- | The exclusive or of entropies of the same length; nothing for none
xorAll :: [BS.ByteString] -> BS.ByteString
xorAll [] = BS.empty
xorAll (entropy : rest) = foldl' xorBytes entropy rest

{- The parts of a mnemonic -}

-- | A secret split with random parts of its length: the part that makes the
-- secret with them, then the random parts. All the parts are needed to
-- merge the secret back, and any fewer tell nothing of it.
splitEntropyWith :: BS.ByteString -> [BS.ByteString] -> [BS.ByteString]
splitEntropyWith secret randoms = xorAll (secret : randoms) : randoms

-- | The mnemonic that parts merge into, whatever their order: the mnemonic
-- of the exclusive or of their entropies. The parts are two or more, of as
-- many words, and all different, since two equal parts cancel out. A part
-- that does not read is named by its position only: its words are secret.
mergeMnemonicParts :: [Text] -> Either String Mnemonic
mergeMnemonicParts parts = do
  when (length parts < 2) $ Left "A mnemonic in parts has two parts or more"
  entropies <- zipWithM entropyOf [1 :: Int ..] parts
  when (length (nub (map BS.length entropies)) > 1) $
    Left "The parts do not have the same number of words"
  when (length (nub entropies) < length entropies) $
    Left "Two of the parts are the same, but every part of a mnemonic is different"
  toMnemonic (xorAll entropies)
  where
    entropyOf n part = first (const $ "Part " <> show n <> " is not a mnemonic") (fromMnemonic part)

{- The entropy of the system -}

-- | The device of the entropy of the system, on the machines that have it
randomDevice :: FilePath
randomDevice = "/dev/random"

-- | Random bytes of the operating system, with the name of their source:
-- the device /dev/random where the machine has it, read directly rather
-- than through a library, and the entropy package elsewhere
systemEntropy :: (MonadIO m) => Natural -> ExceptT String m (Text, BS.ByteString)
systemEntropy = entropyAt randomDevice

-- | Random bytes from the device at the given path, or from the entropy
-- package when nothing is there. Something else than a character device
-- at the path is refused: a plain file there, as a container or a chroot
-- may have, would give the same bytes to every mnemonic. So is a link to
-- nothing: a machine with a broken /dev/random needs a look before it
-- makes a mnemonic. A source that fails, or gives fewer bytes than asked,
-- fails the command. The bytes themselves are taken as they come: see
-- 'checkSource'.
entropyAt :: (MonadIO m) => FilePath -> Natural -> ExceptT String m (Text, BS.ByteString)
entropyAt path bytes = do
  -- The status of the path itself, a link included, where the stat that
  -- follows links sees nothing behind a broken one
  found <- liftIO $ try (getSymbolicLinkStatus path)
  (source, entropy) <- case found of
    Left e
      | isDoesNotExistError e -> (,) "System.Entropy.getEntropy" <$> systemIO (getEntropy (fromIntegral bytes))
      | otherwise -> throwError (couldNotRead e)
    Right _ -> do
      exists <- systemIO (doesPathExist path)
      unless exists $ throwError $ path <> " is a link to nothing, so hw does not read it: " <> checkMachine
      device <- isCharacterDevice <$> systemIO (getFileStatus path)
      unless device $ throwError $ path <> " is not a device, so hw does not read it: " <> checkMachine
      (,) (Text.pack path) <$> systemIO (withBinaryFile path ReadMode (`BS.hGet` fromIntegral bytes))
  unless (BS.length entropy == fromIntegral bytes) $
    throwError $ "The operating system gave " <> show (BS.length entropy) <> " bytes of entropy, not " <> show bytes
  return (source, entropy)
  where
    checkMachine = "check this machine before you make a mnemonic on it"

-- | An action of the system that reads entropy, its failure told
systemIO :: (MonadIO m) => IO a -> ExceptT String m a
systemIO io = liftIO (try io) >>= either (throwError . couldNotRead) return

-- | The error of a read of the system that failed
couldNotRead :: IOException -> String
couldNotRead e = "The entropy of the system could not be read: " <> show e

-- | The bytes read from a source to check it
sampleSize :: Natural
sampleSize = 64

-- | The check of a source of entropy, on a sample of its bytes that is
-- then thrown away: the sample must have no pattern that random bytes
-- about never have (see the last section). The entropy of a mnemonic is
-- read afterwards and taken as it comes. Refusing a mnemonic's own
-- entropy for a pattern would leave out a few of its values, and take a
-- little from the entropy of all of them; a sample thrown away takes
-- nothing.
checkSource :: (Monad m) => (Natural -> ExceptT String m (Text, BS.ByteString)) -> ExceptT String m ()
checkSource readEntropy = do
  (_, sample) <- readEntropy sampleSize
  when (hasPattern sample) $
    throwError "The operating system gave bytes with a pattern that random bytes about never have: its entropy is broken"

{- A new mnemonic -}

-- | The sizes of entropy, in bytes, that a mnemonic can have
entropySizes :: [Natural]
entropySizes = [16, 20 .. 32]

-- | The mnemonic of the entropy of the system, mixed by exclusive or with
-- the entropy of dice rolls if any, and its parts when random parts of its
-- length split it. The dice cover the entropy and every random part, each
-- byte of them used once: the first bytes with the entropy of the system,
-- then as many with each random part. Mixed so, a value is as random as
-- the more random of its two sources, and tells nothing of either; a dice
-- byte used twice would tie the two values it went into together, as a
-- one-time pad used twice does. Both the mnemonic and its parts are
-- checked before the user writes them down: the mnemonic must give back
-- its entropy, and the parts the mnemonic. A failure of either would lose
-- the funds sent to its accounts.
newMnemonic :: BS.ByteString -> Maybe BS.ByteString -> [BS.ByteString] -> Either String (Mnemonic, [Mnemonic])
newMnemonic system diceM randoms = do
  unless (fromIntegral size `elem` entropySizes) $
    Left $ "A mnemonic has 16, 20, 24, 28 or 32 bytes of entropy, not " <> show size
  unless (all ((== size) . BS.length) randoms) $
    Left "A random part of the split does not have as many bytes as the entropy of the system"
  (entropy, mixed) <- case diceM of
    Nothing -> Right (system, randoms)
    Just dice -> do
      unless (BS.length dice == size * (1 + length randoms)) $
        Left "The dice rolls did not give as many bytes as the entropy of the system and its random parts"
      let piece n = BS.take size (BS.drop (n * size) dice)
      Right (xorBytes system (piece 0), zipWith (\n random -> xorBytes random (piece n)) [1 ..] randoms)
  mnem <- toMnemonic entropy
  parts <- if null mixed then Right [] else mapM toMnemonic (splitEntropyWith entropy mixed)
  unless (fromMnemonic mnem == Right entropy) $ Left failedCheck
  unless (null parts || mergeMnemonicParts parts == Right mnem) $ Left failedCheck
  return (mnem, parts)
  where
    size = BS.length system
    failedCheck = "The new mnemonic failed its check, so hw does not show it: please report this bug"

-- | Where a new mnemonic comes from
data Sources m = Sources
  { -- | The entropy of the system: the name of its source and its bytes
    readSystem :: Natural -> ExceptT String m (Text, BS.ByteString),
    -- | The entropy of dice rolls, when the user adds them
    askDice :: Maybe (Natural -> ExceptT String m BS.ByteString),
    -- | A message to the user
    say :: String -> m ()
  }

-- | A new mnemonic of the given number of bytes of entropy, in the given
-- number of parts (one: whole), with the name of the source of its
-- entropy.
--
-- The source is checked first, on a sample that is thrown away. Then the
-- entropy of the system, and the random parts of a split, are read before
-- the dice rolls. The system gives its bytes before it sees a roll, so it
-- cannot choose them to cancel the rolls. The rolls cannot depend on
-- bytes the user never sees. So the mix is as strong as the stronger of
-- the two. A source that fails also fails before the user rolls a die.
-- The dice are asked for the entropy and every random part at once, so
-- that no roll is used twice. The user is told before the first read,
-- which waits while the system gathers its first entropy, on a machine
-- just started for instance.
genMnemonic :: (Monad m) => Sources m -> Natural -> Natural -> ExceptT String m (Text, Mnemonic, [Mnemonic])
genMnemonic sources bytes parts = do
  unless (bytes `elem` entropySizes) $
    throwError $ "A mnemonic has 16, 20, 24, 28 or 32 bytes of entropy, not " <> show bytes
  when (parts == 0) $ throwError "A mnemonic comes in one part or more, not 0"
  lift $ say sources "Reading the entropy of the system, which can take a moment while the system gathers it"
  checkSource (readSystem sources)
  (source, system) <- readSystem sources bytes
  randoms <- replicateM (fromIntegral parts - 1) (snd <$> readSystem sources bytes)
  diceM <- traverse ($ (bytes * parts)) (askDice sources)
  (mnem, split) <- liftEither $ newMnemonic system diceM randoms
  return (source, mnem, split)

{- Patterns in random bytes -}

-- Random bytes are uniform and independent of one another. So a guess of a
-- byte from the bytes before it is right with a chance of 1/256, whatever
-- the rule of the guess, and the number of right guesses among m follows
-- the binomial distribution B(m, 1/256) exactly, even for a few bytes. A
-- broken source gives bytes that simple rules guess far more often: one
-- byte over and over, a pattern that repeats, a counter. hw guesses each
-- byte of a sample by such rules, and counts the most common byte. It
-- refuses the source when a test finds a result so unlikely that random
-- bytes would give it, with all the tests together, less than once in 2^40
-- times.
--
-- This is the principle of the health tests and of the predictors of NIST
-- SP 800-90B, "Recommendation for the Entropy Sources Used for Random Bit
-- Generation" (2018): sections 4.4 and 6.3.7 to 6.3.10. The rate of false
-- alarms, 2^-40, is the lowest it recommends for its health tests. The tests
-- are combined by Bonferroni's inequality: the chance that one of N tests
-- fires is at most N times the chance of the test that fires.
--
-- This finds patterns; it does not estimate entropy. Bytes from a generator
-- with a weak seed look random to it, as to any test.

-- | Whether bytes have a pattern that random bytes have with a chance below
-- 2^-40: a rule guesses too many of them from the bytes before, or one byte
-- fills too many of them
hasPattern :: BS.ByteString -> Bool
hasPattern entropy = fromIntegral (length chances) * minimum chances < 2 ^^ (-40 :: Int)
  where
    chances = mostCommon : map rightGuesses rules
    rules = map repeatAt [1 .. 8] <> map stepAt [1 .. 4]
    -- The byte d places back, as a pattern that repeats every d bytes gives
    repeatAt d i = if i >= d then Just (at (i - d)) else Nothing
    -- The step seen d places back, taken again, as a counter in words of d
    -- bytes gives. Bytes wrap around as counters do: 0xff + 1 is 0x00.
    stepAt d i = if i >= 2 * d then Just (2 * at (i - d) - at (i - 2 * d)) else Nothing
    -- The chance that random bytes give as many right guesses by a rule
    rightGuesses rule =
      let results = [at i == guess | i <- [0 .. size - 1], Just guess <- [rule i]]
       in atLeast (length results) (length (filter id results))
    -- The chance that one of the 256 bytes fills as many places in random bytes
    mostCommon = 256 * atLeast size (maximum (0 : map length (group (sort (BS.unpack entropy)))))
    at = BS.index entropy
    size = BS.length entropy

-- | The chance that k or more of m guesses are right when each is right
-- with a chance of 1/256 apart from the others: the tail of the binomial
-- distribution B(m, 1/256), the sum of C(m, j) 255^(m-j) / 256^m for j from
-- k to m, computed exactly
atLeast :: Int -> Int -> Rational
atLeast m k = sum [fromInteger (choose j * 255 ^ (m - j)) | j <- [k .. m]] / 256 ^ m
  where
    choose j = product [toInteger (m - j + 1) .. toInteger m] `div` product [1 .. toInteger j]
