{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What hw asks the user for on the terminal: the mnemonic of a wallet,
-- or the shares of its backup, and its passphrase, which give the signing
-- key of an account; the dice rolls that add to the entropy of a new
-- mnemonic; the
-- private keys of a sweep, when they are not in a file; the answer to a
-- yes-or-no question, such as whether to send a transaction or to take a
-- high fee rate; a choice among a few, such as the address the change of
-- a payment goes back to; and an answer read as it is typed, such as a fee
-- rate.
--
-- Secrets are typed without echo, as gpg, ssh and sudo take them: nothing
-- shows on the screen, not even their length. The prompts and the messages
-- go to stderr, so that they never mix with what a command writes, its
-- JSON for instance. A question allows three attempts, each mistake told
-- before the next one, and the end of the input cancels the command.
module Haskoin.Wallet.Cli.Prompt
  ( Terminal (..),
    terminal,
    terminalOn,
    attempts,
    cancelled,
    notUtf8,
    askYes,
    askAnswer,
    askChoice,
    readMnemonic,
    SeedWords (..),
    readSeedWords,
    askSeedWords,
    askSeedSecret,
    askNewSeedSecret,
    askSigningKey,
    matchAccount,
    typedPassphraseCreated,
    typedPassphraseRecovered,
    readRolls,
    askDiceEntropy,
    readSecKeys,
    parseKeys,
  )
where

import Control.Exception (bracket_, try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (digitToInt, isSpace, ord)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8')
import Haskoin
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret, Share)
import Haskoin.Wallet.Entropy (diceBits, diceEntropy)
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Shares (Progress (..), addShare, noShares, progress, readShare, rebuilt)
import Numeric.Natural (Natural)
import qualified System.IO as IO
import System.IO.Error (isDoesNotExistError, isEOFError)

{- The terminal -}

-- | Where hw asks: a line typed without echo, or one typed as usual, or why
-- there is none, and a message for the user
data Terminal = Terminal
  { askHidden :: String -> IO (Either String Text),
    askVisible :: String -> IO (Either String Text),
    tell :: String -> IO ()
  }

-- | The terminal of the user: stdin, with the prompts and the messages on
-- stderr
terminal :: Terminal
terminal = terminalOn IO.stdin IO.stderr

-- | A terminal reading from one handle and writing to another. The echo of
-- a hidden line is off while it is typed, and back as it was afterwards,
-- whatever happens; the Enter key, not echoed either, gets its new line.
-- A line is read as bytes up to the new line, a byte that UTF-8 never uses
-- inside a character, and decoded as UTF-8, whatever the locale: a
-- passphrase with accents gives the same seed in every terminal, and bytes
-- that are not UTF-8 are refused rather than read as another passphrase.
terminalOn :: IO.Handle -> IO.Handle -> Terminal
terminalOn input output =
  Terminal
    { askHidden = askLine False,
      askVisible = askLine True,
      tell = IO.hPutStrLn output
    }
  where
    askLine shown prompt = do
      IO.hPutStr output prompt
      IO.hFlush output
      echo <- IO.hGetEcho input
      typed <- bracket_ (IO.hSetEcho input shown) (IO.hSetEcho input echo) (try (C8.hGetLine input))
      -- A terminal echoes the Enter key of a line shown; nothing else does
      onTerminal <- IO.hIsTerminalDevice input
      unless (shown && onTerminal) $ IO.hPutStrLn output ""
      case typed of
        Right line -> return $ either (const $ Left notUtf8) Right (decodeUtf8' (stripReturn line))
        Left e
          | isEOFError e -> return (Left cancelled)
          | otherwise -> ioError e
    -- A line typed on Windows, or pasted from there, ends with a return
    stripReturn line = fromMaybe line (BS.stripSuffix "\r" line)

-- | A line typed without echo; the end of the input cancels the command
hidden :: (MonadIO m) => Terminal -> String -> ExceptT String m Text
hidden term prompt = liftIO (askHidden term prompt) >>= liftEither

-- | The error when the input ends before the answer
cancelled :: String
cancelled = "Cancelled: the input ended before the answer"

-- | The error when what was typed is not UTF-8 text
notUtf8 :: String
notUtf8 = "The input is not UTF-8 text"

-- | The attempts a question allows
attempts :: Int
attempts = 3

-- | Ask until an answer is right, 'attempts' times at most: each mistake is
-- told before the next attempt, and the last one fails the command
retrying :: (MonadIO m) => Terminal -> ExceptT String m (Either String a) -> ExceptT String m a
retrying term ask = go attempts
  where
    go left = do
      answer <- ask
      case answer of
        Right a -> return a
        Left mistake
          | left <= 1 -> throwError mistake
          | otherwise -> liftIO (tell term mistake) >> go (left - 1)

{- Questions -}

-- | Whether the user answers yes to the question: y or yes, in any case.
-- Any other answer is a no, and the end of the input cancels the command.
askYes :: (MonadIO m) => Terminal -> String -> ExceptT String m Bool
askYes term question = do
  answer <- liftIO (askVisible term (question <> " [y/N] ")) >>= liftEither
  return $ Text.toLower (Text.strip answer) `elem` ["y", "yes"]

-- | What the reader makes of the answer to a prompt, without the spaces
-- around it. An answer it refuses is a mistake, told with the reason it
-- gives, and the end of the input cancels the command.
askAnswer :: (MonadIO m) => Terminal -> String -> (Text -> Either String a) -> ExceptT String m a
askAnswer term prompt reader =
  retrying term $ do
    answer <- liftIO (askVisible term prompt) >>= liftEither
    return $ reader (Text.strip answer)

-- | One of the choices, each with the answers that pick it, the first of
-- them shown. An empty answer picks the first choice. An answer that picks
-- none is a mistake, and the end of the input cancels the command.
askChoice :: (MonadIO m) => Terminal -> String -> [([Text], a)] -> ExceptT String m a
askChoice _ _ [] = throwError "There is nothing to choose from"
askChoice term question choices@((firstNames, firstChoice) : _) =
  askAnswer term (question <> " [" <> shown firstNames <> "] ") picked
  where
    picked "" = Right firstChoice
    picked answer =
      case [choice | (names, choice) <- choices, answer `elem` names] of
        choice : _ -> Right choice
        [] -> Left $ "Type one of " <> intercalate ", " (shown . fst <$> choices) <> ", or nothing for " <> shown firstNames
    shown names = maybe "" Text.unpack (listToMaybe names)

{- The mnemonic -}

-- | A mnemonic as typed, in its canonical form, the words of the list in
-- lower case separated by single spaces; or what is wrong with it, without
-- repeating any of its words, which are secret
readMnemonic :: Text -> Either String Mnemonic
readMnemonic typed
  | length ws `notElem` [12, 15, 18, 21, 24] =
      Left $ "A mnemonic has 12, 15, 18, 21 or 24 words, not " <> show (length ws)
  | Just position <- elemIndex False (map (`Map.member` wordListMap) ws) =
      Left $ "Word " <> show (position + 1) <> " is not a word of the mnemonic list"
  | otherwise =
      either (const $ Left "The words do not check out: one of them is mistyped") Right $
        toMnemonic =<< fromMnemonic (Text.unwords ws)
  where
    ws = Text.words (Text.toLower typed)

-- | The words of a seed as typed: the mnemonic of BIP39, or the backup
-- that the shares of SLIP-39 typed rebuild
data SeedWords
  = MnemonicWords !Mnemonic
  | BackupShares !EncryptedMasterSecret
  deriving (Eq)

-- | The words of a mnemonic and the bytes of a backup are not shown. They
-- are the secret of the seed, short of its passphrase.
instance Show SeedWords where
  showsPrec d typed =
    showParen (d > 10) $ case typed of
      MnemonicWords _ -> showString "MnemonicWords <redacted>"
      BackupShares backup -> showString "BackupShares " . showsPrec 11 backup

-- | What the first words typed are: a mnemonic, or a share of a backup.
-- The number of words tells: a mnemonic has 12, 15, 18, 21 or 24, and no
-- share has any of those. Either is read as 'readMnemonic' and
-- 'readShare' read them.
readSeedWords :: Text -> Either String (Either Mnemonic Share)
readSeedWords typed
  | count `elem` [12, 15, 18, 21, 24] = Left <$> readMnemonic typed
  | count >= 20 = Right <$> readShare typed
  | otherwise = Left $ "A mnemonic has 12, 15, 18, 21 or 24 words, and a share of a backup 20 or 33, not " <> show count
  where
    count = length (Text.words typed)

-- | The words of a seed. The user types a mnemonic, or the first share of
-- a backup: hw then asks for the next shares, telling how many are needed,
-- until they rebuild the backup. What does not read is told and asked
-- again, and so are shares that do not rebuild a backup.
askSeedWords :: (MonadIO m) => Terminal -> ExceptT String m SeedWords
askSeedWords term =
  retrying term $ do
    first <- retrying term $ readSeedWords <$> hidden term "Mnemonic, or a share of a backup: "
    case first of
      Left mnemonic -> return (Right (MnemonicWords mnemonic))
      Right share -> fmap BackupShares <$> askShares term share

-- | The backup of a first share and of the shares asked after it, as many
-- as it takes; or why they do not rebuild one. Before each share hw tells
-- what is typed of what is needed. A share that does not go with the
-- others is told and asked again.
askShares :: (MonadIO m) => Terminal -> Share -> ExceptT String m (Either String EncryptedMasterSecret)
askShares term share = liftEither (addShare noShares share) >>= collect
  where
    collect typed = case progress typed of
      Enough shares -> return (rebuilt shares)
      Needs needed -> do
        more <- retrying term $ (readShare >=> addShare typed) <$> hidden term (needed <> ". Next share: ")
        collect more

-- | A passphrase, typed once; nothing when the seed has none
askPassphrase :: (MonadIO m) => Terminal -> ExceptT String m Text
askPassphrase term = hidden term "Passphrase, or nothing if the seed has none: "

-- | The passphrase of the words typed, typed once. One that does not go
-- with the words is told and asked again ('passphraseOf').
askPassphraseOf :: (MonadIO m) => Terminal -> SeedWords -> ExceptT String m Text
askPassphraseOf term ws = retrying term (passphraseOf ws <$> askPassphrase term)

-- | The passphrase as it goes with the words of a seed. That of a backup
-- in shares has printable ASCII characters only, code points 32 to 126:
-- its format asks so, and haskoin-core decrypts a backup with no other.
-- A passphrase with other characters has several encodings that look the
-- same on screen, and each would give another seed. A mnemonic takes any
-- passphrase, which BIP39 normalizes.
passphraseOf :: SeedWords -> Text -> Either String Text
passphraseOf ws passphrase = case ws of
  BackupShares _
    | not (Text.all printableAscii passphrase) ->
        Left "The passphrase of a backup in shares has printable ASCII characters only, as its format asks"
  _ -> Right passphrase
  where
    printableAscii c = ord c >= 32 && ord c <= 126

-- | The secret of the words of a seed with its passphrase
withPassphrase :: SeedWords -> Text -> SeedSecret
withPassphrase (MnemonicWords mnemonic) passphrase = MnemonicSecret (MnemonicPass mnemonic passphrase)
withPassphrase (BackupShares backup) passphrase = SharesSecret backup passphrase

-- | The words and the passphrase of a seed, typed once each, for a
-- command that checks them: against the accounts, or on the chain
askSeedSecret :: (MonadIO m) => Terminal -> ExceptT String m SeedSecret
askSeedSecret term = do
  ws <- askSeedWords term
  withPassphrase ws <$> askPassphraseOf term ws

-- | The words and the passphrase of a seed an account is created from.
--
-- A mnemonic is typed twice: a typo that its checksum lets through, one
-- time in 16 for 12 words, makes another seed, and an account whose funds
-- the mnemonic written down could not spend. The shares of a backup are
-- typed once: the checksum of a share catches any three wrong words, and
-- the digest of the backup a share that is not of it.
--
-- The passphrase is typed twice. One that does not go with the words is
-- told and asked again, as for every command ('passphraseOf').
askNewSeedSecret :: (MonadIO m) => Terminal -> ExceptT String m SeedSecret
askNewSeedSecret term = do
  ws <- confirmedWords
  withPassphrase ws <$> confirmedPassphrase ws
  where
    confirmedWords = retrying term $ do
      first <- askSeedWords term
      case first of
        BackupShares _ -> return (Right first)
        MnemonicWords _ -> do
          liftIO $ tell term "Type the mnemonic again, to check it"
          second <- askSeedWords term
          return $ if first == second then Right first else Left "The mnemonics do not match"
    confirmedPassphrase ws = retrying term $ do
      first <- askPassphrase term
      case passphraseOf ws first of
        Left refused -> return (Left refused)
        Right _ | Text.null first -> return (Right first)
        Right _ -> do
          second <- hidden term "Passphrase again: "
          return $ if first == second then Right first else Left "The passphrases do not match"

{- The signing key of an account -}

-- | The signing key of an account of a kind from the words and the
-- passphrase of its seed, with the public side of the seed. Both are asked
-- again while the key they give is not the account's: a wrong word the
-- checksum of a mnemonic lets through gives another seed, which no
-- passphrase can fix, and any passphrase decrypts a backup in shares to
-- some seed. The one place a private key comes out of the secret of a seed
-- for a command.
askSigningKey :: (MonadIO m) => Terminal -> AccountKind -> Network -> Ctx -> Natural -> XPubKey -> ExceptT String m (PublicSeed, XPrvKey)
askSigningKey term kind net ctx index accPub =
  retrying term $ do
    secret <- askSeedSecret term
    maybe (Left (mismatch secret)) Right
      <$> matchAccount term kind net ctx index accPub secret
  where
    mismatch (MnemonicSecret _) = "The mnemonic or the passphrase does not match the account"
    mismatch (SharesSecret _ _) = "The shares or the passphrase do not match the account"

-- | The seed and the signing key of an account of a kind that the secret
-- of a seed gives, if it gives its public key. The passphrase of a
-- mnemonic is tried normalized, as BIP39 asks, then, when normalizing
-- changes it, as typed: hw derived its accounts so before, and the user is
-- told that other wallets would not find such an account.
matchAccount :: (MonadIO m) => Terminal -> AccountKind -> Network -> Ctx -> Natural -> XPubKey -> SeedSecret -> ExceptT String m (Maybe (PublicSeed, XPrvKey))
matchAccount term kind net ctx index accPub secret = do
  derived <- liftIO . accountOf =<< liftEither (secretMasterKey secret)
  case derived of
    Just found -> return (Just found)
    Nothing -> do
      asTyped <- liftEither (secretTypedMasterKey secret)
      typed <- maybe (return Nothing) (liftIO . accountOf) asTyped
      forM_ typed $ \_ -> liftIO $ tell term typedPassphraseWarning
      return typed
  where
    accountOf master = do
      let seed = seedOfMaster ctx master
      derived <- seedAccountKey seed kind net index
      return $
        if derived == Just accPub
          then Just (seed, accountKey kind net ctx master index)
          else Nothing

-- | What the user is told of an account derived from a passphrase as typed
typedPassphraseWarning :: String
typedPassphraseWarning =
  "This account comes from the passphrase as typed, which hw did not normalize as BIP39 asks: \
  \other wallets would not find it from this mnemonic and passphrase. \
  \Moving its funds to an account created now keeps them within reach of any wallet."

-- | What the user is told when createacc keeps to the seed of the
-- passphrase as typed, the one the accounts of the wallet are of
typedPassphraseCreated :: String
typedPassphraseCreated =
  "The accounts of this seed come from the passphrase as typed, which hw did not normalize as BIP39 asks, \
  \and so does this one, to keep them together: other wallets would not find it from this mnemonic and passphrase."

-- | What the user is told when recover finds accounts from the passphrase
-- as typed
typedPassphraseRecovered :: String
typedPassphraseRecovered =
  "The accounts found come from the passphrase as typed, which hw did not normalize as BIP39 asks: \
  \other wallets would not find them from this mnemonic and passphrase. \
  \Moving their funds to an account created now keeps them within reach of any wallet."

{- Dice rolls -}

-- | The faces of a die on a typed line, from 1 to 6, spaces between them or
-- not; or what is wrong with the line, without repeating it, since the
-- rolls are secret
readRolls :: Text -> Either String [Natural]
readRolls line
  | Text.null faces = Left "The line has no roll: type the faces the die shows, from 1 to 6"
  | Text.all (`elem` ['1' .. '6']) faces = Right $ map (fromIntegral . digitToInt) (Text.unpack faces)
  | otherwise = Left "The line has something else than faces from 1 to 6: none of its rolls were taken"
  where
    faces = Text.filter (not . isSpace) line

-- | Entropy of the given number of bytes from a die the user rolls. The
-- faces are typed without echo, as many on a line as the user likes, and
-- after each line hw tells how many rolls it took and about how many are
-- left. A line with anything else than faces is told and typed again.
askDiceEntropy :: (MonadIO m) => Terminal -> Natural -> ExceptT String m BS.ByteString
askDiceEntropy term bytes = do
  say $ "Roll a die about " <> show (rollsFor needed) <> " times, and type the faces it shows, from 1 to 6"
  say "Type as many faces on a line as you like: they are not shown"
  collect []
  where
    needed = 8 * fromIntegral bytes
    say = liftIO . tell term
    collect taken = do
      line <- retrying term $ readRolls <$> hidden term "Rolls: "
      let rolled = taken <> line
          have = length (diceBits rolled)
      case diceEntropy bytes rolled of
        Just entropy -> do
          say $ "Enough rolls: the " <> show needed <> " bits are complete"
          return entropy
        Nothing -> do
          say $ rolls (length line) <> " taken: " <> show have <> " of the " <> show needed <> " bits, about " <> rolls (rollsFor (needed - have)) <> " to go"
          collect rolled
    rolls n = show n <> if n == 1 then " roll" else " rolls"

-- | About how many rolls give a number of bits: a roll gives two bits for
-- four faces of six and one bit for the other two, 5/3 bits on average
rollsFor :: Int -> Int
rollsFor bits = (3 * bits + 4) `div` 5

{- The private keys of a sweep -}

-- | The private keys of a sweep, from where the user keeps them: a file of
-- UTF-8 text, or typed on the terminal without echo
readSecKeys :: (MonadIO m) => Terminal -> Network -> KeySource -> ExceptT String m [SecKey]
readSecKeys _ net (KeysInFile file) = do
  contents <- liftIO $ try (BS.readFile file)
  case contents of
    Left e
      | isDoesNotExistError e -> throwError "The private key file does not exist"
      | otherwise -> throwError $ "The private key file could not be read: " <> show e
    Right bytes ->
      either (const $ throwError "The private key file is not UTF-8 text") (parseKeys net) (decodeUtf8' bytes)
readSecKeys term net KeysOnTerminal =
  parseKeys net =<< hidden term "Private keys (WIF, minikey or hex, separated by spaces): "

-- | The private keys of a text, at least one
parseKeys :: (Monad m) => Network -> Text -> ExceptT String m [SecKey]
parseKeys net text = do
  keys <- liftEither $ parseSecKeys net text
  when (null keys) $ throwError "No private keys were given"
  return keys
