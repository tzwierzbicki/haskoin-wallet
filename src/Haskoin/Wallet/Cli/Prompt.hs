{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What hw asks the user for on the terminal: the mnemonic of a wallet,
-- or the shares of its backup, and its passphrase, or its master private
-- key in their place, which give the signing key of an account; the dice
-- rolls that add to the entropy of a new
-- mnemonic; the
-- private keys of a sweep, when they are not in a file, on as many lines
-- as the user likes; the answer to a
-- yes-or-no question, such as whether to send a transaction or to take a
-- high fee rate; a choice among a few, such as the address the change of
-- a payment goes back to; and an answer read as it is typed, such as a fee
-- rate.
--
-- Secrets are typed without echo, as gpg, ssh and sudo take them: nothing
-- shows on the screen, not even their length. The prompts and the messages
-- go to stderr, so that they never mix with what a command writes, its
-- JSON for instance. A long one is wrapped between words
-- ("Haskoin.Wallet.Cli.Wrap"). A question allows three attempts, each mistake told
-- before the next one, and the end of the input cancels the command. The
-- error then says how a script answers the question (the type
-- t'ScriptAnswer').
--
-- A terminal keeps what is typed before a program reads it, and gives
-- what hw did not read to the shell when hw ends: the shell shows it, runs
-- it as a command and keeps it in its history. A secret pasted with a
-- line too many would end there, a list of private keys on several lines
-- for instance. So hw forgets what was typed ahead when it ends, once it
-- has asked for a secret ('forgetTypedSecrets').
module Haskoin.Wallet.Cli.Prompt
  ( Terminal (..),
    terminal,
    terminalOn,
    forgetTypedSecrets,
    attempts,
    cancelled,
    notUtf8,
    ScriptAnswer (..),
    cancelledIn,
    askYes,
    askAnswer,
    askChoice,
    askChoiceFrom,
    readMnemonic,
    SeedWords (..),
    readSeedWords,
    askSeedWords,
    askSeedSecret,
    askNewSeedSecret,
    askNewPassphraseOf,
    backupOfShares,
    askSigningKey,
    askSigningKeys,
    matchAccount,
    matchAccounts,
    readRolls,
    askDiceEntropy,
    KeysWanted (..),
    anyKeys,
    keysOfCoins,
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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8')
import Haskoin
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret, Share)
import Haskoin.Wallet.Cli.Wrap (wrapText)
import Haskoin.Wallet.Keys.Descriptor (isPrivateKey)
import Haskoin.Wallet.Keys.Entropy (diceBits, diceEntropy)
import Haskoin.Wallet.FileIO (readTextFile)
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Keys.Shares (Progress (..), addShare, noShares, progress, readShare, rebuilt)
import Numeric.Natural (Natural)
import qualified System.IO as IO
import System.IO.Error (isEOFError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (QueueSelector (InputQueue), discardData, queryTerminal)

{- The terminal -}

-- | Where hw asks: a line typed without echo, or one typed as usual, or why
-- there is none, and a message for the user
data Terminal = Terminal
  { askHidden :: String -> IO (Either String Text),
    askVisible :: String -> IO (Either String Text),
    tell :: String -> IO (),
    -- | All that is left of an input that has an end, a pipe or a file,
    -- asked with one prompt, or why it cannot be read: a secret of
    -- several lines, which a script writes as a file has it. Nothing on a
    -- terminal, which has no end: the user is asked line by line there.
    askToEnd :: String -> IO (Maybe (Either String Text)),
    -- | Forget what was typed ahead on a terminal, and not read yet. Lines
    -- pasted after a secret would otherwise answer the next question, or
    -- reach the shell.
    forgetTyped :: IO ()
  }

-- | The terminal of the user: stdin, with the prompts and the messages on
-- stderr. It notes that a secret was asked for, so that hw forgets what
-- was typed ahead when it ends ('forgetTypedSecrets').
terminal :: Terminal
terminal =
  onHandles
    { askHidden = \prompt -> writeIORef secretAsked True >> askHidden onHandles prompt,
      forgetTyped = discardTyped
    }
  where
    onHandles = terminalOn IO.stdin IO.stderr

-- | A terminal reading from one handle and writing to another. The echo of
-- a hidden line is off from before its prompt shows until it is typed, and
-- back as it was afterwards, whatever happens; the Enter key, not echoed
-- either, gets its new line.
-- A line is read as bytes up to the new line, a byte that UTF-8 never uses
-- inside a character, and decoded as UTF-8, whatever the locale: a
-- passphrase with accents gives the same seed in every terminal, and bytes
-- that are not UTF-8 are refused rather than read as another passphrase.
-- It forgets nothing typed ahead: only the terminal of the user does
-- ('terminal').
terminalOn :: IO.Handle -> IO.Handle -> Terminal
terminalOn input output =
  Terminal
    { askHidden = askLine False,
      askVisible = askLine True,
      tell = IO.hPutStrLn output . wrapText,
      askToEnd = askRest,
      forgetTyped = return ()
    }
  where
    askLine shown prompt = do
      echo <- IO.hGetEcho input
      typed <- bracket_ (IO.hSetEcho input shown) (IO.hSetEcho input echo) $ do
        showPrompt prompt
        try (C8.hGetLine input)
      -- A terminal echoes the Enter key of a line shown; nothing else does
      onTerminal <- IO.hIsTerminalDevice input
      unless (shown && onTerminal) $ IO.hPutStrLn output ""
      case typed of
        Right line -> return $ decoded (stripReturn line)
        Left e
          | isEOFError e -> return (Left cancelled)
          | otherwise -> ioError e
    askRest prompt = do
      onTerminal <- IO.hIsTerminalDevice input
      if onTerminal
        then return Nothing
        else do
          showPrompt prompt
          IO.hPutStrLn output ""
          rest <- readToEnd
          return . Just $ if BS.null rest then Left cancelled else decoded rest
    showPrompt prompt = IO.hPutStr output (wrapText prompt) >> IO.hFlush output
    -- The handle stays open, so that a question asked later finds the
    -- end of the input, as after a last line
    readToEnd = do
      more <- BS.hGetSome input 65536
      if BS.null more then return BS.empty else (more <>) <$> readToEnd
    decoded = either (const $ Left notUtf8) Right . decodeUtf8'
    -- A line typed on Windows, or pasted from there, ends with a return
    stripReturn line = fromMaybe line (BS.stripSuffix "\r" line)

-- | Whether hw asked for a secret on the terminal of the user. The
-- terminal is the one of the process, and so is this note of it: nothing
-- else in hw is kept so.
secretAsked :: IORef Bool
secretAsked = unsafePerformIO (newIORef False)
{-# NOINLINE secretAsked #-}

-- | Forget what was typed ahead on the terminal of the user, when hw asked
-- for a secret there: hw runs this as it ends, whether it failed or not.
-- A command that asked for no secret leaves what was typed ahead to the
-- shell, as a user who types the next command while hw works expects.
forgetTypedSecrets :: IO ()
forgetTypedSecrets = do
  asked <- readIORef secretAsked
  when asked discardTyped

-- | Discard the input that the terminal of the user holds and that
-- nothing has read yet. An input that is no terminal is left alone: a
-- pipe holds what a script wrote for hw.
discardTyped :: IO ()
discardTyped = do
  onTerminal <- queryTerminal stdInput
  when onTerminal $ discardData stdInput InputQueue

-- | A line typed without echo; the end of the input cancels the command
hidden :: (MonadIO m) => Terminal -> String -> ExceptT String m Text
hidden term prompt = liftIO (askHidden term prompt) >>= liftEither

-- | The error when the input ends before the answer
cancelled :: String
cancelled = "Cancelled: the input ended before the answer"

-- | How a question is answered where nobody types, as in a script. The
-- input has ended there, and the error of the question says what the
-- script lacks ('cancelledIn'). "The input ended" alone left the writer
-- of a script to find the option in the README.
data ScriptAnswer
  = -- | An option answers the question in advance, named as it is typed:
    -- "--yes", or "--spend-dust or --leave-dust"
    ScriptOption String
  | -- | No option answers the question, on purpose. A person must, at a
    -- terminal, for the reason given as the end of a sentence: "to see
    -- what the store reports".
    NoScriptOption String
  | -- | A line of the input answers the question, as a script writes the
    -- answers of the setup: nothing to add
    ScriptLine
  deriving (Eq, Show)

-- | The error when the input ends before the answer to a question, with
-- how a script answers it. It names no command to run again: a sending
-- asks the question of its sync once the transaction is sent, and must
-- not be run a second time.
cancelledIn :: ScriptAnswer -> String
cancelledIn inScript = case inScript of
  ScriptOption option -> cancelled <> ". In a script, " <> option <> " answers this question."
  NoScriptOption why -> cancelled <> ". No option answers this question: it needs a person at a terminal, " <> why <> "."
  ScriptLine -> cancelled

-- | A line typed in answer to a question, shown as it is typed. The end
-- of the input cancels the command, with how a script answers the
-- question ('cancelledIn').
answerLine :: (MonadIO m) => Terminal -> ScriptAnswer -> String -> ExceptT String m Text
answerLine term inScript prompt = do
  typed <- liftIO (askVisible term prompt)
  -- A terminal tells the end of its input by this error ('terminalOn')
  either (\problem -> throwError (if problem == cancelled then cancelledIn inScript else problem)) return typed

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
-- Any other answer is a no, and the end of the input cancels the command,
-- with how a script answers the question.
askYes :: (MonadIO m) => Terminal -> ScriptAnswer -> String -> ExceptT String m Bool
askYes term inScript question = do
  answer <- answerLine term inScript (question <> " [y/N] ")
  return $ Text.toLower (Text.strip answer) `elem` ["y", "yes"]

-- | What the reader makes of the answer to a prompt, without the spaces
-- around it. An answer it refuses is a mistake, told with the reason it
-- gives, and the end of the input cancels the command, with how a script
-- answers the question.
askAnswer :: (MonadIO m) => Terminal -> ScriptAnswer -> String -> (Text -> Either String a) -> ExceptT String m a
askAnswer term inScript prompt reader =
  retrying term $ reader . Text.strip <$> answerLine term inScript prompt

-- | One of the choices, each with the answers that pick it, the first of
-- them shown. An empty answer picks the first choice. An answer picks a
-- choice in capitals or in small letters, as a yes or no question takes
-- its answer: BIP84 is bip84, and a Bech32 address is the same address
-- in capitals. An answer that picks none is a mistake, and the end of
-- the input cancels the command.
askChoice :: (MonadIO m) => Terminal -> ScriptAnswer -> String -> [([Text], a)] -> ExceptT String m a
askChoice = askChoiceFrom 0

-- | One of the choices, as 'askChoice' asks it, where an empty answer
-- picks the choice at the place given, from 0, and not the first. A
-- question about a setting lists its choices in one order whatever the
-- setting is now, and an empty answer keeps the setting.
askChoiceFrom :: (MonadIO m) => Int -> Terminal -> ScriptAnswer -> String -> [([Text], a)] -> ExceptT String m a
askChoiceFrom at term inScript question choices = case drop at choices of
  [] -> throwError "There is nothing to choose from"
  (emptyNames, emptyChoice) : _ ->
    askAnswer term inScript (question <> " [" <> shown emptyNames <> "] ") picked
    where
      picked "" = Right emptyChoice
      picked answer =
        case [choice | (names, choice) <- choices, Text.toLower answer `elem` (Text.toLower <$> names)] of
          choice : _ -> Right choice
          [] -> Left $ "Type one of " <> intercalate ", " (shown . fst <$> choices) <> ", or nothing for " <> shown emptyNames
  where
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
-- that the shares of SLIP-39 typed rebuild. Or, in place of words, the
-- master private key, which a script gives on the input of hw: it is the
-- secret whole, with no passphrase to ask after it.
data SeedWords
  = MnemonicWords !Mnemonic
  | BackupShares !EncryptedMasterSecret
  | MasterPrivateKey !XPrvKey
  deriving (Eq)

-- | The words of a mnemonic, the bytes of a backup and a master key are
-- not shown. They are the secret of the seed, short of its passphrase.
instance Show SeedWords where
  showsPrec d typed =
    showParen (d > 10) $ case typed of
      MnemonicWords _ -> showString "MnemonicWords <redacted>"
      BackupShares backup -> showString "BackupShares " . showsPrec 11 backup
      MasterPrivateKey _ -> showString "MasterPrivateKey <redacted>"

-- | What the first line typed is: the whole of a seed, a mnemonic or a
-- master private key, or a share of a backup, which asks for the next
-- ones. A single word that starts as a private key of BIP32 does is read
-- as a master key ('readMasterKey'). Otherwise the number of words tells:
-- a mnemonic has 12, 15, 18, 21 or 24, and no share has any of those.
-- Either is read as 'readMnemonic' and 'readShare' read them.
readSeedWords :: Text -> Either String (Either SeedWords Share)
readSeedWords typed
  | [word] <- Text.words typed, isPrivateKey word = Left . MasterPrivateKey <$> readMasterKey word
  | count `elem` [12, 15, 18, 21, 24] = Left . MnemonicWords <$> readMnemonic typed
  | count >= 20 = Right <$> readShare typed
  | otherwise = Left $ "A mnemonic has 12, 15, 18, 21 or 24 words, and a share of a backup 20 or 33, not " <> show count
  where
    count = length (Text.words typed)

-- | The words of a seed. The user types a mnemonic, or the first share of
-- a backup: hw then asks for the next shares, telling how many are needed,
-- until they rebuild the backup. A master private key is taken in their
-- place, though the question does not offer it: it is for the input of a
-- script, and nobody types 111 characters without seeing them. What does
-- not read is told and asked again, and so are shares that do not rebuild
-- a backup.
askSeedWords :: (MonadIO m) => Terminal -> ExceptT String m SeedWords
askSeedWords term =
  retrying term $ do
    first <- retrying term $ readSeedWords <$> hidden term "Mnemonic, or a share of a backup: "
    case first of
      Left whole -> return (Right whole)
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
-- with the words is told and asked again (@passphraseOf@).
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

-- | The secret of the words of a seed with its passphrase. A master
-- private key is the secret already: the passphrase of its seed went into
-- it, so it takes none.
withPassphrase :: SeedWords -> Text -> SeedSecret
withPassphrase (MnemonicWords mnemonic) passphrase = MnemonicSecret (MnemonicPass mnemonic passphrase)
withPassphrase (BackupShares backup) passphrase = SharesSecret backup passphrase
withPassphrase (MasterPrivateKey master) _ = MasterKeySecret master

-- | The secret of what was typed of a seed: that of its words, with the
-- passphrase the given question asks for them; or the master private key,
-- after which nothing is asked
secretOfTyped :: (Monad m) => (SeedWords -> m SeedSecret) -> SeedWords -> m SeedSecret
secretOfTyped _ (MasterPrivateKey master) = return (MasterKeySecret master)
secretOfTyped withAskedPassphrase ws = withAskedPassphrase ws

-- | The words and the passphrase of a seed, typed once each, or its
-- master private key alone, for a command that checks them: against the
-- accounts, or on the chain
askSeedSecret :: (MonadIO m) => Terminal -> ExceptT String m SeedSecret
askSeedSecret term =
  askSeedWords term >>= secretOfTyped (\ws -> withPassphrase ws <$> askPassphraseOf term ws)

-- | The words and the passphrase of a seed an account is created from.
--
-- A mnemonic is typed twice: a typo that its checksum lets through, one
-- time in 16 for 12 words, makes another seed, and an account whose funds
-- the mnemonic written down could not spend. The shares of a backup are
-- typed once: the checksum of a share catches any three wrong words, and
-- the digest of the backup a share that is not of it. A master private
-- key is given once too: its checksum of four bytes catches a typo.
--
-- The passphrase is typed twice. One that does not go with the words is
-- told and asked again, as for every command (@passphraseOf@). None is
-- asked after a master private key.
askNewSeedSecret :: (MonadIO m) => Terminal -> ExceptT String m SeedSecret
askNewSeedSecret term = confirmedWords >>= secretOfTyped (confirmedPassphraseOf term (askPassphrase term))
  where
    confirmedWords = retrying term $ do
      first <- askSeedWords term
      case first of
        MnemonicWords _ -> do
          liftIO $ tell term "Type the mnemonic again, to check it"
          second <- askSeedWords term
          return $ if first == second then Right first else Left "The mnemonics do not match"
        _ -> return (Right first)

-- | The secret of the words newportfolio has just made, with the
-- passphrase the user gives them now, typed twice: the passphrase is the
-- one part of the seed hw cannot make.
--
-- The question is not the one of @askPassphrase@, which asks for the
-- passphrase "if the seed has none". No seed exists yet here. The user
-- chooses whether the new wallet gets a passphrase, and a question about
-- one that a seed has reads as a question about something they should
-- know already.
askNewPassphraseOf :: (MonadIO m) => Terminal -> SeedWords -> ExceptT String m SeedSecret
askNewPassphraseOf term = confirmedPassphraseOf term (hidden term "Passphrase for the new wallet, or nothing for none: ")

-- | The secret of the words of a seed a wallet is created from, with
-- their passphrase, which the given question asks for, typed twice. An
-- empty passphrase is typed once. One that does not go with the words is
-- told and asked again (@passphraseOf@), and so are two that differ.
confirmedPassphraseOf :: (MonadIO m) => Terminal -> ExceptT String m Text -> SeedWords -> ExceptT String m SeedSecret
confirmedPassphraseOf term ask ws =
  fmap (withPassphrase ws) . retrying term $ do
    first <- ask
    case passphraseOf ws first of
      Left refused -> return (Left refused)
      Right _ | Text.null first -> return (Right first)
      Right _ -> do
        second <- hidden term "Passphrase again: "
        return $ if first == second then Right first else Left "The passphrases do not match"

-- | The backup that the shares of a new backup rebuild, as many of them
-- as it takes: what newportfolio derives the wallet from once it has
-- made the shares
backupOfShares :: [Mnemonic] -> Either String EncryptedMasterSecret
backupOfShares = go noShares
  where
    go typed texts = case progress typed of
      Enough shares -> rebuilt shares
      Needs _ -> case texts of
        [] -> Left "The shares do not rebuild a backup"
        text : rest -> readShare text >>= addShare typed >>= (`go` rest)

{- The signing key of an account -}

-- | The signing key of an account of a kind from the words and the
-- passphrase of its seed, or from its master private key, with the public
-- side of the seed. They are asked again while the key they give is not
-- the account's: a wrong word the checksum of a mnemonic lets through
-- gives another seed, which no passphrase can fix, any passphrase
-- decrypts a backup in shares to some seed, and a master key may be that
-- of another wallet.
askSigningKey :: (MonadIO m) => Terminal -> AccountKind -> Network -> Natural -> XPubKey -> ExceptT String m (PublicSeed, XPrvKey)
askSigningKey term kind net index accPub =
  retrying term $ do
    secret <- askSeedSecret term
    maybe (Left (mismatch "account" secret)) Right
      <$> matchAccount kind net index accPub secret

-- | The signing keys of the accounts of a portfolio on a network, each
-- with its kind, from the words and the passphrase of the seed, with the
-- public side of the seed. The accounts are given by their kinds and
-- their public keys, and the index is that of the portfolio. They are
-- asked again while the keys they give are not those of the accounts. A
-- payment that spends BIP44 and BIP84 coins of a portfolio together is
-- signed with the keys of both accounts, from the mnemonic typed once.
-- With 'askSigningKey', the one place a private key comes out of the
-- secret of a seed for a command.
askSigningKeys :: (MonadIO m) => Terminal -> Network -> Natural -> [(AccountKind, XPubKey)] -> ExceptT String m (PublicSeed, [(AccountKind, XPrvKey)])
askSigningKeys term net index accounts =
  retrying term $ do
    secret <- askSeedSecret term
    maybe (Left (mismatch "portfolio" secret)) Right
      <$> matchAccounts net index accounts secret

-- | What is told of a secret that does not give the keys of what it is
-- asked for, an account or a portfolio
mismatch :: String -> SeedSecret -> String
mismatch asked secret = typed <> " not match the " <> asked
  where
    typed = case secret of
      MnemonicSecret _ -> "The mnemonic or the passphrase does"
      SharesSecret _ _ -> "The shares or the passphrase do"
      MasterKeySecret _ -> "The master private key does"

-- | The seed and the signing key of an account of a kind that the secret
-- of a seed gives, if it gives its public key: 'matchAccounts' for one
-- account
matchAccount :: (MonadIO m) => AccountKind -> Network -> Natural -> XPubKey -> SeedSecret -> ExceptT String m (Maybe (PublicSeed, XPrvKey))
matchAccount kind net index accPub secret = do
  matched <- matchAccounts net index [(kind, accPub)] secret
  return $ case matched of
    Just (seed, [(_, key)]) -> Just (seed, key)
    _ -> Nothing

-- | The seed and the signing keys of accounts at one index on a network
-- that the secret of a seed gives, if it gives the public key of every
-- one of them. The passphrase of a mnemonic is normalized as BIP39 asks
-- ('Haskoin.Wallet.Keys.masterKey').
matchAccounts :: (MonadIO m) => Network -> Natural -> [(AccountKind, XPubKey)] -> SeedSecret -> ExceptT String m (Maybe (PublicSeed, [(AccountKind, XPrvKey)]))
matchAccounts net index accounts secret = do
  master <- liftEither (secretMasterKey secret)
  let seed = seedOfMaster master
  derived <- liftIO $ mapM (\(kind, _) -> seedAccountKey seed kind net index) accounts
  return $
    if not (null accounts) && derived == map (Just . snd) accounts
      then Just (seed, [(kind, accountKey kind net master index) | (kind, _) <- accounts])
      else Nothing

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
-- four faces of six and one bit for the other two, 5\/3 bits on average
rollsFor :: Int -> Int
rollsFor bits = (3 * bits + 4) `div` 5

{- The private keys of a sweep -}

-- | What the private keys typed on a terminal are for, which tells when
-- they are complete: after the keys of a line, nothing more to ask for, or
-- the prompt of the next line and what is wanted after it. A terminal has
-- no end as a file has, so only the purpose of the keys can end them, or
-- the user, with an empty line.
newtype KeysWanted = KeysWanted {afterKeys :: [SecKey] -> Maybe (String, KeysWanted)}

-- | The keys of a sweep to prepare. They are never complete: the keys say
-- which coins the sweep spends, so only the user knows that none is left.
-- Each prompt tells how many keys were read.
anyKeys :: KeysWanted
anyKeys = counting 0
  where
    counting have = KeysWanted $ \keys ->
      let total = have + length keys
       in Just (keysRead total <> " More private keys, or Enter to continue: ", counting total)
    keysRead :: Int -> String
    keysRead 1 = "1 private key read."
    keysRead n = show n <> " private keys read."

-- | The keys of a sweep to sign, which spends a coin at each address
-- given, with the addresses where a key may hold coins. They are complete
-- once every coin has its key, so one line is enough for most sweeps.
-- Each prompt tells how many coins have their key.
keysOfCoins :: (SecKey -> [Address]) -> [Address] -> KeysWanted
keysOfCoins addressesOf coins = awaiting coins
  where
    awaiting left = KeysWanted $ \keys ->
      case filter (`notElem` concatMap addressesOf keys) left of
        [] -> Nothing
        still -> Just (withKey (length coins - length still) <> " More private keys, or Enter to stop: ", awaiting still)
    withKey :: Int -> String
    withKey have
      | length coins == 1 = "The coin has no key yet."
      | have == 0 = "None of the " <> show (length coins) <> " coins has its key yet."
      | have == 1 = "1 of the " <> show (length coins) <> " coins has its key."
      | otherwise = show have <> " of the " <> show (length coins) <> " coins have their key."

-- | The private keys of a sweep, from where the user keeps them: a file of
-- UTF-8 text, or the input of hw, without echo. A pipe is read to its end,
-- as the file it stands for, so that a file of keys works both ways. On a
-- terminal the keys are typed or pasted on as many lines as the user
-- likes, until they are complete for what they are wanted for or until an
-- empty line (@askKeyLines@). Reading one line there, as hw did, left the
-- next lines of a pasted list to the shell.
readSecKeys :: (MonadIO m) => Terminal -> Network -> KeysWanted -> KeySource -> ExceptT String m [SecKey]
readSecKeys _ net _ (KeysInFile file) = parseKeys net =<< ExceptT (liftIO (readTextFile file))
readSecKeys term net wanted KeysOnTerminal = do
  piped <- liftIO $ askToEnd term keysPrompt
  case piped of
    Just text -> parseKeys net =<< liftEither text
    Nothing -> askKeyLines term net wanted

-- | The first prompt of the private keys
keysPrompt :: String
keysPrompt = "Private keys (WIF, minikey or hex, separated by spaces): "

-- | The private keys typed on a terminal, a line after another, none
-- shown: until nothing more is wanted, or until an empty line, which
-- gives the keys typed so far. A line is read as a line of a key file is
-- ('parseSecKeysOfLine'): one with words but no key fails the command,
-- named by its number, and a comment is passed over. What was typed ahead
-- after the keys is forgotten: the lines of a pasted list that follow an
-- empty one would answer the next question of the command.
askKeyLines :: (MonadIO m) => Terminal -> Network -> KeysWanted -> ExceptT String m [SecKey]
askKeyLines term net = go (1 :: Int) [] keysPrompt
  where
    go number typed prompt wanted = do
      line <- hidden term prompt
      if Text.null (Text.strip line)
        then complete typed
        else do
          keys <- liftEither $ parseSecKeysOfLine net number line
          -- A comment has no key: the same prompt asks again
          let asked
                | null keys = Just (prompt, wanted)
                | otherwise = wanted.afterKeys keys
          case asked of
            Nothing -> complete (typed <> keys)
            Just (next, still) -> go (number + 1) (typed <> keys) next still
    complete keys = do
      when (null keys) $ throwError "No private keys were given"
      liftIO $ forgetTyped term
      return keys

-- | The private keys of a text, at least one
parseKeys :: (Monad m) => Network -> Text -> ExceptT String m [SecKey]
parseKeys net text = do
  keys <- liftEither $ parseSecKeys net text
  when (null keys) $ throwError "No private keys were given"
  return keys
