{-# LANGUAGE OverloadedStrings #-}

-- | What hw asks the user on the terminal: the mnemonic, or the shares of
-- a backup, and the passphrase of a seed, or its master private key alone,
-- once to check them against a portfolio or the chain, the mnemonic and the passphrase twice to add a
-- portfolio, and the passphrase alone twice for a seed newportfolio made;
-- the signing keys of the accounts of a portfolio, asked until the
-- mnemonic and the passphrase give them, three attempts at most; the
-- private keys of a sweep, from a file, from an input that has an end, or
-- typed on as many lines as the user likes; and the questions: yes or
-- no, a pick among choices, and an answer read as typed. A scripted
-- terminal types the lines of each test and keeps what hw shows; the real
-- one is tested on files.
module Haskoin.Wallet.Cli.PromptSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Haskoin
import Haskoin.Crypto.Keys.Slip39 (EncryptedMasterSecret (..))
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Cli.Prompt
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Testnet3 (btcTestnet3)
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec

spec :: Spec
spec = do
  readingSpec
  shownWordsSpec
  mnemonicSpec
  sharesSpec
  masterKeySpec
  newSeedSpec
  seedOnceSpec
  signingSpec
  portfolioKeysSpec
  matchingSpec
  terminalSpec
  diceSpec
  sweepKeysSpec
  questionSpec

{- A scripted terminal -}

-- | Ask a question on a terminal that types the given lines, then ends:
-- the answer, what hw showed, prompts and messages in order, and the
-- lines left untyped. The lines are typed ahead, as a paste has them: hw
-- reads them one by one, and those it forgets are left to nobody.
answering :: [Text] -> (Terminal -> ExceptT String IO a) -> IO (Either String a, [String], [Text])
answering lines' question = do
  left <- newIORef lines'
  shown <- newIORef []
  let typed prompt = do
        modifyIORef shown (<> [prompt])
        rest <- readIORef left
        case rest of
          [] -> return (Left cancelled)
          line : more -> writeIORef left more >> return (Right line)
      term =
        Terminal
          { askHidden = typed,
            askVisible = typed,
            tell = \message -> modifyIORef shown (<> [message]),
            askToEnd = \_ -> return Nothing,
            forgetTyped = writeIORef left []
          }
  answer <- runExceptT (question term)
  (,,) answer <$> readIORef shown <*> readIORef left

-- | The answer alone
answerOf :: [Text] -> (Terminal -> ExceptT String IO a) -> IO (Either String a)
answerOf lines' question = (\(answer, _, _) -> answer) <$> answering lines' question

-- | Ask a question on the real terminal, with the bytes given as its
-- input, read through a file handle: what hw showed, written to another
-- file. A file is no terminal, and it has an end, as a pipe has.
typing :: BS.ByteString -> (Terminal -> IO a) -> IO String
typing bytes question =
  inScratchDirectory $ \dir -> do
    BS.writeFile (dir </> "input") bytes
    _ <- IO.withBinaryFile (dir </> "input") IO.ReadMode $ \input ->
      IO.withFile (dir </> "output") IO.WriteMode $ \output ->
        question (terminalOn input output)
    readFile (dir </> "output")

-- | The answer to a question on the real terminal, with what hw showed
typingFor :: BS.ByteString -> (Terminal -> IO a) -> IO (a, String)
typingFor bytes question = do
  answer <- newIORef Nothing
  shown <- typing bytes $ \term -> writeIORef answer . Just =<< question term
  (,) <$> (fromJust <$> readIORef answer) <*> pure shown

{- Fixtures -}

-- | The words of the test wallet, and its passphrase
testWords, testPassphrase :: Text
testWords = mnemonicWords testMnemonic
testPassphrase = mnemonicPass testMnemonic

-- | Another seed: the mnemonic of the entropy of zeros, BIP39's first vector
otherWords :: Text
otherWords = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

-- | The words of the test wallet with "snow" typed "now": the checksum,
-- four bits for twelve words, lets this typo through
typoWords :: Text
typoWords = "now senior nerve virus fabric now fringe clip marble interest analyst can"

-- | The prompts of hw
mnemonicPrompt, passphrasePrompt, newWalletPrompt, againPrompt :: String
mnemonicPrompt = "Mnemonic, or a share of a backup: "
passphrasePrompt = "Passphrase, or nothing if the seed has none: "
newWalletPrompt = "Passphrase for the new wallet, or nothing for none: "
againPrompt = "Passphrase again: "

-- | The three shares of the entropy 00 to 0f at a threshold of 2, with the
-- identifier 42, as hw makes them: see "Haskoin.Wallet.Keys.SharesSpec". And the
-- second share of the entropy 10 to 1f made the same way: a share of
-- another backup that says nothing of it, down to the identifier.
firstOfThree, secondOfThree, secondOfOther :: Text
firstOfThree = "acid fawn academic acid axis auction endless award answer random spew dryer again public civil unwrap scandal pleasure extra member"
secondOfThree = "acid fawn academic agency damage stilt math warmth satisfy step gravity prospect elder swing flexible explain subject sugar holy sled"
secondOfOther = "acid fawn academic agency change enlarge ting award step recall favorite critical arena taxi parking fiscal helpful order provide inside"

-- | The backup those shares rebuild: the entropy 00 to 0f encrypted with
-- the empty passphrase, at an iteration exponent of 1
backupOfThree :: EncryptedMasterSecret
backupOfThree = EncryptedMasterSecret 42 True 1 (fromJust (decodeHex "8376b0ea5b004f0f7cf83f4af6d32e52"))

-- | The prompts of the shares after the first, for a backup that needs
-- three
secondSharePrompt, thirdSharePrompt :: String
secondSharePrompt = "Share 1 of the 3 needed. Next share: "
thirdSharePrompt = "Share 2 of the 3 needed. Next share: "

-- | The public key of account 0 of the test words with the passphrase
-- "café", as BIP39 derives it, from the passphrase normalized
cafeAccount :: XPubKey
cafeAccount = fromJust $ xPubImport btc "xpub6D64kaV7GCsgoiy8dZB9botsFqRwDm9vHDvxPZPxbKZLEdkb4eV57Xt8GKfpcEq9rZKMN6eipk6Ubi3xFmMJUrT5ihGpsKEgcMh44XJGq4T"

-- | The public key of account 0 of the test wallet
testAccount :: XPubKey
testAccount = fromJust $ xPubImport btc testAccountXPub

{- Reading a mnemonic -}

readingSpec :: Spec
readingSpec =
  describe "A mnemonic as typed" $ do
    it "is kept in its canonical form: lower case, words separated by single spaces" $
      readMnemonic "  Snow  senior NERVE virus fabric now fringe clip marble interest analyst can "
        `shouldBe` Right testWords

    it "may have 24 words" $
      readMnemonic (Text.unwords (replicate 23 "abandon" <> ["art"]))
        `shouldBe` Right (Text.unwords (replicate 23 "abandon" <> ["art"]))

    it "is refused with a wrong number of words, without repeating them" $
      readMnemonic (Text.unwords (take 11 (Text.words testWords)))
        `shouldBe` Left "A mnemonic has 12, 15, 18, 21 or 24 words, not 11"

    it "is refused with a word that is not in the list, named by its position only" $
      readMnemonic "snow senior nerve virus fabrik now fringe clip marble interest analyst can"
        `shouldBe` Left "Word 5 is not a word of the mnemonic list"

    it "is refused when its words do not check out" $
      readMnemonic "slow senior nerve virus fabric now fringe clip marble interest analyst can"
        `shouldBe` Left "The words do not check out: one of them is mistyped"

    it "lets through a typo that keeps the checksum: only the account, or a second typing, tells" $
      readMnemonic typoWords `shouldBe` Right typoWords

{- The words of a seed, shown -}

-- | The words typed as a log, an error message or a failed test would
-- print them
shownWordsSpec :: Spec
shownWordsSpec =
  describe "The words of a seed, shown" $ do
    it "hides the words of a mnemonic" $
      show (MnemonicWords testWords) `shouldBe` "MnemonicWords <redacted>"

    -- 10282 is the identifier every share of the backup carries. It is not
    -- secret.
    it "hides the bytes of a backup in shares, and keeps its identifier" $
      show (BackupShares testSharesBackup)
        `shouldBe` "BackupShares (EncryptedMasterSecret {identifier = 10282, extendable = False, iterationExponent = 0, ciphertext = <redacted>})"

    it "hides a master private key whole" $
      show (MasterPrivateKey testMasterKey) `shouldBe` "MasterPrivateKey <redacted>"

    it "goes in parentheses inside another value, as any value does" $
      show (Just (MnemonicWords testWords)) `shouldBe` "Just (MnemonicWords <redacted>)"

{- Asking for a mnemonic -}

mnemonicSpec :: Spec
mnemonicSpec =
  describe "Asking for a mnemonic" $ do
    it "reads it whole, without echo" $
      answering [testWords] askSeedWords `shouldReturn` (Right (MnemonicWords testWords), [mnemonicPrompt], [])

    it "tells what is wrong with it and asks again" $
      answering ["snow senior", testWords] askSeedWords
        `shouldReturn` ( Right (MnemonicWords testWords),
                         [mnemonicPrompt, "A mnemonic has 12, 15, 18, 21 or 24 words, and a share of a backup 20 or 33, not 2", mnemonicPrompt],
                         []
                       )

    it "tells what is wrong with its words as for a mnemonic, when it has as many words as one" $
      answering ["slow senior nerve virus fabric now fringe clip marble interest analyst can", testWords] askSeedWords
        `shouldReturn` ( Right (MnemonicWords testWords),
                         [mnemonicPrompt, "The words do not check out: one of them is mistyped", mnemonicPrompt],
                         []
                       )

    it "fails after three mistakes, asking no more" $
      answering ["one", "two", "three", testWords] askSeedWords
        `shouldReturn` ( Left "A mnemonic has 12, 15, 18, 21 or 24 words, and a share of a backup 20 or 33, not 1",
                         [ mnemonicPrompt,
                           "A mnemonic has 12, 15, 18, 21 or 24 words, and a share of a backup 20 or 33, not 1",
                           mnemonicPrompt,
                           "A mnemonic has 12, 15, 18, 21 or 24 words, and a share of a backup 20 or 33, not 1",
                           mnemonicPrompt
                         ],
                         [testWords]
                       )

    it "is cancelled when the input ends" $
      answerOf [] askSeedWords `shouldReturn` Left cancelled

{- The shares of a backup -}

sharesSpec :: Spec
sharesSpec =
  describe "Asking for the shares of a backup" $ do
    it "takes a share for a mnemonic, then asks for the next ones, telling how many are needed" $
      answering testShares askSeedWords
        `shouldReturn` (Right (BackupShares testSharesBackup), [mnemonicPrompt, secondSharePrompt, thirdSharePrompt], [])

    it "takes the shares in any order" $
      answerOf (reverse testShares) askSeedWords `shouldReturn` Right (BackupShares testSharesBackup)

    it "asks for no other share when the first is the whole backup" $
      answering ["analysis morning academic academic center intend saver petition romp expand dryer smirk award estate sunlight deny mortgage debut dictate rocky"] askSeedWords
        `shouldReturn` (Right (BackupShares (EncryptedMasterSecret 1234 True 1 (fromJust (decodeHex "8376b0ea5b004f0f7cf83f4af6d32e52")))), [mnemonicPrompt], [])

    it "tells what is wrong with a share, without repeating its words, and asks for it again" $
      answering (take 1 testShares <> [Text.replace "acne" "acme" (testShares !! 1)] <> drop 1 testShares) askSeedWords
        `shouldReturn` ( Right (BackupShares testSharesBackup),
                         [mnemonicPrompt, secondSharePrompt, "Word 4 is not a word of the share list", secondSharePrompt, thirdSharePrompt],
                         []
                       )

    it "tells of a share typed twice, and of one of another backup, and asks again" $
      answering (take 1 testShares <> take 1 testShares <> [firstOfThree] <> drop 1 testShares) askSeedWords
        `shouldReturn` ( Right (BackupShares testSharesBackup),
                         [ mnemonicPrompt,
                           secondSharePrompt,
                           "This share was typed already",
                           secondSharePrompt,
                           "This share is of another backup than the ones before",
                           secondSharePrompt,
                           thirdSharePrompt
                         ],
                         []
                       )

    it "refuses a mnemonic where the next share is asked" $
      answering (take 1 testShares <> [testWords] <> drop 1 testShares) askSeedWords
        `shouldReturn` ( Right (BackupShares testSharesBackup),
                         [mnemonicPrompt, secondSharePrompt, "A share of a backup has 20 or 33 words, not 12", secondSharePrompt, thirdSharePrompt],
                         []
                       )

    -- The first share is a group of one; the others are of a group that
    -- needs three of its five
    it "tells how many groups are complete, and how far each one started is, for a backup of several groups" $
      (\(_, shown, _) -> shown) <$> answering testGroupShares askSeedWords
        `shouldReturn` [ mnemonicPrompt,
                         "Groups complete: 1 of the 2 needed. Next share: ",
                         "Groups complete: 1 of the 2 needed; group 3 has 1 of its 3 shares. Next share: ",
                         "Groups complete: 1 of the 2 needed; group 3 has 2 of its 3 shares. Next share: "
                       ]

    -- The second share is of another backup with the same identifier: only
    -- the digest tells. The right two are then typed.
    it "asks for the shares again from the first when they do not rebuild a backup" $
      answering [firstOfThree, secondOfOther, firstOfThree, secondOfThree] askSeedWords
        `shouldReturn` ( Right (BackupShares backupOfThree),
                         [ mnemonicPrompt,
                           "Share 1 of the 2 needed. Next share: ",
                           "These shares do not rebuild a backup: one of them is of another backup, or was altered (The digest of the secret does not match)",
                           mnemonicPrompt,
                           "Share 1 of the 2 needed. Next share: "
                         ],
                         []
                       )

    it "is cancelled when the input ends before the last share" $
      answerOf (take 2 testShares) askSeedWords `shouldReturn` Left cancelled

{- A master private key, given in place of the mnemonic -}

-- | What is told of a master private key that does not read
keyDoesNotRead :: String
keyDoesNotRead = "The master private key does not read: it is mistyped, or it is not a key that starts with xprv or tprv"

masterKeySpec :: Spec
masterKeySpec =
  describe "A master private key, given where the mnemonic is asked" $ do
    it "is taken in place of the mnemonic" $
      answering [testMasterXPrv] askSeedWords
        `shouldReturn` (Right (MasterPrivateKey testMasterKey), [mnemonicPrompt], [])

    it "is told to be wrong when mistyped, without being repeated, and asked again" $
      answering [testMasterXPrvTypo, testMasterXPrv] askSeedWords
        `shouldReturn` (Right (MasterPrivateKey testMasterKey), [mnemonicPrompt, keyDoesNotRead, mnemonicPrompt], [])

    -- The line after the key is not read: no passphrase is asked
    it "is the whole secret for a command that checks a seed: no passphrase is asked" $
      answering [testMasterXPrv, "not read"] askSeedSecret
        `shouldReturn` (Right (MasterKeySecret testMasterKey), [mnemonicPrompt], ["not read"])

    -- The checksum of the key catches a typo, so one reading is enough
    it "is given once for a new portfolio, as newportfolio --existing asks, with no passphrase" $
      answering [testMasterXPrv, "not read"] askNewSeedSecret
        `shouldReturn` (Right (MasterKeySecret testMasterKey), [mnemonicPrompt], ["not read"])

{- A seed typed twice, as newportfolio --existing asks for it -}

newSeedSpec :: Spec
newSeedSpec = do
  describe "Asking for the seed of a new portfolio, as newportfolio --existing does" $ do
    it "takes the mnemonic and the passphrase twice each" $
      answering [testWords, testWords, testPassphrase, testPassphrase] askNewSeedSecret
        `shouldReturn` ( Right (MnemonicSecret testMnemonic),
                         [mnemonicPrompt, "Type the mnemonic again, to check it", mnemonicPrompt, passphrasePrompt, againPrompt],
                         []
                       )

    it "takes the same words however they were typed the second time" $
      answerOf [testWords, Text.toUpper testWords, ""] askNewSeedSecret
        `shouldReturn` Right (MnemonicSecret (MnemonicPass testWords ""))

    it "catches a typo the checksum lets through, and asks for the mnemonic again" $
      answering [testWords, typoWords, testWords, testWords, ""] askNewSeedSecret
        `shouldReturn` ( Right (MnemonicSecret (MnemonicPass testWords "")),
                         [ mnemonicPrompt,
                           "Type the mnemonic again, to check it",
                           mnemonicPrompt,
                           "The mnemonics do not match",
                           mnemonicPrompt,
                           "Type the mnemonic again, to check it",
                           mnemonicPrompt,
                           passphrasePrompt
                         ],
                         []
                       )

    it "asks once for a passphrase left empty" $
      answering [testWords, testWords, ""] askNewSeedSecret
        `shouldReturn` ( Right (MnemonicSecret (MnemonicPass testWords "")),
                         [mnemonicPrompt, "Type the mnemonic again, to check it", mnemonicPrompt, passphrasePrompt],
                         []
                       )

    it "asks for the passphrase twice again when the two differ" $
      answering [testWords, testWords, "correct horse", "correct hrose", testPassphrase, testPassphrase] askNewSeedSecret
        `shouldReturn` ( Right (MnemonicSecret testMnemonic),
                         [ mnemonicPrompt,
                           "Type the mnemonic again, to check it",
                           mnemonicPrompt,
                           passphrasePrompt,
                           againPrompt,
                           "The passphrases do not match",
                           passphrasePrompt,
                           againPrompt
                         ],
                         []
                       )

    it "is cancelled when the input ends" $
      answerOf [testWords, testWords, testPassphrase] askNewSeedSecret `shouldReturn` Left cancelled

    -- The checksum of a share catches any three wrong words, and the digest
    -- of the backup a wrong share: nothing is left for a second typing
    it "takes the shares of a backup once, and their passphrase twice" $
      answering (testShares <> [testSharesPassphrase, testSharesPassphrase]) askNewSeedSecret
        `shouldReturn` ( Right testSharesSecret,
                         [mnemonicPrompt, secondSharePrompt, thirdSharePrompt, passphrasePrompt, againPrompt],
                         []
                       )

    it "asks once for the passphrase of a backup in shares left empty" $
      answerOf (testShares <> [""]) askNewSeedSecret
        `shouldReturn` Right (SharesSecret testSharesBackup "")

    -- The format of the shares allows printable ASCII only: another
    -- passphrase is refused before it is typed a second time
    it "refuses a passphrase with an accent for a backup in shares, and asks for another" $
      answering (testShares <> ["caf\233", testSharesPassphrase, testSharesPassphrase]) askNewSeedSecret
        `shouldReturn` ( Right testSharesSecret,
                         [ mnemonicPrompt,
                           secondSharePrompt,
                           thirdSharePrompt,
                           passphrasePrompt,
                           "The passphrase of a backup in shares has printable ASCII characters only, as its format asks",
                           passphrasePrompt,
                           againPrompt
                         ],
                         []
                       )

    it "takes the space and the tilde, the first and the last printable ASCII characters, in such a passphrase" $
      answerOf (testShares <> [" ~", " ~"]) askNewSeedSecret
        `shouldReturn` Right (SharesSecret testSharesBackup " ~")

  -- newportfolio makes the words of a new mnemonic, or the shares of a
  -- new backup, and the user types the passphrase alone
  describe "Asking for the passphrase of a seed newportfolio made" $ do
    -- No seed exists yet: the user chooses whether the new wallet gets a
    -- passphrase, so the question does not speak of the one a seed has
    it "asks for the passphrase of the new wallet, twice, and gives the secret of the words with it" $
      answering [testPassphrase, testPassphrase] (`askNewPassphraseOf` MnemonicWords testWords)
        `shouldReturn` (Right (MnemonicSecret testMnemonic), [newWalletPrompt, againPrompt], [])

    it "asks once for a passphrase left empty" $
      answering [""] (`askNewPassphraseOf` MnemonicWords testWords)
        `shouldReturn` (Right (MnemonicSecret (MnemonicPass testWords "")), [newWalletPrompt], [])

    it "asks for the passphrase twice again when the two differ" $
      answering ["correct horse", "correct hrose", testPassphrase, testPassphrase] (`askNewPassphraseOf` MnemonicWords testWords)
        `shouldReturn` ( Right (MnemonicSecret testMnemonic),
                         [newWalletPrompt, againPrompt, "The passphrases do not match", newWalletPrompt, againPrompt],
                         []
                       )

  describe "The backup that the shares of a new backup rebuild" $ do
    -- Two of the three shares rebuild the backup
    it "is rebuilt from as many shares as it takes" $
      backupOfShares [firstOfThree, secondOfThree] `shouldBe` Right backupOfThree

    it "is refused from fewer shares than it takes" $
      backupOfShares [firstOfThree] `shouldBe` Left "The shares do not rebuild a backup"

{- A seed typed once, as recover and checkmnemonic ask for it -}

seedOnceSpec :: Spec
seedOnceSpec =
  describe "Asking for a seed to check, as recover and checkmnemonic do" $ do
    it "takes the mnemonic and the passphrase once each" $
      answering [testWords, testPassphrase] askSeedSecret
        `shouldReturn` (Right (MnemonicSecret testMnemonic), [mnemonicPrompt, passphrasePrompt], [])

    it "takes the shares of a backup and the passphrase once each" $
      answering (testShares <> [testSharesPassphrase]) askSeedSecret
        `shouldReturn` (Right testSharesSecret, [mnemonicPrompt, secondSharePrompt, thirdSharePrompt, passphrasePrompt], [])

    -- The format of the shares allows printable ASCII only, for a backup
    -- that exists as for a new one: another passphrase is told, and asked
    -- again
    it "refuses a passphrase with an accent for a backup in shares, and asks for another" $
      answering (testShares <> ["caf\233", testSharesPassphrase]) askSeedSecret
        `shouldReturn` ( Right testSharesSecret,
                         [ mnemonicPrompt,
                           secondSharePrompt,
                           thirdSharePrompt,
                           passphrasePrompt,
                           "The passphrase of a backup in shares has printable ASCII characters only, as its format asks",
                           passphrasePrompt
                         ],
                         []
                       )

    -- BIP39 normalizes the passphrase of a mnemonic, whatever its characters
    it "takes a passphrase with an accent for a mnemonic" $
      answerOf [testWords, "caf\233"] askSeedSecret
        `shouldReturn` Right (MnemonicSecret (MnemonicPass testWords "caf\233"))

{- The signing key of an account -}

signingSpec :: Spec
signingSpec =
  describe "Asking for the signing key of an account, as signtx and signmsg do" $ do
    let signingKeyOf lines' = answering lines' $ \term -> keyOf <$> askSigningKey term Bip44 btc 0 testAccount
        keyOf (seed, key) = (fingerprintToText (seedFingerprint seed), xPrvExport btc key)
        signed = Right (testFingerprintText, testAccountXPrv)
        mismatch = "The mnemonic or the passphrase does not match the account"
    it "gives the key and the seed of the account from its mnemonic and passphrase" $
      signingKeyOf [testWords, testPassphrase] `shouldReturn` (signed, [mnemonicPrompt, passphrasePrompt], [])

    it "asks for both again after a wrong passphrase" $
      signingKeyOf [testWords, "correct horse", testWords, testPassphrase]
        `shouldReturn` (signed, [mnemonicPrompt, passphrasePrompt, mismatch, mnemonicPrompt, passphrasePrompt], [])

    it "asks for both again after a wrong mnemonic, which no passphrase could fix" $
      signingKeyOf [otherWords, testPassphrase, testWords, testPassphrase]
        `shouldReturn` (signed, [mnemonicPrompt, passphrasePrompt, mismatch, mnemonicPrompt, passphrasePrompt], [])

    it "fails after three attempts that do not match, asking no more" $ do
      (answer, _, left) <- signingKeyOf [typoWords, testPassphrase, typoWords, testPassphrase, typoWords, testPassphrase, testWords]
      (answer, left) `shouldBe` (Left mismatch, [testWords])

    -- The bip44 account 0 on testnet of the backup of the device tests
    let sharesAccount = fromJust $ xPubImport btcTestnet3 testSharesTestnetXPub
        sharesKeyOf lines' = answering lines' $ \term ->
          (\(seed, key) -> (fingerprintToText (seedFingerprint seed), xPrvExport btcTestnet3 key)) <$> askSigningKey term Bip44 btcTestnet3 0 sharesAccount
    it "gives the key and the seed of an account from the shares of its backup and their passphrase" $
      sharesKeyOf (testShares <> [testSharesPassphrase])
        `shouldReturn` ( Right (testSharesFingerprintText, testSharesTestnetXPrv),
                         [mnemonicPrompt, secondSharePrompt, thirdSharePrompt, passphrasePrompt],
                         []
                       )

    -- Any passphrase decrypts the backup to a seed: only the account tells
    it "asks for the shares and the passphrase again after a wrong passphrase" $ do
      (answer, shown, _) <- sharesKeyOf (testShares <> ["trezor"] <> testShares <> [testSharesPassphrase])
      answer `shouldBe` Right (testSharesFingerprintText, testSharesTestnetXPrv)
      shown
        `shouldBe` [ mnemonicPrompt,
                     secondSharePrompt,
                     thirdSharePrompt,
                     passphrasePrompt,
                     "The shares or the passphrase do not match the account",
                     mnemonicPrompt,
                     secondSharePrompt,
                     thirdSharePrompt,
                     passphrasePrompt
                   ]

    it "is cancelled when the input ends" $ do
      (answer, _, _) <- signingKeyOf [testWords]
      answer `shouldBe` Left cancelled

    it "gives the key and the seed of the account from the master private key of its wallet alone" $
      signingKeyOf [testMasterXPrv] `shouldReturn` (signed, [mnemonicPrompt], [])

    -- The master key of a wallet is the same on every network
    it "gives the key of an account on Bitcoin from a master private key written for a test network" $
      signingKeyOf [testMasterTPrv] `shouldReturn` (signed, [mnemonicPrompt], [])

    it "asks again after the master private key of another wallet, saying it does not match the account" $
      signingKeyOf [otherMasterXPrv, testMasterXPrv]
        `shouldReturn` (signed, [mnemonicPrompt, "The master private key does not match the account", mnemonicPrompt], [])

{- The signing keys of a portfolio -}

-- | The public key of the bip84 account 1 of the test wallet, of its
-- portfolio 1
segwitAccount1 :: XPubKey
segwitAccount1 = fromJust $ xPubImport btc testSegwitAccountXPub1

portfolioKeysSpec :: Spec
portfolioKeysSpec = do
  describe "Asking for the signing keys of the accounts of a portfolio, as signtx does for a payment of both types" $ do
    let keysOf accounts lines' = answering lines' $ \term -> exported <$> askSigningKeys term btc 0 accounts
        exported (seed, keys) = (fingerprintToText (seedFingerprint seed), [(kind, xPrvExport btc key) | (kind, key) <- keys])
        mismatch = "The mnemonic or the passphrase does not match the portfolio"
    it "gives the key of each account, with its type, from the mnemonic and the passphrase typed once" $
      keysOf [(Bip44, testAccount), (Bip84, testSegwitAccountPub)] [testWords, testPassphrase]
        `shouldReturn` ( Right (testFingerprintText, [(Bip44, testAccountXPrv), (Bip84, testSegwitAccountXPrv)]),
                         [mnemonicPrompt, passphrasePrompt],
                         []
                       )

    it "asks for both again after a wrong passphrase, saying it does not match the portfolio" $
      keysOf [(Bip44, testAccount), (Bip84, testSegwitAccountPub)] [testWords, "correct horse", testWords, testPassphrase]
        `shouldReturn` ( Right (testFingerprintText, [(Bip44, testAccountXPrv), (Bip84, testSegwitAccountXPrv)]),
                         [mnemonicPrompt, passphrasePrompt, mismatch, mnemonicPrompt, passphrasePrompt],
                         []
                       )

    it "gives the key of each account from the master private key of the wallet alone" $
      keysOf [(Bip44, testAccount), (Bip84, testSegwitAccountPub)] [testMasterXPrv]
        `shouldReturn` ( Right (testFingerprintText, [(Bip44, testAccountXPrv), (Bip84, testSegwitAccountXPrv)]),
                         [mnemonicPrompt],
                         []
                       )

    it "asks again after the master private key of another wallet, saying it does not match the portfolio" $
      keysOf [(Bip44, testAccount), (Bip84, testSegwitAccountPub)] [otherMasterXPrv, testMasterXPrv]
        `shouldReturn` ( Right (testFingerprintText, [(Bip44, testAccountXPrv), (Bip84, testSegwitAccountXPrv)]),
                         [mnemonicPrompt, "The master private key does not match the portfolio", mnemonicPrompt],
                         []
                       )

  describe "Matching the accounts of a portfolio with a mnemonic and a passphrase" $ do
    let matching accounts = runExceptT $ isJust <$> matchAccounts btc 0 accounts (MnemonicSecret testMnemonic)
    it "finds the accounts when the seed gives the key of each" $
      matching [(Bip44, testAccount), (Bip84, testSegwitAccountPub)] `shouldReturn` Right True

    -- The key of the bip84 account of portfolio 1 is not at index 0
    it "finds nothing when the seed gives the key of one of them only" $
      matching [(Bip44, testAccount), (Bip84, segwitAccount1)] `shouldReturn` Right False

    it "finds nothing for no account" $
      matching [] `shouldReturn` Right False

{- Matching an account -}

matchingSpec :: Spec
matchingSpec =
  describe "Matching an account with a mnemonic and a passphrase" $ do
    let matching account passphrase =
          runExceptT $ isJust <$> matchAccount Bip44 btc 0 account (MnemonicSecret (MnemonicPass testWords passphrase))
    it "finds the account BIP39 derives from an accented passphrase, typed composed" $
      matching cafeAccount "caf\233" `shouldReturn` Right True

    it "finds it as well from the passphrase typed decomposed" $
      matching cafeAccount "cafe\769" `shouldReturn` Right True

    it "finds nothing for a passphrase of another account" $
      matching cafeAccount "cafe" `shouldReturn` Right False

{- The real terminal -}

terminalSpec :: Spec
terminalSpec =
  describe "The terminal" $ do
    let asked bytes = typingFor bytes (`askHidden` "Secret: ")
    it "shows the prompt and reads a line, then goes to a new line, the Enter key not echoed" $
      asked "correct horse\n" `shouldReturn` (Right "correct horse", "Secret: \n")

    it "reads the line without the return a Windows line ends with" $
      asked "correct horse\r\n" `shouldReturn` (Right "correct horse", "Secret: \n")

    it "reads UTF-8 whatever the locale" $
      asked "caf\195\169\n" `shouldReturn` (Right "caf\233", "Secret: \n")

    it "refuses bytes that are not UTF-8, which would read as another passphrase" $
      asked "caf\233\n" `shouldReturn` (Left notUtf8, "Secret: \n")

    it "tells the end of the input" $
      asked "" `shouldReturn` (Left cancelled, "Secret: \n")

    it "writes a message on its own line" $
      typing "" (\term -> tell term "The passphrases do not match") `shouldReturn` "The passphrases do not match\n"

    -- A file is no terminal: nothing echoes the line, nor its Enter key
    it "reads a line typed in the open, then goes to a new line when the input is no terminal" $
      typingFor "yes\n" (`askVisible` "Send? ") `shouldReturn` (Right "yes", "Send? \n")

    -- A pipe or a file has an end, where a terminal has none
    it "reads all that is left of an input that has an end, after one prompt" $
      typingFor "first line\nsecond line\n" (`askToEnd` "Keys: ")
        `shouldReturn` (Just (Right "first line\nsecond line\n"), "Keys: \n")

    it "reads what is left after the lines read before" $
      typingFor "yes\nfirst line\nsecond line\n" (\term -> askVisible term "Send? " >> askToEnd term "Keys: ")
        `shouldReturn` (Just (Right "first line\nsecond line\n"), "Send? \nKeys: \n")

    it "tells the end of an input with nothing left" $
      typingFor "" (`askToEnd` "Keys: ") `shouldReturn` (Just (Left cancelled), "Keys: \n")

    it "tells the end of the input to a question that comes after all of it was read" $
      typingFor "first line\n" (\term -> askToEnd term "Keys: " >> askVisible term "Send? ")
        `shouldReturn` (Left cancelled, "Keys: \nSend? \n")

    it "refuses an input that is not UTF-8 text" $
      typingFor "caf\233\n" (`askToEnd` "Keys: ") `shouldReturn` (Just (Left notUtf8), "Keys: \n")

{- Dice rolls -}

-- | What hw tells before the rolls of one byte: about 5 rolls for its 8
-- bits, since a roll gives 5/3 bits on average
diceIntro :: [String]
diceIntro =
  [ "Roll a die about 5 times, and type the faces it shows, from 1 to 6",
    "Type as many faces on a line as you like: they are not shown"
  ]

-- | The prompt of a line of rolls
rollsPrompt :: String
rollsPrompt = "Rolls: "

diceSpec :: Spec
diceSpec = do
  describe "A line of dice rolls" $ do
    it "holds the faces typed, with or without spaces between them" $ do
      readRolls "3 1 6" `shouldBe` Right [3, 1, 6]
      readRolls "316" `shouldBe` Right [3, 1, 6]
      readRolls "  31 6 " `shouldBe` Right [3, 1, 6]

    it "is refused when it holds no roll" $ do
      readRolls "" `shouldBe` Left "The line has no roll: type the faces the die shows, from 1 to 6"
      readRolls "   " `shouldBe` Left "The line has no roll: type the faces the die shows, from 1 to 6"

    -- The rolls are secret: the message does not repeat the line
    it "is refused, none of its rolls taken, when it holds anything else than faces from 1 to 6" $ do
      readRolls "3 1 7" `shouldBe` Left "The line has something else than faces from 1 to 6: none of its rolls were taken"
      readRolls "3 0 1" `shouldBe` Left "The line has something else than faces from 1 to 6: none of its rolls were taken"
      readRolls "3, 1" `shouldBe` Left "The line has something else than faces from 1 to 6: none of its rolls were taken"

  describe "Asking for dice rolls" $ do
    it "takes the rolls of a byte from one line, without echo" $
      answering ["6 6 6 1"] (`askDiceEntropy` 1)
        `shouldReturn` (Right (BS.pack [0x01]), diceIntro <> [rollsPrompt, "Enough rolls: the 8 bits are complete"], [])

    it "takes rolls from several lines, telling after each how many it took and about how many are left" $
      answering ["6 6", "6 1"] (`askDiceEntropy` 1)
        `shouldReturn` ( Right (BS.pack [0x01]),
                         diceIntro
                           <> [ rollsPrompt,
                                "2 rolls taken: 4 of the 8 bits, about 3 rolls to go",
                                rollsPrompt,
                                "Enough rolls: the 8 bits are complete"
                              ],
                         []
                       )

    it "tells of a single roll as one" $
      answering ["5", "6 6 6 4"] (`askDiceEntropy` 1)
        `shouldReturn` ( Right (BS.pack [0x80]),
                         diceIntro
                           <> [ rollsPrompt,
                                "1 roll taken: 1 of the 8 bits, about 5 rolls to go",
                                rollsPrompt,
                                "Enough rolls: the 8 bits are complete"
                              ],
                         []
                       )

    it "drops the rolls beyond the bits it needs" $
      answerOf ["3 3 3 3 1 2 4"] (`askDiceEntropy` 1) `shouldReturn` Right (BS.pack [0xff])

    it "tells what is wrong with a line and asks for it again, taking none of its rolls" $
      answering ["6 6 7 1", "6 6 6 1"] (`askDiceEntropy` 1)
        `shouldReturn` ( Right (BS.pack [0x01]),
                         diceIntro
                           <> [ rollsPrompt,
                                "The line has something else than faces from 1 to 6: none of its rolls were taken",
                                rollsPrompt,
                                "Enough rolls: the 8 bits are complete"
                              ],
                         []
                       )

    it "fails after three wrong lines in a row, asking no more" $
      answering ["7", "", "8", "6 6 6 1"] (`askDiceEntropy` 1)
        `shouldReturn` ( Left "The line has something else than faces from 1 to 6: none of its rolls were taken",
                         diceIntro
                           <> [ rollsPrompt,
                                "The line has something else than faces from 1 to 6: none of its rolls were taken",
                                rollsPrompt,
                                "The line has no roll: type the faces the die shows, from 1 to 6",
                                rollsPrompt
                              ],
                         ["6 6 6 1"]
                       )

    -- The end of the input used to make hw ask for a roll again and again
    it "is cancelled when the input ends" $
      answerOf ["6 6"] (`askDiceEntropy` 1) `shouldReturn` Left cancelled

    -- 128 bits: about 77 rolls of 5/3 bits; 64 rolls of two bits suffice
    it "asks for about 77 rolls for the 16 bytes of a mnemonic of 12 words" $
      answering [Text.replicate 64 "6"] (`askDiceEntropy` 16)
        `shouldReturn` ( Right (BS.replicate 16 0x00),
                         [ "Roll a die about 77 times, and type the faces it shows, from 1 to 6",
                           "Type as many faces on a line as you like: they are not shown",
                           rollsPrompt,
                           "Enough rolls: the 128 bits are complete"
                         ],
                         []
                       )

    it "asks for about 154 rolls for the 32 bytes of a mnemonic of 24 words" $ do
      (_, shown, _) <- answering [] (`askDiceEntropy` 32)
      take 1 shown `shouldBe` ["Roll a die about 154 times, and type the faces it shows, from 1 to 6"]

{- The private keys of a sweep -}

sweepKeysSpec :: Spec
sweepKeysSpec =
  describe "The private keys of a sweep" $ do
    -- The key of the Bitcoin wiki's "Wallet import format" page, and the
    -- keys of the numbers 1 and 2, each with the address of its compressed
    -- public key
    let wif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617" :: Text
        wikiKey = fromJust $ secKey =<< decodeHex "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
        wifOne = "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU73sVHnoWn" :: Text
        keyOne = fromJust $ secKey =<< decodeHex "0000000000000000000000000000000000000000000000000000000000000001"
        hexTwo = "0000000000000000000000000000000000000000000000000000000000000002" :: Text
        keyTwo = fromJust $ secKey =<< decodeHex hexTwo
        wikiAddr = testAddress "1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK"
        addrOne = testAddress "1BgGZ9tcN4rm9KBzDn7KprQz87SZ26SAMH"
        addressesOf key
          | key == wikiKey = [wikiAddr]
          | key == keyOne = [addrOne]
          | otherwise = []
        fromFile file = answerOf [] $ \term -> readSecKeys term btc anyKeys (KeysInFile file)
        -- The keys of a sweep to prepare, and of a sweep to sign that
        -- spends a coin at each of the addresses
        preparing term = readSecKeys term btc anyKeys KeysOnTerminal
        signing coins term = readSecKeys term btc (keysOfCoins addressesOf coins) KeysOnTerminal
        toPrepare lines' = answering lines' preparing
        toSign coins lines' = answering lines' (signing coins)
        onPipe = runExceptT . preparing
        firstPrompt = "Private keys (WIF, minikey or hex, separated by spaces): "
    it "are read from a file" $
      inScratchDirectory $ \dir -> do
        BS.writeFile (dir </> "keys") (encodeUtf8 wif <> "\n")
        fromFile (dir </> "keys") `shouldReturn` Right [wikiKey]

    -- The file is named each time, in the words of the other files hw
    -- reads, with no name of a function of the program
    it "are refused from a file that does not exist, which is named" $
      inScratchDirectory $ \dir ->
        fromFile (dir </> "missing") `shouldReturn` Left ("The file " <> dir </> "missing" <> " could not be read: no such file or directory")

    it "are refused from a file that is not UTF-8 text, which is named" $
      inScratchDirectory $ \dir -> do
        BS.writeFile (dir </> "keys") "\255\254"
        fromFile (dir </> "keys") `shouldReturn` Left ("The file " <> dir </> "keys" <> " could not be read: it is not UTF-8 text")

    it "are refused from a directory, which is named, with the reason" $
      inScratchDirectory $ \dir ->
        fromFile dir `shouldReturn` Left ("The file " <> dir <> " could not be read: is a directory")

    -- A pipe has an end: the keys are all of it, as a key file has them
    it "are read from every line of an input that has an end, after one prompt" $
      typingFor (encodeUtf8 (wif <> "\n" <> wifOne <> "\n")) onPipe
        `shouldReturn` (Right [wikiKey, keyOne], "Private keys (WIF, minikey or hex, separated by spaces): \n")

    it "are read from an input that has an end as from a file: a comment and an empty line are passed over" $
      typingFor (encodeUtf8 ("# The old wallet\n" <> wif <> "\n\n" <> wifOne <> " paper\n")) onPipe
        `shouldReturn` (Right [wikiKey, keyOne], "Private keys (WIF, minikey or hex, separated by spaces): \n")

    it "are refused from an input that has an end when a line has words but no key, named by its number" $
      (fst <$> typingFor (encodeUtf8 (wif <> "\nnonsense\n")) onPipe)
        `shouldReturn` Left "No private key on line 2"

    -- hw cannot tell when the keys of a sweep to prepare are complete: the
    -- keys say which coins it sweeps
    it "are typed on a terminal for a sweep to prepare until an empty line, the count told after each line" $
      toPrepare [wif, ""]
        `shouldReturn` (Right [wikiKey], [firstPrompt, "1 private key read. More private keys, or Enter to continue: "], [])

    it "are typed on as many lines as the user likes" $
      toPrepare [wif, wifOne, ""]
        `shouldReturn` ( Right [wikiKey, keyOne],
                         [ firstPrompt,
                           "1 private key read. More private keys, or Enter to continue: ",
                           "2 private keys read. More private keys, or Enter to continue: "
                         ],
                         []
                       )

    it "are typed several on a line, with spaces between them" $
      toPrepare [wif <> " " <> wifOne, ""]
        `shouldReturn` (Right [wikiKey, keyOne], [firstPrompt, "2 private keys read. More private keys, or Enter to continue: "], [])

    -- A pasted key file may start with a comment
    it "are asked for again after a line that is a comment" $
      toPrepare ["# The old wallet", wif, ""]
        `shouldReturn` (Right [wikiKey], [firstPrompt, firstPrompt, "1 private key read. More private keys, or Enter to continue: "], [])

    -- The sweep to sign tells which coins need a key
    it "are complete for a sweep to sign once every coin has its key: one Enter for one key" $
      toSign [wikiAddr] [wif] `shouldReturn` (Right [wikiKey], [firstPrompt], [])

    it "are asked for again while a coin of the sweep to sign has no key, the coins with a key counted" $
      toSign [wikiAddr, addrOne] [wif, wifOne]
        `shouldReturn` (Right [wikiKey, keyOne], [firstPrompt, "1 of the 2 coins has its key. More private keys, or Enter to stop: "], [])

    -- Two coins at the address of the first key, one at that of the second
    it "count every coin at the address of a key" $
      toSign [wikiAddr, wikiAddr, addrOne] [wif, wifOne]
        `shouldReturn` (Right [wikiKey, keyOne], [firstPrompt, "2 of the 3 coins have their key. More private keys, or Enter to stop: "], [])

    it "are asked for again after a key that is of no coin" $
      toSign [wikiAddr, addrOne] [hexTwo, wif <> " " <> wifOne]
        `shouldReturn` (Right [keyTwo, wikiKey, keyOne], [firstPrompt, "None of the 2 coins has its key yet. More private keys, or Enter to stop: "], [])

    it "are asked for again after a key that is not the one of the only coin" $
      toSign [wikiAddr] [wifOne, wif]
        `shouldReturn` (Right [keyOne, wikiKey], [firstPrompt, "The coin has no key yet. More private keys, or Enter to stop: "], [])

    -- signsweep then names the coin without a key
    it "are those typed so far when the user stops with an empty line before every coin has its key" $
      toSign [wikiAddr, addrOne] [wif, ""]
        `shouldReturn` (Right [wikiKey], [firstPrompt, "1 of the 2 coins has its key. More private keys, or Enter to stop: "], [])

    it "are refused when the first line is empty" $ do
      answerOf [""] preparing `shouldReturn` Left "No private keys were given"
      answerOf [""] (signing [wikiAddr]) `shouldReturn` Left "No private keys were given"

    -- The line is not shown: a key with one character wrong is the key
    it "are refused when a line typed has words but no key, named by its number" $
      answerOf [wif, "nonsense"] preparing `shouldReturn` Left "No private key on line 2"

    it "are cancelled when the input ends" $
      answerOf [] preparing `shouldReturn` Left cancelled

    -- A line pasted after the keys would answer the next question of hw,
    -- or run in the shell once hw has ended
    it "leave nothing typed ahead on the terminal: lines pasted after the keys are forgotten" $ do
      toPrepare [wif, "", "y"]
        `shouldReturn` (Right [wikiKey], [firstPrompt, "1 private key read. More private keys, or Enter to continue: "], [])
      toSign [wikiAddr] [wif, wifOne] `shouldReturn` (Right [wikiKey], [firstPrompt], [])

{- Questions -}

questionSpec :: Spec
questionSpec = do
  describe "A question to answer yes or no" $ do
    let asking term = askYes term ScriptLine "Send this transaction?"
    it "is answered yes by y or yes, in any case, with spaces around" $
      forM_ ["y", "yes", "Y", "YES", " yes "] $ \line ->
        answerOf [line] asking `shouldReturn` Right True

    it "is answered no by anything else, an empty line included" $
      forM_ ["", "n", "no", "yess", "send"] $ \line ->
        answerOf [line] asking `shouldReturn` Right False

    it "shows the question with the answers it takes, no being the one of an empty line" $ do
      (_, shown, _) <- answering ["y"] asking
      shown `shouldBe` ["Send this transaction? [y/N] "]

    it "is cancelled by the end of the input" $
      answerOf [] asking `shouldReturn` Left "Cancelled: the input ended before the answer"

    -- A script has no input left for the question: the error names what
    -- it lacks
    it "names the option that answers it in a script, when the input ends before the answer" $
      answerOf [] (\term -> askYes term (ScriptOption "--yes") "Send this transaction?")
        `shouldReturn` Left "Cancelled: the input ended before the answer. In a script, --yes answers this question."

    it "says that it needs a person at a terminal, and why, when no option answers it and the input ends" $
      answerOf [] (\term -> askYes term (NoScriptOption "to see what the store reports") "Keep looking?")
        `shouldReturn` Left "Cancelled: the input ended before the answer. No option answers this question: it needs a person at a terminal, to see what the store reports."

  describe "A question to pick one of a few choices" $ do
    -- Two addresses, each picked by its path or by its text
    let choices = [(["/0/3", "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1"], 3 :: Int), (["/0/5", "1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc"], 5)]
        asking term = askChoice term ScriptLine "Which address gets the change?" choices
    it "takes the choice an answer names, with spaces around" $ do
      answerOf ["/0/5"] asking `shouldReturn` Right 5
      answerOf [" /0/3 "] asking `shouldReturn` Right 3

    it "takes a choice by any of its names" $
      answerOf ["1MZuimSXigp8oqxkVUvZofqHNtVjdcdAqc"] asking `shouldReturn` Right 5

    -- A Bech32 address is the same address in capitals, and a script
    -- may write BIP84 for bip84
    it "takes the name of a choice in capitals or in small letters" $ do
      let types = [(["bip44"], 44 :: Int), (["bip84"], 84)]
      answerOf ["BIP84"] (\term -> askChoice term ScriptLine "Default type" types) `shouldReturn` Right 84
      answerOf ["Bip44"] (\term -> askChoice term ScriptLine "Default type" types) `shouldReturn` Right 44

    it "takes the first choice for an empty line" $
      answerOf [""] asking `shouldReturn` Right 3

    it "shows the question with the first choice, which an empty line takes" $ do
      (_, shown, _) <- answering ["/0/5"] asking
      shown `shouldBe` ["Which address gets the change? [/0/3] "]

    it "tells an answer that names no choice, and asks again" $ do
      (answer, shown, _) <- answering ["/0/4", "/0/5"] asking
      answer `shouldBe` Right 5
      shown
        `shouldBe` [ "Which address gets the change? [/0/3] ",
                     "Type one of /0/3, /0/5, or nothing for /0/3",
                     "Which address gets the change? [/0/3] "
                   ]

    it "fails after three answers that name no choice" $
      answerOf ["/0/4", "/0/6", "/0/7"] asking `shouldReturn` Left "Type one of /0/3, /0/5, or nothing for /0/3"

    it "is cancelled by the end of the input" $
      answerOf [] asking `shouldReturn` Left cancelled

  describe "A question whose answer is read as typed" $ do
    -- A reader that takes a number up to 10, as a fee rate question takes
    -- a number above the lowest rate
    let upToTen typed = case reads (Text.unpack typed) of
          [(n, "")] | n <= (10 :: Int) -> Right n
          _ -> Left "Type a number up to 10"
        asking term = askAnswer term ScriptLine "How many? " upToTen
    it "reads the answer without the spaces around it" $
      answerOf [" 7 "] asking `shouldReturn` Right 7

    it "tells the mistake the reader finds, and asks again" $ do
      (answer, shown, _) <- answering ["11", "7"] asking
      answer `shouldBe` Right 7
      shown `shouldBe` ["How many? ", "Type a number up to 10", "How many? "]

    it "fails after three answers the reader refuses" $
      answerOf ["11", "12", "13"] asking `shouldReturn` Left "Type a number up to 10"

    it "is cancelled by the end of the input" $
      answerOf [] asking `shouldReturn` Left cancelled

    it "names the option that answers it in a script, when the input ends before the answer" $
      answerOf [] (\term -> askAnswer term (ScriptOption "--fee-rate") "How many? " upToTen)
        `shouldReturn` Left "Cancelled: the input ended before the answer. In a script, --fee-rate answers this question."
