{-# LANGUAGE OverloadedStrings #-}

-- | What hw asks the user on the terminal: the mnemonic, whole or in parts,
-- and the passphrase of a seed, once to check them against an account or
-- the chain, twice to create an account; the signing key of an account,
-- asked until the mnemonic and the passphrase give it, three attempts at
-- most; and the private keys of a sweep, from a file or typed. A scripted
-- terminal types the lines of each test and keeps what hw shows; the real
-- one is tested on files.
module Haskoin.Wallet.PromptSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Haskoin
import Haskoin.Wallet.Keys
import Haskoin.Wallet.Prompt
import Haskoin.Wallet.TestUtils
import System.FilePath ((</>))
import qualified System.IO as IO
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  readingSpec
  mnemonicSpec
  newSeedSpec
  seedOnceSpec
  signingSpec ctx
  matchingSpec ctx
  terminalSpec
  diceSpec
  sweepKeysSpec

{- A scripted terminal -}

-- | Ask a question on a terminal that types the given lines, then ends:
-- the answer, what hw showed, prompts and messages in order, and the
-- lines left untyped
answering :: [Text] -> (Terminal -> ExceptT String IO a) -> IO (Either String a, [String], [Text])
answering lines' question = do
  left <- newIORef lines'
  shown <- newIORef []
  let term =
        Terminal
          { askHidden = \prompt -> do
              modifyIORef shown (<> [prompt])
              rest <- readIORef left
              case rest of
                [] -> return (Left cancelled)
                line : more -> writeIORef left more >> return (Right line),
            tell = \message -> modifyIORef shown (<> [message])
          }
  answer <- runExceptT (question term)
  (,,) answer <$> readIORef shown <*> readIORef left

-- | The answer alone
answerOf :: [Text] -> (Terminal -> ExceptT String IO a) -> IO (Either String a)
answerOf lines' question = (\(answer, _, _) -> answer) <$> answering lines' question

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

-- | Two parts that merge into the words of the test wallet: the mnemonic
-- of the entropy of ones, and that of the test entropy with every bit
-- flipped
firstPart, secondPart :: Text
firstPart = "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong"
secondPart = "crash detail high arena purse hand pepper start joke medal waste tennis"

-- | The prompts of hw
mnemonicPrompt, passphrasePrompt, againPrompt :: String
mnemonicPrompt = "Mnemonic: "
passphrasePrompt = "Passphrase, or nothing if the seed has none: "
againPrompt = "Passphrase again: "

-- | The public keys of account 0 of the test words with the passphrase
-- "café": as BIP39 derives it, from the passphrase normalized, and as hw
-- derived it before, from the passphrase as typed with a composed é
cafeAccountNormalized, cafeAccountTyped :: Ctx -> XPubKey
cafeAccountNormalized ctx = fromJust $ xPubImport btc ctx "xpub6D64kaV7GCsgoiy8dZB9botsFqRwDm9vHDvxPZPxbKZLEdkb4eV57Xt8GKfpcEq9rZKMN6eipk6Ubi3xFmMJUrT5ihGpsKEgcMh44XJGq4T"
cafeAccountTyped ctx = fromJust $ xPubImport btc ctx "xpub6CYaMuQZPjbAfUFbaTqJiNFqdLff7m8SPyEJW72VnP92yqEhMaxQJfMSrMHRzJtHbDyuymHTa4vBqopb4qHWHfV12AYK4mMXJAW5bzx3DPG"

-- | The public key of account 0 of the test wallet
testAccount :: Ctx -> XPubKey
testAccount ctx = fromJust $ xPubImport btc ctx testAccountXPub

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

{- Asking for a mnemonic -}

mnemonicSpec :: Spec
mnemonicSpec =
  describe "Asking for a mnemonic" $ do
    it "reads it whole, without echo" $
      answering [testWords] (`askMnemonic` 1) `shouldReturn` (Right testWords, [mnemonicPrompt], [])

    it "tells what is wrong with it and asks again" $
      answering ["snow senior", testWords] (`askMnemonic` 1)
        `shouldReturn` ( Right testWords,
                         [mnemonicPrompt, "A mnemonic has 12, 15, 18, 21 or 24 words, not 2", mnemonicPrompt],
                         []
                       )

    it "fails after three mistakes, asking no more" $
      answering ["one", "two", "three", testWords] (`askMnemonic` 1)
        `shouldReturn` ( Left "A mnemonic has 12, 15, 18, 21 or 24 words, not 1",
                         [ mnemonicPrompt,
                           "A mnemonic has 12, 15, 18, 21 or 24 words, not 1",
                           mnemonicPrompt,
                           "A mnemonic has 12, 15, 18, 21 or 24 words, not 1",
                           mnemonicPrompt
                         ],
                         [testWords]
                       )

    it "is cancelled when the input ends" $
      answerOf [] (`askMnemonic` 1) `shouldReturn` Left cancelled

    it "reads its parts and merges them" $
      answering [firstPart, secondPart] (`askMnemonic` 2)
        `shouldReturn` (Right testWords, ["Mnemonic part 1 of 2: ", "Mnemonic part 2 of 2: "], [])

    it "asks for the parts again when they do not merge" $
      answering [firstPart, firstPart, firstPart, secondPart] (`askMnemonic` 2)
        `shouldReturn` ( Right testWords,
                         [ "Mnemonic part 1 of 2: ",
                           "Mnemonic part 2 of 2: ",
                           "Two of the parts are the same, but every part of a mnemonic is different",
                           "Mnemonic part 1 of 2: ",
                           "Mnemonic part 2 of 2: "
                         ],
                         []
                       )

    -- Merging them used to stop hw instead
    it "asks for the parts again when they do not have the same number of words" $
      answering [firstPart, Text.unwords (replicate 23 "abandon" <> ["art"]), firstPart, secondPart] (`askMnemonic` 2)
        `shouldReturn` ( Right testWords,
                         [ "Mnemonic part 1 of 2: ",
                           "Mnemonic part 2 of 2: ",
                           "The parts do not have the same number of words",
                           "Mnemonic part 1 of 2: ",
                           "Mnemonic part 2 of 2: "
                         ],
                         []
                       )

{- A new seed, as createacc asks for it -}

newSeedSpec :: Spec
newSeedSpec =
  describe "Asking for the seed of a new account, as createacc does" $ do
    it "takes the mnemonic and the passphrase twice each" $
      answering [testWords, testWords, testPassphrase, testPassphrase] (`askNewMnemonicPass` 1)
        `shouldReturn` ( Right testMnemonic,
                         [mnemonicPrompt, "Type the mnemonic again, to check it", mnemonicPrompt, passphrasePrompt, againPrompt],
                         []
                       )

    it "takes the same words however they were typed the second time" $
      answerOf [testWords, Text.toUpper testWords, ""] (`askNewMnemonicPass` 1)
        `shouldReturn` Right (MnemonicPass testWords "")

    it "catches a typo the checksum lets through, and asks for the mnemonic again" $
      answering [testWords, typoWords, testWords, testWords, ""] (`askNewMnemonicPass` 1)
        `shouldReturn` ( Right (MnemonicPass testWords ""),
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
      answering [testWords, testWords, ""] (`askNewMnemonicPass` 1)
        `shouldReturn` ( Right (MnemonicPass testWords ""),
                         [mnemonicPrompt, "Type the mnemonic again, to check it", mnemonicPrompt, passphrasePrompt],
                         []
                       )

    it "asks for the passphrase twice again when the two differ" $
      answering [testWords, testWords, "correct horse", "correct hrose", testPassphrase, testPassphrase] (`askNewMnemonicPass` 1)
        `shouldReturn` ( Right testMnemonic,
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

    it "takes the parts of a mnemonic twice" $
      answerOf [firstPart, secondPart, firstPart, secondPart, ""] (`askNewMnemonicPass` 2)
        `shouldReturn` Right (MnemonicPass testWords "")

    it "is cancelled when the input ends" $
      answerOf [testWords, testWords, testPassphrase] (`askNewMnemonicPass` 1) `shouldReturn` Left cancelled

{- A seed typed once, as recover and testacc ask for it -}

seedOnceSpec :: Spec
seedOnceSpec =
  describe "Asking for a seed to check, as recover and testacc do" $
    it "takes the mnemonic and the passphrase once each" $
      answering [testWords, testPassphrase] (`askMnemonicPass` 1)
        `shouldReturn` (Right testMnemonic, [mnemonicPrompt, passphrasePrompt], [])

{- The signing key of an account -}

signingSpec :: Ctx -> Spec
signingSpec ctx =
  describe "Asking for the signing key of an account, as signtx and signmsg do" $ do
    let signingKeyOf lines' parts = answering lines' $ \term -> keyOf <$> askSigningKey term btc ctx parts 0 (testAccount ctx)
        keyOf (seed, key) = (fingerprintToText (seedFingerprint seed), xPrvExport btc key)
        signed = Right (testFingerprintText, testAccountXPrv)
        mismatch = "The mnemonic or the passphrase does not match the account"
    it "gives the key and the seed of the account from its mnemonic and passphrase" $
      signingKeyOf [testWords, testPassphrase] 1 `shouldReturn` (signed, [mnemonicPrompt, passphrasePrompt], [])

    it "asks for both again after a wrong passphrase" $
      signingKeyOf [testWords, "correct horse", testWords, testPassphrase] 1
        `shouldReturn` (signed, [mnemonicPrompt, passphrasePrompt, mismatch, mnemonicPrompt, passphrasePrompt], [])

    it "asks for both again after a wrong mnemonic, which no passphrase could fix" $
      signingKeyOf [otherWords, testPassphrase, testWords, testPassphrase] 1
        `shouldReturn` (signed, [mnemonicPrompt, passphrasePrompt, mismatch, mnemonicPrompt, passphrasePrompt], [])

    it "fails after three attempts that do not match, asking no more" $ do
      (answer, _, left) <- signingKeyOf [typoWords, testPassphrase, typoWords, testPassphrase, typoWords, testPassphrase, testWords] 1
      (answer, left) `shouldBe` (Left mismatch, [testWords])

    it "takes a mnemonic in parts" $
      signingKeyOf [firstPart, secondPart, testPassphrase] 2
        `shouldReturn` (signed, ["Mnemonic part 1 of 2: ", "Mnemonic part 2 of 2: ", passphrasePrompt], [])

    it "is cancelled when the input ends" $ do
      (answer, _, _) <- signingKeyOf [testWords] 1
      answer `shouldBe` Left cancelled

{- Matching an account -}

matchingSpec :: Ctx -> Spec
matchingSpec ctx =
  describe "Matching an account with a mnemonic and a passphrase" $ do
    -- Whether the account is found, and what the user is told
    let matching account passphrase = do
          (answer, shown, _) <- answering [] $ \term ->
            isJust <$> matchAccount term btc ctx 0 (account ctx) (MnemonicPass testWords passphrase)
          return (answer, shown)
    it "finds the account BIP39 derives from an accented passphrase, typed composed" $
      matching cafeAccountNormalized "caf\233" `shouldReturn` (Right True, [])

    it "finds it as well from the passphrase typed decomposed" $
      matching cafeAccountNormalized "cafe\769" `shouldReturn` (Right True, [])

    it "finds an account hw derived from the passphrase as typed, and warns that other wallets would not" $ do
      (answer, shown) <- matching cafeAccountTyped "caf\233"
      answer `shouldBe` Right True
      shown `shouldSatisfy` any ("other wallets would not find it" `isInfixOf`)

    it "finds nothing for a passphrase of another account" $
      matching cafeAccountNormalized "cafe" `shouldReturn` (Right False, [])

{- The real terminal -}

terminalSpec :: Spec
terminalSpec =
  describe "The terminal" $ do
    -- The input typed as bytes, read through a file handle; what hw shows
    -- written to another file
    let typing bytes question =
          inScratchDirectory $ \dir -> do
            BS.writeFile (dir </> "input") bytes
            _ <- IO.withBinaryFile (dir </> "input") IO.ReadMode $ \input ->
              IO.withFile (dir </> "output") IO.WriteMode $ \output ->
                question (terminalOn input output)
            readFile (dir </> "output")
        asked bytes = do
          answer <- newIORef (Left "not asked")
          shown <- typing bytes $ \term -> writeIORef answer =<< askHidden term "Secret: "
          (,) <$> readIORef answer <*> pure shown
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
    -- The key of the Bitcoin wiki's "Wallet import format" page
    let wif = "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617" :: Text
        wikiKey = fromJust $ secKey =<< decodeHex "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
        fromFile file = answerOf [] $ \term -> readSecKeys term btc (KeysInFile file)
    it "are read from a file" $
      inScratchDirectory $ \dir -> do
        BS.writeFile (dir </> "keys") (encodeUtf8 wif <> "\n")
        fromFile (dir </> "keys") `shouldReturn` Right [wikiKey]

    it "are refused from a file that does not exist" $
      inScratchDirectory $ \dir ->
        fromFile (dir </> "missing") `shouldReturn` Left "The private key file does not exist"

    it "are refused from a file that is not UTF-8 text" $
      inScratchDirectory $ \dir -> do
        BS.writeFile (dir </> "keys") "\255\254"
        fromFile (dir </> "keys") `shouldReturn` Left "The private key file is not UTF-8 text"

    it "are refused from a file that cannot be read, with the reason" $
      inScratchDirectory $ \dir -> do
        answer <- fromFile dir
        either ("The private key file could not be read: " `isPrefixOf`) (const False) answer `shouldBe` True

    it "are typed on the terminal" $
      answering [wif] (\term -> readSecKeys term btc KeysOnTerminal)
        `shouldReturn` (Right [wikiKey], ["Private keys (WIF, minikey or hex, separated by spaces): "], [])

    it "are refused when none is given" $
      answerOf [""] (\term -> readSecKeys term btc KeysOnTerminal) `shouldReturn` Left "No private keys were given"

    it "are cancelled when the input ends" $
      answerOf [] (\term -> readSecKeys term btc KeysOnTerminal) `shouldReturn` Left cancelled
