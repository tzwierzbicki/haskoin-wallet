{-# LANGUAGE OverloadedStrings #-}

-- | The sheet to write a mnemonic on by hand, or the shares of a backup:
-- which sheet the options of newportfolio give, what each page says, how
-- many lines it has and where, and the file hw keeps it in for a wallet.
module Haskoin.Wallet.Report.MnemonicSheetSpec (spec) where

import Data.Char (isDigit)
import Data.List ((\\))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (fromGregorian)
import Haskoin.Wallet.Report.MnemonicSheet
import Haskoin.Wallet.Report.Pdf (Font (..), Mark (..))
import Haskoin.Wallet.Report.TestUtils (marksOf, textsOf)
import Haskoin.Wallet.TestUtils (testFingerprint, testFingerprint2)
import Test.Hspec

-- A page starts with the three texts of its title block: the kind of
-- secret in capitals, the title, and the line below it. The name of the
-- logo follows.
--
-- The lines to write on are rules 0.6 points thick. A page has four more
-- of them than it has words: the name of the wallet, the date and the
-- fingerprint, and none for the passphrase, which has two boxes.
--
-- The numbers of the words are the only texts in bold that are digits
-- alone.

-- | The first page of a sheet as it is drawn, with nothing known of its
-- wallet: the one page of a mnemonic, or the page of the first share
firstPage :: SheetKind -> [Mark]
firstPage = concatMap (pageMarks Nothing) . take 1 . sheetPages

-- | The numbers of the words of a page, in the order they are drawn
wordNumbers :: [Mark] -> [Text]
wordNumbers page = [text | TextMark _ _ Bold _ _ _ text <- page, Text.all isDigit text]

-- | How far down the page each rule to write on is, in hundredths of a
-- point, for the rules that start at the place given
rulesFrom :: Integer -> [Mark] -> [Integer]
rulesFrom left page = [round (y * 100) | RuleMark from _ y 0.6 _ <- page, round (from * 100) == left]

spec :: Spec
spec = do
  describe "The sheet for the options of newportfolio" $ do
    it "is for a mnemonic of 12 words without any option" $
      sheetKindOf Nothing Nothing Nothing `shouldBe` Right (MnemonicSheet 12)

    -- BIP39 has three words for every four bytes of entropy
    it "is for a mnemonic of 15, 18, 21 or 24 words with an --entropy of 20, 24, 28 or 32 bytes" $
      [sheetKindOf (Just bytes) Nothing Nothing | bytes <- [20, 24, 28, 32]]
        `shouldBe` [Right (MnemonicSheet 15), Right (MnemonicSheet 18), Right (MnemonicSheet 21), Right (MnemonicSheet 24)]

    it "is for shares of 20 words with --shares, at the threshold of --threshold" $
      sheetKindOf Nothing (Just 3) (Just 2) `shouldBe` Right (SharesSheet 2 3 20)

    it "is for shares of 33 words with an --entropy of 32 bytes" $
      sheetKindOf (Just 32) (Just 5) (Just 3) `shouldBe` Right (SharesSheet 3 5 33)

    it "is for one share, the whole backup, with --shares 1 and no threshold" $
      sheetKindOf Nothing (Just 1) Nothing `shouldBe` Right (SharesSheet 1 1 20)

    it "is refused a --threshold without --shares, as newportfolio refuses it" $
      sheetKindOf Nothing Nothing (Just 2) `shouldBe` Left "--threshold goes with --shares: it is the number of shares that rebuild the seed"

    it "is refused several shares without a threshold, as newportfolio refuses them" $
      sheetKindOf Nothing (Just 3) Nothing `shouldBe` Left "With more than one share, give --threshold: the number of shares that rebuild the seed"

    it "is refused shares of an entropy that a backup in shares cannot have" $
      sheetKindOf (Just 20) (Just 3) (Just 2) `shouldBe` Left "A backup in shares has 16 or 32 bytes of entropy, not 20"

    it "is refused an entropy that no mnemonic has" $
      sheetKindOf (Just 17) Nothing Nothing `shouldBe` Left "A mnemonic has 16, 20, 24, 28 or 32 bytes of entropy, not 17"

  describe "The options of mnemonicsheet that give a sheet" $ do
    it "are none for a mnemonic of 12 words, the default" $
      sheetOptions (MnemonicSheet 12) `shouldBe` []

    -- 24 words are 32 bytes of entropy
    it "are the entropy alone for a longer mnemonic" $
      sheetOptions (MnemonicSheet 24) `shouldBe` ["--entropy", "32"]

    it "are the shares and their threshold for a backup in shares" $
      sheetOptions (SharesSheet 2 3 20) `shouldBe` ["--shares", "3", "--threshold", "2"]

    -- Shares of 33 words are of 32 bytes of entropy
    it "leave the threshold out for a single share" $
      sheetOptions (SharesSheet 1 1 33) `shouldBe` ["--entropy", "32", "--shares", "1"]

  describe "The sheet for a mnemonic" $ do
    it "is one page" $
      length (sheetPages (MnemonicSheet 24)) `shouldBe` 1

    it "starts with the kind of mnemonic, the title, and how many words restore a wallet" $
      take 3 (textsOf (firstPage (MnemonicSheet 24)))
        `shouldBe` [ "BIP39 MNEMONIC \183 24 WORDS",
                     "Mnemonic Backup Sheet",
                     "The twenty-four words that restore a wallet, written by hand"
                   ]

    it "has a number for each of its 12 words, from 1 to 12" $
      wordNumbers (firstPage (MnemonicSheet 12)) `shouldBe` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]

    -- 12 lines for the words, and 3 for the fields
    it "has a line to write each word on" $
      length [() | RuleMark _ _ _ 0.6 _ <- firstPage (MnemonicSheet 12)] `shouldBe` 15

    -- 15 words are 8 lines in the first column and 7 in the second
    it "puts the words down the first column, then down the second from its top" $ do
      let page = firstPage (MnemonicSheet 15)
          heightOf number = textBaseline <$> marksOf number page
          leftOf number = textX <$> marksOf number page
      heightOf "9" `shouldBe` heightOf "1"
      zipWith (>) (heightOf "8") (heightOf "7") `shouldBe` [True]
      zipWith (>) (leftOf "9") (leftOf "8") `shouldBe` [True]

    it "says how to keep it, and names the list of BIP39 and the command that checks it" $
      map pageNotes (sheetPages (MnemonicSheet 12))
        `shouldBe` [ [ "Whoever reads these words can spend the funds of the wallet. Keep this sheet where only you can reach it.",
                       "Keep a second copy in another safe place, in case this one is lost or destroyed.",
                       "Never type the words into a website or a phone, and never photograph this sheet.",
                       "Write with a pen, in block capitals. Then read the words back against the screen, in order.",
                       "Every word is in the BIP39 list of 2048 English words. Its first four letters tell it from all the others.",
                       "A passphrase is part of the secret: without it, these words give another wallet. Keep it apart from this sheet.",
                       "Before you send funds to the wallet, check this sheet with hw checkmnemonic, which asks for the words."
                     ]
                   ]

  describe "The sheet for a backup in shares" $ do
    it "has a page for each share, titled with its number among the shares" $
      map pageTitle (sheetPages (SharesSheet 2 3 20))
        `shouldBe` ["Backup Share 1 of 3", "Backup Share 2 of 3", "Backup Share 3 of 3"]

    it "says on each page how many words a share has, and how many shares restore the wallet" $
      map (\page -> (pageKind page, pageSubtitle page, pageWordCount page)) (sheetPages (SharesSheet 2 3 20))
        `shouldBe` replicate 3 ("SLIP-39 share \183 20 words", "Twenty words, written by hand. Any 2 of the 3 shares restore the wallet.", 20)

    it "says how many shares can be lost, for the holder of one share who knows nothing else" $
      take 2 (concatMap pageNotes (take 1 (sheetPages (SharesSheet 3 5 20))))
        `shouldBe` [ "Any 3 of the 5 shares restore the wallet, so whoever reads 3 of them can spend its funds. Fewer reveal nothing about it.",
                     "Keep each share in a different safe place. If up to 2 shares are lost, the others still restore the wallet. If more are lost, so are the funds."
                   ]

    it "says that one share can be lost when all but one restore the wallet" $
      take 1 (drop 1 (concatMap pageNotes (take 1 (sheetPages (SharesSheet 2 3 20)))))
        `shouldBe` ["Keep each share in a different safe place. If 1 share is lost, the others still restore the wallet. If more are lost, so are the funds."]

    it "says that every share is needed when the threshold is the number of shares" $
      [(pageSubtitle page, take 2 (pageNotes page)) | page <- take 1 (sheetPages (SharesSheet 2 2 33))]
        `shouldBe` [ ( "Thirty-three words, written by hand. The 2 shares together restore the wallet.",
                       [ "The 2 shares together restore the wallet, so whoever reads all of them can spend its funds. Fewer reveal nothing about it.",
                         "Keep each share in a different safe place. Every share is needed: if one is lost, so are the funds."
                       ]
                     )
                   ]

    it "names the list of SLIP-39, whose words are not those of BIP39" $
      take 1 (drop 4 (concatMap pageNotes (take 1 (sheetPages (SharesSheet 2 3 20)))))
        `shouldBe` ["Every word is in the SLIP-39 list of 1024 English words. Its first four letters tell it from all the others."]

    -- A single share is the whole backup, as a mnemonic is
    it "reads as the sheet of a whole secret for a single share" $
      [(pageTitle page, pageSubtitle page, take 2 (pageNotes page)) | page <- sheetPages (SharesSheet 1 1 20)]
        `shouldBe` [ ( "Backup Share 1 of 1",
                       "The twenty words that restore a wallet, written by hand",
                       [ "Whoever reads these words can spend the funds of the wallet. Keep this sheet where only you can reach it.",
                         "Keep a second copy in another safe place, in case this one is lost or destroyed."
                       ]
                     )
                   ]

    -- The longest secret hw makes: 17 lines in the first column, which
    -- starts 36 points right of the left margin of 56.7 points. The lines
    -- of a ruled notebook are 6 to 8 millimetres apart, and 6 millimetres
    -- are 17 points.
    it "keeps the 33 lines of a long share on one page, at least 6 millimetres apart" $ do
      let page = firstPage (SharesSheet 2 2 33)
          lines33 = rulesFrom 9270 page
      length lines33 `shouldBe` 17
      minimum (zipWith (-) (drop 1 lines33) lines33) `shouldSatisfy` (>= 1700)
      wordNumbers page `shouldSatisfy` \numbers -> length numbers == 33 && last numbers == "33"

    -- The heading of the notes stands on a rule 0.8 points thick, the
    -- second of the page after that of the heading of the words
    it "keeps every line for a word above the notes" $ do
      let page = firstPage (SharesSheet 2 2 33)
          notesRule = maximum [y | RuleMark _ _ y 0.8 _ <- page]
      maximum [y | RuleMark _ _ y 0.6 _ <- page] `shouldSatisfy` (< notesRule - 20)

  describe "The fields of a sheet" $ do
    let wallet = SheetWallet testFingerprint (fromGregorian 2026 11 25)
        pageFor walletM = concatMap (pageMarks walletM) (sheetPages (MnemonicSheet 12))

    it "are left to the pen when hw knows nothing of the wallet" $
      take 6 (drop 4 (textsOf (pageFor Nothing)))
        `shouldBe` ["Wallet", "Date", "Fingerprint", "Passphrase", "None", "Yes, kept elsewhere"]

    -- The fingerprint of the test wallet is bc560f95
    it "have the day and the fingerprint of the wallet printed when hw knows them, and nothing else" $
      (textsOf (pageFor (Just wallet)) \\ textsOf (pageFor Nothing))
        `shouldBe` ["25 November 2026", "bc560f95"]

  describe "The file of the sheet hw keeps for a wallet" $ do
    it "is in mnemonic-sheets next to the database, named after the wallet and its fingerprint" $
      walletSheetFile "/home/user/.hw/portfolios.sqlite" 0 testFingerprint
        `shouldBe` "/home/user/.hw/mnemonic-sheets/wallet0-bc560f95-mnemonic-sheet.pdf"

    it "is another file for another wallet at the same index" $
      walletSheetFile "/home/user/.hw/portfolios.sqlite" 0 testFingerprint2
        `shouldBe` "/home/user/.hw/mnemonic-sheets/wallet0-4435aa1c-mnemonic-sheet.pdf"
