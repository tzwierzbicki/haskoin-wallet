{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A database file as show reads it: how it is told from the other
-- files, which wallets and portfolios it gives, which databases it
-- refuses, and that it leaves the file as it was. Each test writes a
-- database in a scratch directory, with the wallets of the two test
-- mnemonics: bc560f95 is the fingerprint of the first, 4435aa1c of the
-- second. What show prints of the file is in
-- "Haskoin.Wallet.Cli.PrettyPrinterSpec", and the command itself in
-- "Haskoin.Wallet.Workflow.ShowSpec".
module Haskoin.Wallet.DatabaseFileSpec (spec) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import qualified Database.Persist as P
import Database.Persist.Sql (rawExecute)
import Haskoin (Ctx, fingerprintToText, prepareContext)
import Haskoin.Wallet.Backup (copyDatabase)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (Portfolio (..), setActivePortfolio, setPortfolioDescription, setWalletDescription)
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), PortfolioShown (..), WalletShown (..), databaseHeaderLength, databaseShown, startsDatabase)
import Haskoin.Wallet.FileIO (readFileStart)
import Haskoin.Wallet.Migration.TestUtils (version1Database, version2Database, version6Database)
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import System.Directory (createDirectory, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  describe "A file is taken for a database" $ do
    -- The 16 bytes of the header, then the size of a page as SQLite
    -- writes it
    it "when it starts with the words SQLite format 3 and a zero byte" $
      startsDatabase "SQLite format 3\NUL\DLE\NUL" `shouldBe` True

    it "not when it starts as a file of a wallet does, with a record" $
      startsDatabase "{\"type\":\"xpub\"" `shouldBe` False

    -- A text may start with the same words, but not with the zero byte
    it "not when it is a text that starts with the same words" $
      startsDatabase "SQLite format 3 is the format of this file\n" `shouldBe` False

    it "not when it is empty" $
      startsDatabase "" `shouldBe` False

    it "when it is a database hw wrote, read by its first 16 bytes" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "portfolios.sqlite"
        versionedDatabase file (return ())
        databaseHeaderLength `shouldBe` 16
        (fmap startsDatabase <$> readFileStart 16 file) `shouldReturn` Right True

  describe "The wallets of a database file" $ do
    it "are none in a database without a wallet, which gives its format version alone" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file (return ())
        databaseShown file `shouldReturn` Right (DatabaseShown 5 [])

    -- The wallet of the first test mnemonic is created first
    it "come in the order of their indexes, each with the fingerprint of its seed" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          _ <- portfolioOf ctx testMnemonic
          portfolioOf ctx testMnemonic2
        (fmap fingerprints <$> databaseShown file) `shouldReturn` Right [(0, "bc560f95"), (1, "4435aa1c")]

    it "come with the description the user gave each, which is empty for a wallet without one" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          first <- portfolioOf ctx testMnemonic
          _ <- portfolioOf ctx testMnemonic2
          setWalletDescription first.portfolioWalletId "Family"
        (fmap (map walletShownDescription . databaseShownWallets) <$> databaseShown file)
          `shouldReturn` Right ["Family", ""]

  describe "The portfolios of a wallet of a database file" $ do
    it "come in the order of their indexes" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          _ <- nextPortfolioOf ctx testMnemonic
          nextPortfolioOf ctx testMnemonic
        (fmap (map (map portfolioShownIndex) . portfoliosShown) <$> databaseShown file)
          `shouldReturn` Right [[0, 1]]

    -- The first portfolio created is the active one, until a switch
    it "mark the first one created as the active one, and no other" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          _ <- nextPortfolioOf ctx testMnemonic
          nextPortfolioOf ctx testMnemonic
        (fmap (map (map portfolioShownActive) . portfoliosShown) <$> databaseShown file)
          `shouldReturn` Right [[True, False]]

    it "mark the one a switch made active, and no other" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          _ <- nextPortfolioOf ctx testMnemonic
          second <- nextPortfolioOf ctx testMnemonic
          lift $ setActivePortfolio second.portfolioId
        (fmap (map (map portfolioShownActive) . portfoliosShown) <$> databaseShown file)
          `shouldReturn` Right [[False, True]]

    -- In a database of two wallets, one portfolio is active in all
    it "are all not active in a wallet when the active one is of another wallet" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          _ <- portfolioOf ctx testMnemonic
          portfolioOf ctx testMnemonic2
        (fmap (map (map portfolioShownActive) . portfoliosShown) <$> databaseShown file)
          `shouldReturn` Right [[True], [False]]

    it "come with the description the user gave each, which is empty for a portfolio without one" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          first <- nextPortfolioOf ctx testMnemonic
          _ <- nextPortfolioOf ctx testMnemonic
          setPortfolioDescription first.portfolioId "Savings"
        (fmap (map (map portfolioShownDescription) . portfoliosShown) <$> databaseShown file)
          `shouldReturn` Right [["Savings", ""]]

    -- The row is given the time of the fixtures, 1 Nov 2023 at midnight
    -- UTC, in place of the time of the test
    it "come with the time each was created" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "copy.sqlite"
        versionedDatabase file $ do
          portfolio <- portfolioOf ctx testMnemonic
          lift $ P.update portfolio.portfolioId [DBPortfolioCreated P.=. fixtureTime]
        (fmap (map (map portfolioShownCreated) . portfoliosShown) <$> databaseShown file)
          `shouldReturn` Right [[UTCTime (fromGregorian 2023 11 1) 0]]

  describe "A database file show does not read" $ do
    it "is one of hw 0.9, at format version 2, refused with its version" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "old.sqlite"
        inDatabaseFile file version2Database
        databaseShown file
          `shouldReturn` Left ("The SQLite database " <> file <> " cannot be shown: it is a database of hw at format version 2, made by an earlier hw, and show reads version 5 only")

    it "is one of a newer hw, at format version 6, refused with its version" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "new.sqlite"
        inDatabaseFile file version6Database
        databaseShown file
          `shouldReturn` Left ("The SQLite database " <> file <> " cannot be shown: it is a database of hw at format version 6, made by a newer hw, and show reads version 5 only")

    -- A database of another program has no table of the version either
    it "is one that gives no format version, as a database of hw 0.8 or of another program" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "other.sqlite"
        inDatabaseFile file version1Database
        databaseShown file
          `shouldReturn` Left ("The SQLite database " <> file <> " cannot be shown: it does not give a format version, so it is not a database of hw, or it is one of hw 0.8")

    it "is one without any table" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "empty.sqlite"
        inDatabaseFile file $ do
          rawExecute "create table notes (line text)" []
          rawExecute "drop table notes" []
        databaseShown file
          `shouldReturn` Left ("The SQLite database " <> file <> " cannot be shown: it has no tables, so it is not a database of hw")

    -- A copy that was cut short after its first bytes, as by a full disk
    it "is one SQLite cannot read, refused in the words of SQLite" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "cut.sqlite"
        BS.writeFile file "SQLite format 3\NUL"
        databaseShown file
          `shouldReturn` Left ("The SQLite database " <> file <> " cannot be shown: SQLite could not read it (file is not a database)")

  describe "A database file that was shown" $ do
    -- The copy is made as backup makes it
    it "holds the same bytes as before" $
      inScratchDirectory $ \dir -> do
        let copy = dir </> "copies" </> "copy.sqlite"
        copyOfTestWallet ctx dir
        bytesBefore <- BS.readFile copy
        _ <- databaseShown copy
        BS.readFile copy `shouldReturn` bytesBefore

    it "has no other file beside it, when it is a copy hw made" $
      inScratchDirectory $ \dir -> do
        copyOfTestWallet ctx dir
        _ <- databaseShown (dir </> "copies" </> "copy.sqlite")
        listDirectory (dir </> "copies") `shouldReturn` ["copy.sqlite"]

    -- hw keeps its database in the write-ahead log mode, where SQLite
    -- needs two more files to read it. They go once it is read.
    it "has no other file beside it, when it is the database of hw copied by hand" $
      inScratchDirectory $ \dir -> do
        let file = dir </> "portfolios.sqlite"
        versionedDatabase file (portfolioOf ctx testMnemonic)
        _ <- databaseShown file
        listDirectory dir `shouldReturn` ["portfolios.sqlite"]

-- | A database of hw at format version 5 in the file given, with the
-- tables of hw, prepared by the action
versionedDatabase :: FilePath -> ExceptT String (DB IO) a -> IO ()
versionedDatabase file prepare = do
  preparedDatabase file (lift (P.insert_ (DBVersion 5)) >> prepare)

-- | A database with portfolio 0 of the test wallet in the directory
-- given, and its copy, as backup makes it, alone in the directory copies
-- under it: copies/copy.sqlite
copyOfTestWallet :: Ctx -> FilePath -> IO ()
copyOfTestWallet ctx dir = do
  let database = dir </> "portfolios.sqlite"
  versionedDatabase database (portfolioOf ctx testMnemonic)
  createDirectory (dir </> "copies")
  either fail return =<< copyDatabase database (dir </> "copies" </> "copy.sqlite")

-- | The wallets show read, each by its index and the fingerprint of its
-- seed as text
fingerprints :: DatabaseShown -> [(Natural, Text)]
fingerprints shown = [(walletShownIndex wallet, fingerprintToText (walletShownFingerprint wallet)) | wallet <- databaseShownWallets shown]

-- | The portfolios show read, those of each wallet in a list
portfoliosShown :: DatabaseShown -> [[PortfolioShown]]
portfoliosShown = map walletShownPortfolios . databaseShownWallets
