{-# LANGUAGE OverloadedStrings #-}

-- | The commands and the active portfolio, which the database keeps:
-- switch, which changes it, exportwallet, which writes every wallet
-- whichever portfolio is active, the commands that work in it, coin
-- control, which spends its coins alone, and the deletions, which refuse
-- it. Then the copies of the database and of the labels the commands that
-- change them leave next to the database, the backup hw makes before a
-- deletion, restoredatabase, which puts a copy back, and showbackups.
-- Each test has a database of its own in a file,
-- and runs the command as hw runs it, from the words typed, without a
-- terminal and without a store. The commands that ask the user or the store, newportfolio,
-- recover and sync among them, run as the binary in
-- "Haskoin.Wallet.Workflow".
module Haskoin.Wallet.CommandsSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import Data.Maybe (listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Sql (PersistValue (..), rawExecute)
import Haskoin
import Haskoin.Wallet.Backup (BackupsShown (..), CopiesShown (..))
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Commands (commandResponse)
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database (DB, DBAddress (..))
import Haskoin.Wallet.Database.Account (PortfolioView (..))
import Haskoin.Wallet.Database.Address (genExtAddress)
import Haskoin.Wallet.Database.Coin (refreshCoins)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), WalletShown (..))
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.Migration.TestUtils (version6Database)
import Haskoin.Wallet.TestUtils
import System.Directory (createDirectory, doesDirectoryExist, listDirectory)
import Numeric.Natural (Natural)
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))
import Test.Hspec

spec :: Spec
spec = do
  switchSpec
  exportingSpec
  workingSpec
  coinControlSpec
  deletingSpec
  databaseCopiesSpec
  labelCopiesSpec
  backupBeforeSpec
  restoreSpec
  showBackupsSpec

{- Fixtures -}

-- | The answer of hw to the words typed after hw, in the database of the
-- configuration. Words the command line refuses fail the test.
hwAnswer :: Config -> [String] -> IO Response
hwAnswer wallet typed = either fail (commandResponse wallet UnitBitcoin) (parsedLine typed)

-- | A wallet in the scratch directory, its data directory, with what the
-- action creates
inWallet :: FilePath -> ExceptT String (DB IO) a -> IO Config
inWallet = walletInDirectory

-- | Portfolios 0 and 1 of the test wallet, wallet0. Portfolio 0 is the
-- active one, as the first created.
twoPortfolios :: ExceptT String (DB IO) ()
twoPortfolios = testPortfolio >> void (nextPortfolioOf testMnemonic)

-- | Portfolio 0 of the test wallet, wallet0, the active one as the first
-- created, and portfolio 0 of the second test wallet, wallet1
twoWallets :: ExceptT String (DB IO) ()
twoWallets = testPortfolio >> void (portfolioOf testMnemonic2)

-- | Each portfolio portfolios lists, with whether it is the active one
marksOf :: Response -> Either String [(PortfolioRef, Bool)]
marksOf (ResponsePortfolios views) = Right [(viewRef view, viewActive view) | view <- views]
marksOf other = Left (show other)

-- | The portfolio an answer shows, with whether it is the active one
shownOf :: Response -> Either String (PortfolioRef, Bool)
shownOf (ResponseSwitched view) = Right (viewRef view, viewActive view)
shownOf (ResponsePortfolio view) = Right (viewRef view, viewActive view)
shownOf other = Left (show other)

-- | A coin of 50000 at the first receiving address of portfolio 1 of the
-- test wallet on btc, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo: the output 0 of
-- fakeTxHash 1, which coin control names by its txid in full
coinOfPortfolio1 :: ExceptT String (DB IO) ()
coinOfPortfolio1 = do
  _ <- testPortfolio
  second <- nextPortfolioOf testMnemonic
  (accId, _) <- accountOf second btc Bip44
  _ <- genExtAddress def accId ""
  void $ refreshCoins btc accId (const seenTime) [account1Addr] [storeCoin (fakeTxHash 1, 0) (Just 800000) account1Addr 50000]

-- | The coin of 'coinOfPortfolio1' as the command line takes it
coinWord :: String
coinWord = "0000000000000000000000000000000000000000000000000000000000000001:0"

-- | A payment of 0.0001 btc to an address of another wallet, at a fee
-- rate that needs no question
paymentWords :: [String]
paymentWords = ["preparetx", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0001", "--fee-rate", "10", "--yes"]

{- switch -}

switchSpec :: Spec
switchSpec =
  describe "The command switch" $ do
    it "answers with the portfolio it made the active one, marked as active" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        shownOf <$> hwAnswer wallet ["switch", "--portfolio", "1"] `shouldReturn` Right (PortfolioRef 0 1, True)

    -- The database keeps it: portfolios runs in a transaction of its own
    it "leaves the portfolio it switched to active for the next command, which portfolios marks" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        marksOf <$> hwAnswer wallet ["portfolios"] `shouldReturn` Right [(PortfolioRef 0 0, False), (PortfolioRef 0 1, True)]

    it "refuses a portfolio the wallet does not have, naming where the portfolios are listed" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "1"]
          `shouldReturn` ResponseError "wallet1 has no portfolio1: portfolios lists the portfolios"

{- exportwallet -}

-- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
-- test mnemonic, 4435aa1c. Each test makes portfolio0 of wallet1 the
-- active one first. The fingerprints in the file tell which wallets were
-- written.
exportingSpec :: Spec
exportingSpec =
  describe "The command exportwallet and the active portfolio" $ do
    it "writes every wallet, whichever portfolio is active" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        let file = dir </> "wallet.jsonl"
        hwAnswer wallet ["exportwallet", file] `shouldReturn` ResponseFile file
        written <- readFile file
        ("bc560f95" `isInfixOf` written, "4435aa1c" `isInfixOf` written) `shouldBe` (True, True)

    it "writes the one wallet --wallet names, which need not be the wallet of the active portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        let file = dir </> "wallet.jsonl"
        hwAnswer wallet ["exportwallet", file, "--wallet", "0"] `shouldReturn` ResponseFile file
        written <- readFile file
        ("bc560f95" `isInfixOf` written, "4435aa1c" `isInfixOf` written) `shouldBe` (True, False)

    -- Both wallets have a portfolio0
    it "writes with --portfolio alone a portfolio of the wallet of the active portfolio, as switch finds one" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        let file = dir </> "wallet.jsonl"
        hwAnswer wallet ["exportwallet", file, "--portfolio", "0"] `shouldReturn` ResponseFile file
        written <- readFile file
        ("bc560f95" `isInfixOf` written, "4435aa1c" `isInfixOf` written) `shouldBe` (False, True)

{- The commands that work in the active portfolio -}

workingSpec :: Spec
workingSpec =
  describe "The commands that work in the active portfolio" $ do
    it "list it marked as the active one among the portfolios: the first one created" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        marksOf <$> hwAnswer wallet ["portfolios"] `shouldReturn` Right [(PortfolioRef 0 0, True), (PortfolioRef 0 1, False)]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is /0/0 of m/44'/0'/1'
    it "give out an address of the active portfolio with receive" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        given <- hwAnswer wallet ["receive"]
        case given of
          ResponseAddress ref addr _ _ _ _ -> (ref, dBAddressAddress addr) `shouldBe` (AccountRef (Place (PortfolioRef 0 1) btc) Bip44, "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo")
          other -> expectationFailure ("Not an address: " <> show other)

    it "describe the active portfolio with label, given the description alone" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        described <- hwAnswer wallet ["label", "savings"]
        case described of
          ResponsePortfolio view -> (viewRef view, viewDescription view) `shouldBe` (PortfolioRef 0 1, "savings")
          other -> expectationFailure ("Not a portfolio: " <> show other)

    it "describe the wallet of the active portfolio with label --of-wallet, given the description alone" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        described <- hwAnswer wallet ["label", "laptop", "--of-wallet"]
        case described of
          ResponsePortfolio view -> (viewRef view, viewWalletDescription view) `shouldBe` (PortfolioRef 1 0, "laptop")
          other -> expectationFailure ("Not a portfolio: " <> show other)

    it "refuse show --portfolio with anything but a file of a wallet, which is where it names a portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        hwAnswer wallet ["show", "--portfolio", "1"]
          `shouldReturn` ResponseError "show takes --wallet and --portfolio only with a file of a wallet, where they name one wallet and one portfolio of the file. The rest is shown in the active portfolio, or where it is"

{- Coin control -}

-- Portfolio 0 is the active one, and the coin and its address are of
-- portfolio 1
coinControlSpec :: Spec
coinControlSpec =
  describe "Coin control" $ do
    it "refuses a coin of another portfolio than the active one, naming the portfolio that holds it and the switch to it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir coinOfPortfolio1
        hwAnswer wallet (paymentWords <> ["--coin", coinWord])
          `shouldReturn` ResponseError "The coin 00000000:0 is in portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet 0 first"

    it "refuses an address of --spend-from of another portfolio than the active one, naming the portfolio that holds it and the switch to it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir coinOfPortfolio1
        hwAnswer wallet (paymentWords <> ["--spend-from", "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo"])
          `shouldReturn` ResponseError "The address 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is in portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet 0 first"

    -- The payment is on the network of its coins, which --network may not
    -- contradict
    it "refuses a --network that holds none of the coins given, naming the network they are on" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir coinOfPortfolio1
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        hwAnswer wallet (paymentWords <> ["--coin", coinWord, "--network", "bch"])
          `shouldReturn` ResponseError "The coins, addresses or tags given are on btc, not on the --network bch"

{- Deleting -}

deletingSpec :: Spec
deletingSpec = do
  describe "The command deleteportfolio" $ do
    it "deletes a portfolio of the wallet of the active portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        (deletedOf <$> hwAnswer wallet ["deleteportfolio", "1", "--yes"]) `shouldReturn` Right "portfolio1 of wallet0 has been deleted"

    it "refuses the active portfolio, and says to switch to another one first" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        hwAnswer wallet ["deleteportfolio", "1", "--yes"]
          `shouldReturn` ResponseError "portfolio1 of wallet0 is the active portfolio: switch to another portfolio first"

    -- wallet1 has portfolios 0 and 1, and wallet0, of the active
    -- portfolio, has portfolio 0 alone
    it "looks for the portfolio in the wallet of the active portfolio alone" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets >> void (nextPortfolioOf testMnemonic2))
        hwAnswer wallet ["deleteportfolio", "1", "--yes"]
          `shouldReturn` ResponseError "wallet0 has no portfolio1: portfolios lists the portfolios"

  -- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
  -- one, 4435aa1c
  describe "The command deletewallet and the active portfolio" $ do
    it "refuses the wallet of the active portfolio while another wallet exists, and says to switch to a portfolio of another wallet first" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        hwAnswer wallet ["deletewallet", "0", "--yes"]
          `shouldReturn` ResponseError "wallet0 holds the active portfolio, portfolio0 of wallet0: switch to a portfolio of another wallet first"

    it "deletes the wallet that held the active portfolio once the active portfolio is of another wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        (deletedOf <$> hwAnswer wallet ["deletewallet", "0", "--yes"])
          `shouldReturn` Right "wallet0 has been deleted, with all of its portfolios"

    it "deletes the last wallet, the one of the active portfolio, and says that the first portfolio created next becomes the active one" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        (deletedOf <$> hwAnswer wallet ["deletewallet", "0", "--yes"])
          `shouldReturn` Right "wallet0 has been deleted, with all of its portfolios. The database has no wallet left: the first portfolio created next becomes the active one."

    it "leaves no active portfolio once the last wallet is deleted: switch is refused, with how to make a wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["deletewallet", "0", "--yes"]
        hwAnswer wallet ["switch", "--portfolio", "0"]
          `shouldReturn` ResponseError "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"

{- The copies of the database -}

-- The database of each test is portfolios.sqlite in its scratch directory,
-- so its copies go into the auto-backups directory there. wallet0 is of
-- the test mnemonic, bc560f95, and wallet1 of the second one, 4435aa1c.
-- The name of a copy carries the time it was made, so the tests look at
-- what comes after the time: the counts of wallets and portfolios.
databaseCopiesSpec :: Spec
databaseCopiesSpec =
  describe "The copies of the database" $ do
    -- The copy is the backup before the deletion, of 2 wallets. No copy
    -- of the wallet left follows it.
    it "are made before deletewallet and not after it: the one copy counts the wallets and portfolios the database held" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        map countsOf <$> databaseCopiesIn dir `shouldReturn` ["-wallets-2-portfolios-2.sqlite"]

    -- The copy is the backup before the deletion, of 2 portfolios
    it "are made before deleteportfolio and not after it: the one copy counts the portfolios the database held" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["deleteportfolio", "1", "--yes"]
        map countsOf <$> databaseCopiesIn dir `shouldReturn` ["-wallets-1-portfolios-2.sqlite"]

    it "hold the database as it was before deletewallet, with the wallet it deleted" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        copies <- databaseCopiesIn dir
        mapM (\copy -> inDatabaseFile (dir </> "auto-backups" </> copy) (length <$> getWallets)) copies `shouldReturn` [2]

    -- The file holds both wallets, which the second database, in a data
    -- directory of its own, lacks
    it "are made after importwallet when it creates portfolios" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        let file = dir </> "wallet.jsonl"
            otherDir = dir </> "other"
        _ <- hwAnswer wallet ["exportwallet", file]
        createDirectory otherDir
        other <- inWallet otherDir (return ())
        _ <- hwAnswer other ["importwallet", file]
        map countsOf <$> databaseCopiesIn otherDir `shouldReturn` ["-wallets-2-portfolios-2.sqlite"]

    -- The database has the portfolios of the file already
    it "are not made after importwallet when it creates no portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        let file = dir </> "wallet.jsonl"
        _ <- hwAnswer wallet ["exportwallet", file]
        _ <- hwAnswer wallet ["importwallet", file]
        databaseCopiesIn dir `shouldReturn` []

    it "are not made by label, which adds and deletes no portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["label", "savings"]
        databaseCopiesIn dir `shouldReturn` []

    -- portfolio0 is never deleted
    it "are not made by a deleteportfolio that is refused" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["deleteportfolio", "0", "--yes"]
        databaseCopiesIn dir `shouldReturn` []

    it "are not made when the configuration sets autobackup-database to false" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet {configAutoBackupDatabase = False} ["deleteportfolio", "1", "--yes"]
        databaseCopiesIn dir `shouldReturn` []

    -- Each deletewallet makes a copy before it: one of 2 wallets, then
    -- one of the wallet left. Two copies are among the ten newest, which
    -- are always kept.
    it "are both kept after two commands in a row" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        _ <- hwAnswer wallet ["deletewallet", "0", "--yes"]
        map countsOf <$> databaseCopiesIn dir
          `shouldReturn` ["-wallets-2-portfolios-2.sqlite", "-wallets-1-portfolios-1.sqlite"]

-- | The copies of the databases of a scratch directory, by their names in
-- their order: none when the auto-backups directory is not there
databaseCopiesIn :: FilePath -> IO [FilePath]
databaseCopiesIn = copyNamesIn ".sqlite"

-- | The files of the auto-backups directory of a scratch directory that
-- have the extension given, by their names in their order: none when the
-- directory is not there
copyNamesIn :: String -> FilePath -> IO [FilePath]
copyNamesIn extension dir = do
  let backups = dir </> "auto-backups"
  exists <- doesDirectoryExist backups
  if exists then sort . filter (extension `isSuffixOf`) <$> listDirectory backups else return []

-- | What the name of a copy says after its time, which ends with Z:
-- -wallets-1-portfolios-1.sqlite for a copy of the database
countsOf :: FilePath -> String
countsOf = reverse . takeWhile (/= 'Z') . reverse

{- The copies of the labels -}

-- The database of each test is portfolios.sqlite in its scratch directory,
-- so the copies of its labels go into the auto-backups directory there.
-- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
-- one, 4435aa1c.
labelCopiesSpec :: Spec
labelCopiesSpec =
  describe "The copies of the labels" $ do
    it "are made after label, and hold the description it gave" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["label", "savings"]
        map ("\"savings\"" `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [True]

    -- No address has the tag yet: the copy is the only file that says
    -- the tag locks its coins
    it "are made after tag --lock-coins, and hold the lock of a tag that nothing has yet" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["tag", "savings", "--lock-coins"]
        map ("\"taglocks\":[\"savings\"]" `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [True]

    -- The address book is in the file exportwallet writes for the whole
    -- database, which the copies hold
    it "are made after newcontact, and hold the contact it added" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["newcontact", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord"]
        map ("{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"label\":\"Landlord\",\"network\":\"btc\",\"contact\":true,\"added\":\"" `isInfixOf`) <$> labelCopiesIn dir
          `shouldReturn` [True]

    it "are made after deletecontact, and no longer hold the contact" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["newcontact", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord"]
        _ <- hwAnswer wallet ["deletecontact", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "--yes"]
        map ("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [True, False]

    -- The time in the name changes from run to run: the test looks at
    -- what comes after it
    it "are named after the counts of wallets and portfolios of the database" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["label", "savings"]
        map countsOf <$> copyNamesIn ".jsonl" dir `shouldReturn` ["-wallets-2-portfolios-2.jsonl"]

    it "hold the file exportwallet writes without options" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["label", "savings"]
        let file = dir </> "wallet.jsonl"
        _ <- hwAnswer wallet ["exportwallet", file]
        exported <- readFile file
        labelCopiesIn dir `shouldReturn` [exported]

    it "are not made again by a label that changes nothing" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["label", "savings"]
        _ <- hwAnswer wallet ["label", "savings"]
        length <$> labelCopiesIn dir `shouldReturn` 1

    it "are not made by switch, which changes no label" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        labelCopiesIn dir `shouldReturn` []

    -- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is of another wallet
    it "are not made by a label that is refused" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["label", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "rent"]
        labelCopiesIn dir `shouldReturn` []

    it "are not made when the configuration sets autobackup-labels to false" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet {configAutoBackupLabels = False} ["label", "savings"]
        labelCopiesIn dir `shouldReturn` []

    -- Each label makes a copy of the 2 wallets. Two copies are among the
    -- ten newest, which are always kept.
    it "are both kept after two commands in a row" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["label", "savings"]
        _ <- hwAnswer wallet ["label", "holidays"]
        map countsOf <$> copyNamesIn ".jsonl" dir `shouldReturn` ["-wallets-2-portfolios-2.jsonl", "-wallets-2-portfolios-2.jsonl"]

    -- The two copies were made while the copies were on, one after each
    -- of the first two labels
    it "stay as they are once both kinds of copy are turned off" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["label", "savings"]
        _ <- hwAnswer wallet ["label", "holidays"]
        let copiesOff = wallet {configAutoBackupDatabase = False, configAutoBackupLabels = False}
        _ <- hwAnswer copiesOff ["label", "rent"]
        map ("\"rent\"" `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [False, False]

    it "are made after receive --label, and hold the label of the address" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["receive", "--label", "rent"]
        map ("\"rent\"" `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [True]

    -- The address is given out without a label, which the file leaves out
    it "are not made by receive without --label" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["receive"]
        labelCopiesIn dir `shouldReturn` []

    it "are made after lockcoin, and hold the coin locked" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir coinOfPortfolio1
        _ <- hwAnswer wallet ["lockcoin", coinWord]
        map (coinWord `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [True]

    -- The copy is the backup before the deletion, with both wallets
    it "are made before deletewallet, with the wallet it deletes, and not after it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        map (\copy -> ("bc560f95" `isInfixOf` copy, "4435aa1c" `isInfixOf` copy)) <$> labelCopiesIn dir
          `shouldReturn` [(True, True)]

    -- The copies of the database are off, so no backup is made before
    -- the deletion. The copy of the labels is the one label made, with
    -- both wallets.
    it "are not made after deletewallet when no backup was made before it either" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["label", "savings"]
        _ <- hwAnswer wallet {configAutoBackupDatabase = False} ["deletewallet", "1", "--yes"]
        map (\copy -> ("bc560f95" `isInfixOf` copy, "4435aa1c" `isInfixOf` copy)) <$> labelCopiesIn dir
          `shouldReturn` [(True, True)]

    -- label made the copy, and the deletion finds the same labels in it
    it "are not made when the last wallet is deleted, and the copy made before still holds it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["label", "savings"]
        _ <- hwAnswer wallet ["deletewallet", "0", "--yes"]
        map ("bc560f95" `isInfixOf`) <$> labelCopiesIn dir `shouldReturn` [True]

-- | What each copy of the labels of the database of a scratch directory
-- holds, in the order of their names: none when the auto-backups
-- directory is not there
labelCopiesIn :: FilePath -> IO [String]
labelCopiesIn dir = mapM (readFile . ((dir </> "auto-backups") </>)) =<< copyNamesIn ".jsonl" dir

{- The backup before a deletion -}

-- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
-- one, 4435aa1c. The name of a backup carries the time it was made, so
-- the tests look at where it is and at what it holds.
backupBeforeSpec :: Spec
backupBeforeSpec =
  describe "The backup hw makes before it deletes" $ do
    it "is a copy of the database and its labels, both in auto-backups" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        made <- backupMadeBy <$> hwAnswer wallet ["deletewallet", "1", "--yes"]
        fmap (fmap (\(copy, labels) -> [(takeDirectory copy, takeExtension copy), (takeDirectory labels, takeExtension labels)])) made
          `shouldBe` Right (Just [(dir </> "auto-backups", ".sqlite"), (dir </> "auto-backups", ".jsonl")])

    -- user-backups holds what the backup command wrote, and nothing else
    it "puts nothing into user-backups" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        doesDirectoryExist (dir </> "user-backups") `shouldReturn` False

    -- label wrote the copy of the labels that the answer names: the
    -- deletion finds the same labels in it
    it "names the newest copy of the labels when it holds them already, and writes no second one" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["label", "savings"]
        copiesBefore <- copyNamesIn ".jsonl" dir
        made <- backupMadeBy <$> hwAnswer wallet ["deletewallet", "1", "--yes"]
        copiesAfter <- copyNamesIn ".jsonl" dir
        (fmap (fmap (takeFileName . snd)) made, map countsOf copiesAfter)
          `shouldBe` (Right (listToMaybe copiesBefore), ["-wallets-2-portfolios-2.jsonl"])

    it "holds the wallet that was deleted" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        made <- backupMadeBy <$> hwAnswer wallet ["deletewallet", "1", "--yes"]
        case made of
          Right (Just (copy, _)) -> inDatabaseFile copy (runExceptT walletsHeld) `shouldReturn` Right [(0, "bc560f95"), (1, "4435aa1c")]
          other -> expectationFailure ("No backup: " <> show other)

    -- The description is of portfolio0 of wallet1, the wallet deleted
    it "holds the labels of the wallet that was deleted" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        _ <- hwAnswer wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        _ <- hwAnswer wallet ["label", "holidays"]
        _ <- hwAnswer wallet ["switch", "--wallet", "0", "--portfolio", "0"]
        made <- backupMadeBy <$> hwAnswer wallet ["deletewallet", "1", "--yes"]
        case made of
          Right (Just (_, labels)) -> ("\"holidays\"" `isInfixOf`) <$> readFile labels `shouldReturn` True
          other -> expectationFailure ("No backup: " <> show other)

    -- The database held 1 wallet and 2 portfolios before portfolio1 went
    it "is made before deleteportfolio too, and counts the portfolios the database held before" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        made <- backupMadeBy <$> hwAnswer wallet ["deleteportfolio", "1", "--yes"]
        fmap (fmap (countsOf . fst)) made `shouldBe` Right (Just "-wallets-1-portfolios-2.sqlite")

    -- portfolio0 is never deleted
    it "is not made when the deletion is refused" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoPortfolios
        _ <- hwAnswer wallet ["deleteportfolio", "0", "--yes"]
        doesDirectoryExist (dir </> "auto-backups") `shouldReturn` False

    it "is not made when the configuration sets autobackup-database to false, and the wallet is deleted all the same" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        hwAnswer wallet {configAutoBackupDatabase = False} ["deletewallet", "1", "--yes"]
          `shouldReturn` ResponseDeleted "wallet1 has been deleted, with all of its portfolios" Nothing

    -- A file stands where the directory auto-backups goes. The name of
    -- the copy carries the time, so the refusal is checked around it.
    it "stops the deletion when it cannot be made, and says why and how to go on without it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        writeFile (dir </> "auto-backups") ""
        refused <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        let said = case refused of
              ResponseError err -> cs err
              other -> show other
        ( ("Nothing was deleted, since the backup that goes first could not be made. The database could not be copied to " <> (dir </> "auto-backups" </> "portfolios-version-5-")) `isPrefixOf` said,
          ".sqlite: not a directory. To go on without a backup, set autobackup-database to false in the configuration" `isSuffixOf` said
          )
          `shouldBe` (True, True)

    it "leaves the wallet in the database when it cannot be made" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir twoWallets
        writeFile (dir </> "auto-backups") ""
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        inDatabaseFile (dir </> "portfolios.sqlite") (runExceptT walletsHeld) `shouldReturn` Right [(0, "bc560f95"), (1, "4435aa1c")]

{- restoredatabase -}

-- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
-- one, 4435aa1c. The copy is made with backup, in a directory of its
-- own, copies. Every command gets --yes: these tests have no terminal to
-- answer the question on.
restoreSpec :: Spec
restoreSpec =
  describe "The command restoredatabase" $ do
    -- The copy holds both wallets, and wallet1 is deleted after it
    it "brings back a wallet deleted since the copy" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        inDatabaseFile (dir </> "portfolios.sqlite") (runExceptT walletsHeld) `shouldReturn` Right [(0, "bc560f95"), (1, "4435aa1c")]

    -- The copy holds wallet0 alone, and wallet1 is created after it
    it "takes away a wallet created since the copy" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        copy <- copyOf wallet dir
        inDatabaseFile (dir </> "portfolios.sqlite") (void (runExceptT (portfolioOf testMnemonic2)))
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        inDatabaseFile (dir </> "portfolios.sqlite") (runExceptT walletsHeld) `shouldReturn` Right [(0, "bc560f95")]

    it "brings back a description as the copy had it" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        _ <- hwAnswer wallet ["label", "savings"]
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["label", "holidays"]
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        inDatabaseFile (dir </> "portfolios.sqlite") (queryRows "select description from d_b_portfolio")
          `shouldReturn` [[PersistText "savings"]]

    -- The coin of 50000 goes with the last wallet, and comes back
    it "brings back the coins of the copy" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir coinOfPortfolio1
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["deletewallet", "0", "--yes"]
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        inDatabaseFile (dir </> "portfolios.sqlite") (queryRows "select value from d_b_coin")
          `shouldReturn` [[PersistInt64 50000]]

    -- portfolio1 is active in the copy, and portfolio0 in the database
    it "makes the active portfolio the one that was active in the copy" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoPortfolios
        _ <- hwAnswer wallet ["switch", "--portfolio", "1"]
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["switch", "--portfolio", "0"]
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        marksOf <$> hwAnswer wallet ["portfolios"] `shouldReturn` Right [(PortfolioRef 0 0, False), (PortfolioRef 0 1, True)]

    it "answers with the copy it read and the wallets the database holds now" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        restoredOf <$> hwAnswer wallet ["restoredatabase", copy, "--yes"]
          `shouldReturn` Right (copy, [(0, "bc560f95"), (1, "4435aa1c")])

    -- The copy holds portfolio0. The database it replaces holds two
    -- portfolios, which its backup keeps.
    it "backs up the database it replaces first, with what the copy lacks" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        copy <- copyOf wallet dir
        inDatabaseFile (dir </> "portfolios.sqlite") (void (runExceptT (nextPortfolioOf testMnemonic)))
        made <- backupMadeBy <$> hwAnswer wallet ["restoredatabase", copy, "--yes"]
        case made of
          Right (Just (earlier, _)) -> inDatabaseFile earlier (length <$> getAllPortfolios) `shouldReturn` 2
          other -> expectationFailure ("No backup: " <> show other)

    -- A new computer: the database has no wallet to lose
    it "makes no backup of a database without a wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        copy <- copyOf wallet dir
        createDirectory (dir </> "new")
        new <- versionedWalletIn (dir </> "new") (return ())
        backupMadeBy <$> hwAnswer new ["restoredatabase", copy, "--yes"] `shouldReturn` Right Nothing

    it "makes no backup when the configuration sets autobackup-database to false" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        copy <- copyOf wallet dir
        backupMadeBy <$> hwAnswer wallet {configAutoBackupDatabase = False} ["restoredatabase", copy, "--yes"]
          `shouldReturn` Right Nothing

    it "leaves the copy as it was" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        untouched <- BS.readFile copy
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        (== untouched) <$> BS.readFile copy `shouldReturn` True

    it "leaves no file beside the copy" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        listDirectory (dir </> "copies") `shouldReturn` ["copy.sqlite"]

    -- The copy holds wallet0 alone. The database it replaces holds two
    -- wallets: its backup is the one copy. No copy of the one wallet the
    -- database holds after the restore follows it.
    it "is not followed by a copy of the database: the one copy is the backup made first" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        copy <- copyOf wallet dir
        inDatabaseFile (dir </> "portfolios.sqlite") (void (runExceptT (portfolioOf testMnemonic2)))
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        map countsOf <$> databaseCopiesIn dir `shouldReturn` ["-wallets-2-portfolios-2.sqlite"]

    -- The copy holds the description savings, and the database it
    -- replaces holidays. The one copy of the labels is the backup made
    -- first.
    it "is not followed by a copy of the labels: the one copy holds the labels of the database it replaced" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        _ <- hwAnswer wallet {configAutoBackupLabels = False} ["label", "savings"]
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet {configAutoBackupLabels = False} ["label", "holidays"]
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        map (\labels -> ("\"savings\"" `isInfixOf` labels, "\"holidays\"" `isInfixOf` labels)) <$> labelCopiesIn dir
          `shouldReturn` [(False, True)]

    -- A new computer: no backup goes first, and none follows
    it "leaves no copy at all when the database it replaces holds no wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        copy <- copyOf wallet dir
        createDirectory (dir </> "new")
        new <- versionedWalletIn (dir </> "new") (return ())
        _ <- hwAnswer new ["restoredatabase", copy, "--yes"]
        doesDirectoryExist (dir </> "new" </> "auto-backups") `shouldReturn` False

    -- A backup has two files under one name
    it "refuses the file of the labels of a backup, and says where it goes" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        let labels = dir </> "wallet.jsonl"
        _ <- hwAnswer wallet ["exportwallet", labels]
        hwAnswer wallet ["restoredatabase", labels, "--yes"]
          `shouldReturn` ResponseError (cs (labels <> " is not an SQLite database. restoredatabase takes a copy of the database, such as the one backup wrote. The file of the labels of a backup goes to importwallet"))

    it "refuses the database itself" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        let database = dir </> "portfolios.sqlite"
        hwAnswer wallet ["restoredatabase", database, "--yes"]
          `shouldReturn` ResponseError (cs (database <> " is the database itself. Give a copy of it"))

    it "refuses a file that is not there" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        let missing = dir </> "missing.sqlite"
        hwAnswer wallet ["restoredatabase", missing, "--yes"]
          `shouldReturn` ResponseError (cs ("The file " <> missing <> " could not be read: no such file or directory"))

    it "refuses a copy of a newer hw, at format version 6, with its version" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir testPortfolio
        let newer = dir </> "newer.sqlite"
        inDatabaseFile newer version6Database
        hwAnswer wallet ["restoredatabase", newer, "--yes"]
          `shouldReturn` ResponseError (cs ("The SQLite database " <> newer <> " cannot be restored: it is a database of hw at format version 6, made by a newer hw, and restoredatabase reads version 5 only"))

    -- Another build of hw at version 5 gave the wallets one more column
    it "refuses a copy whose tables are not those of this hw, and names the first that differs" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        inDatabaseFile copy (rawExecute "alter table d_b_wallet add column nickname varchar" [])
        hwAnswer wallet ["restoredatabase", copy, "--yes"]
          `shouldReturn` ResponseError (cs ("The SQLite database " <> copy <> " cannot be restored: its table d_b_wallet is not the one of this hw, so it was made by another build of hw"))

    -- wallet1 was deleted after the copy, and stays deleted
    it "leaves the database as it was when it refuses the copy" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        _ <- hwAnswer wallet ["deletewallet", "1", "--yes"]
        inDatabaseFile copy (rawExecute "alter table d_b_wallet add column nickname varchar" [])
        _ <- hwAnswer wallet ["restoredatabase", copy, "--yes"]
        inDatabaseFile (dir </> "portfolios.sqlite") (runExceptT walletsHeld) `shouldReturn` Right [(0, "bc560f95")]

    -- A text took the place of the database, and the copy is of another
    -- build of hw, with one more column. hw knows the tables of the copy
    -- only once a database with its own tables is there to compare.
    it "says where the damaged database is, and that the database is new, when it refuses the copy after it moved a damaged database" $
      inScratchDirectory $ \dir -> do
        wallet <- versionedWalletIn dir twoWallets
        copy <- copyOf wallet dir
        inDatabaseFile copy (rawExecute "alter table d_b_wallet add column nickname varchar" [])
        writeFile (dir </> "portfolios.sqlite") "These are the notes of the user, not a database\n"
        answered <- hwAnswer wallet ["restoredatabase", copy]
        moved <- listDirectory (dir </> "broken-databases")
        case moved of
          [name] ->
            answered
              `shouldBe` ResponseError (cs ("The SQLite database " <> copy <> " cannot be restored: its table d_b_wallet is not the one of this hw, so it was made by another build of hw. The damaged database was moved to " <> (dir </> "broken-databases" </> name) <> ", and the database is new, without a wallet"))
          other -> expectationFailure ("Not one damaged database: " <> show other)

-- | A copy of the database of the wallet, as backup FILE writes it,
-- alone in the directory copies of the scratch directory: copies/copy.sqlite
copyOf :: Config -> FilePath -> IO FilePath
copyOf wallet dir = do
  let copy = dir </> "copies" </> "copy.sqlite"
  createDirectory (dir </> "copies")
  _ <- hwAnswer wallet ["backup", copy]
  return copy

-- | What restoredatabase answers with: the copy it read, and the wallets
-- the database holds now, each by its index and its fingerprint
restoredOf :: Response -> Either String (FilePath, [(Natural, Text)])
restoredOf (ResponseRestored file shown _ _) =
  Right (file, [(walletShownIndex wallet, fingerprintToText (walletShownFingerprint wallet)) | wallet <- databaseShownWallets shown])
restoredOf other = Left (show other)

{- showbackups -}

showBackupsSpec :: Spec
showBackupsSpec =
  describe "The command showbackups" $ do
    -- label made one copy of the labels. No command added or deleted a
    -- wallet or a portfolio, so the database has no copy.
    it "counts the copies hw made by itself, of each kind" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["label", "savings"]
        shown <- backupsOf <$> hwAnswer wallet ["showbackups"]
        fmap (\held -> (copiesCount (backupsDatabase held), copiesCount (backupsLabels held))) shown `shouldBe` Right (0, 1)

    it "counts the backups that backup wrote" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        _ <- hwAnswer wallet ["backup"]
        shown <- backupsOf <$> hwAnswer wallet ["showbackups"]
        fmap (copiesCount . backupsUser) shown `shouldBe` Right 1

    it "names the two directories of the data directory" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        shown <- backupsOf <$> hwAnswer wallet ["showbackups"]
        fmap (\held -> (backupsAutoDirectory held, backupsUserDirectory held)) shown
          `shouldBe` Right (dir </> "auto-backups", dir </> "user-backups")

    it "says which kind of copy the configuration turns off" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir testPortfolio
        shown <- backupsOf <$> hwAnswer wallet {configAutoBackupLabels = False} ["showbackups"]
        fmap (\held -> (backupsDatabaseOn held, backupsLabelsOn held)) shown `shouldBe` Right (True, False)
  where
    backupsOf (ResponseBackups shown) = Right shown
    backupsOf other = Left (show other)
