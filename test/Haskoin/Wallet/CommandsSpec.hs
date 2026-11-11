{-# LANGUAGE OverloadedStrings #-}

-- | The commands and the active portfolio, which the database keeps:
-- switch, which changes it, the commands that work in it, coin control,
-- which spends its coins alone, and the deletions, which refuse it. Each
-- test has a database of its own in a file, and runs the command as hw
-- runs it, from the words typed, without a terminal and without a store.
-- The commands that ask the user or the store, newportfolio, recover and
-- sync among them, run as the binary in "Haskoin.Wallet.Workflow".
module Haskoin.Wallet.CommandsSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Data.Default (def)
import Data.List (isInfixOf)
import Haskoin
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Commands (commandResponse)
import Haskoin.Wallet.Config (Config)
import Haskoin.Wallet.Database (DB, DBAddress (..))
import Haskoin.Wallet.Database.Account (PortfolioView (..))
import Haskoin.Wallet.Database.Address (genExtAddress)
import Haskoin.Wallet.Database.Coin (refreshCoins)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  switchSpec ctx
  workingSpec ctx
  coinControlSpec ctx
  deletingSpec ctx

{- Fixtures -}

-- | The answer of hw to the words typed after hw, in the database of the
-- configuration. Words the command line refuses fail the test.
hwAnswer :: Ctx -> Config -> [String] -> IO Response
hwAnswer ctx wallet typed = either fail (commandResponse ctx wallet UnitBitcoin) (parsedLine typed)

-- | A database in the scratch directory, with what the action creates
inWallet :: FilePath -> ExceptT String (DB IO) a -> IO Config
inWallet dir = walletInFile (dir </> "wallet.sqlite")

-- | Portfolios 0 and 1 of the test wallet, wallet0. Portfolio 0 is the
-- active one, as the first created.
twoPortfolios :: Ctx -> ExceptT String (DB IO) ()
twoPortfolios ctx = testPortfolio ctx >> void (nextPortfolioOf ctx testMnemonic)

-- | Portfolio 0 of the test wallet, wallet0, the active one as the first
-- created, and portfolio 0 of the second test wallet, wallet1
twoWallets :: Ctx -> ExceptT String (DB IO) ()
twoWallets ctx = testPortfolio ctx >> void (portfolioOf ctx testMnemonic2)

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
coinOfPortfolio1 :: Ctx -> ExceptT String (DB IO) ()
coinOfPortfolio1 ctx = do
  _ <- testPortfolio ctx
  second <- nextPortfolioOf ctx testMnemonic
  (accId, _) <- accountOf second btc Bip44
  _ <- genExtAddress ctx def accId ""
  void $ refreshCoins btc ctx accId [account1Addr] [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) account1Addr 50000]

-- | The coin of 'coinOfPortfolio1' as the command line takes it
coinWord :: String
coinWord = "0000000000000000000000000000000000000000000000000000000000000001:0"

-- | A payment of 0.0001 btc to an address of another wallet, at a fee
-- rate that needs no question
paymentWords :: [String]
paymentWords = ["preparetx", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU:0.0001", "--fee-rate", "10", "--yes"]

{- switch -}

switchSpec :: Ctx -> Spec
switchSpec ctx =
  describe "The command switch" $ do
    it "answers with the portfolio it made the active one, marked as active" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        shownOf <$> hwAnswer ctx wallet ["switch", "--portfolio", "1"] `shouldReturn` Right (PortfolioRef 0 1, True)

    -- The database keeps it: portfolios runs in a transaction of its own
    it "leaves the portfolio it switched to active for the next command, which portfolios marks" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        _ <- hwAnswer ctx wallet ["switch", "--portfolio", "1"]
        marksOf <$> hwAnswer ctx wallet ["portfolios"] `shouldReturn` Right [(PortfolioRef 0 0, False), (PortfolioRef 0 1, True)]

    it "refuses a portfolio the wallet does not have, naming where the portfolios are listed" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets ctx)
        hwAnswer ctx wallet ["switch", "--wallet", "1", "--portfolio", "1"]
          `shouldReturn` ResponseError "wallet1 has no portfolio1: portfolios lists the portfolios"

{- The commands that work in the active portfolio -}

workingSpec :: Ctx -> Spec
workingSpec ctx =
  describe "The commands that work in the active portfolio" $ do
    it "list it marked as the active one among the portfolios: the first one created" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        marksOf <$> hwAnswer ctx wallet ["portfolios"] `shouldReturn` Right [(PortfolioRef 0 0, True), (PortfolioRef 0 1, False)]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is /0/0 of m/44'/0'/1'
    it "give out an address of the active portfolio with receive" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        _ <- hwAnswer ctx wallet ["switch", "--portfolio", "1"]
        given <- hwAnswer ctx wallet ["receive"]
        case given of
          ResponseAddress ref addr _ -> (ref, dBAddressAddress addr) `shouldBe` (AccountRef (Place (PortfolioRef 0 1) btc) Bip44, "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo")
          other -> expectationFailure ("Not an address: " <> show other)

    it "describe the active portfolio with label, given the description alone" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        _ <- hwAnswer ctx wallet ["switch", "--portfolio", "1"]
        described <- hwAnswer ctx wallet ["label", "savings"]
        case described of
          ResponsePortfolio view -> (viewRef view, viewDescription view) `shouldBe` (PortfolioRef 0 1, "savings")
          other -> expectationFailure ("Not a portfolio: " <> show other)

    it "describe the wallet of the active portfolio with label --of-wallet, given the description alone" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets ctx)
        _ <- hwAnswer ctx wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        described <- hwAnswer ctx wallet ["label", "laptop", "--of-wallet"]
        case described of
          ResponsePortfolio view -> (viewRef view, viewWalletDescription view) `shouldBe` (PortfolioRef 1 0, "laptop")
          other -> expectationFailure ("Not a portfolio: " <> show other)

    -- wallet1 is of the second test mnemonic, 4435aa1c, and wallet0 of the
    -- test mnemonic, bc560f95
    it "write the wallet of the active portfolio with exportwallet" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets ctx)
        _ <- hwAnswer ctx wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        let file = dir </> "wallet.jsonl"
        hwAnswer ctx wallet ["exportwallet", file] `shouldReturn` ResponseFile file
        written <- readFile file
        ("4435aa1c" `isInfixOf` written, "bc560f95" `isInfixOf` written) `shouldBe` (True, False)

    it "refuse show --portfolio with anything but a file of a wallet, which is where it names a portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        hwAnswer ctx wallet ["show", "--portfolio", "1"]
          `shouldReturn` ResponseError "show takes --portfolio only with a file of a wallet, where it names one portfolio of the file. The rest is shown in the active portfolio, or where it is"

{- Coin control -}

-- Portfolio 0 is the active one, and the coin and its address are of
-- portfolio 1
coinControlSpec :: Ctx -> Spec
coinControlSpec ctx =
  describe "Coin control" $ do
    it "refuses a coin of another portfolio than the active one, naming the portfolio that holds it and the switch to it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (coinOfPortfolio1 ctx)
        hwAnswer ctx wallet (paymentWords <> ["--coin", coinWord])
          `shouldReturn` ResponseError "The coin 00000000:0 is in portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet 0 first"

    it "refuses an address of --spend-from of another portfolio than the active one, naming the portfolio that holds it and the switch to it" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (coinOfPortfolio1 ctx)
        hwAnswer ctx wallet (paymentWords <> ["--spend-from", "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo"])
          `shouldReturn` ResponseError "The address 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is in portfolio1 of wallet0, not in the active portfolio: run switch 1 --wallet 0 first"

    -- The payment is on the network of its coins, which --network may not
    -- contradict
    it "refuses a --network that holds none of the coins given, naming the network they are on" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (coinOfPortfolio1 ctx)
        _ <- hwAnswer ctx wallet ["switch", "--portfolio", "1"]
        hwAnswer ctx wallet (paymentWords <> ["--coin", coinWord, "--network", "bch"])
          `shouldReturn` ResponseError "The coins or addresses given are on btc, not on the --network bch"

{- Deleting -}

deletingSpec :: Ctx -> Spec
deletingSpec ctx = do
  describe "The command deleteportfolio" $ do
    it "deletes a portfolio of the wallet of the active portfolio" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        hwAnswer ctx wallet ["deleteportfolio", "--portfolio", "1"] `shouldReturn` ResponseDeleted "portfolio1 of wallet0 has been deleted"

    it "refuses the active portfolio, and says to switch to another one first" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoPortfolios ctx)
        _ <- hwAnswer ctx wallet ["switch", "--portfolio", "1"]
        hwAnswer ctx wallet ["deleteportfolio", "--portfolio", "1"]
          `shouldReturn` ResponseError "portfolio1 of wallet0 is the active portfolio: switch to another portfolio first"

    -- wallet1 has portfolios 0 and 1, and wallet0, of the active
    -- portfolio, has portfolio 0 alone
    it "looks for the portfolio in the wallet of the active portfolio alone" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets ctx >> void (nextPortfolioOf ctx testMnemonic2))
        hwAnswer ctx wallet ["deleteportfolio", "--portfolio", "1"]
          `shouldReturn` ResponseError "wallet0 has no portfolio1: portfolios lists the portfolios"

  -- wallet0 is of the test mnemonic, bc560f95, and wallet1 of the second
  -- one, 4435aa1c
  describe "The command deletewallet and the active portfolio" $ do
    it "refuses the wallet of the active portfolio while another wallet exists, and says to switch to a portfolio of another wallet first" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets ctx)
        hwAnswer ctx wallet ["deletewallet", "bc560f95", "--wallet", "0"]
          `shouldReturn` ResponseError "wallet0 holds the active portfolio, portfolio0 of wallet0: switch to a portfolio of another wallet first"

    it "deletes the wallet that held the active portfolio once the active portfolio is of another wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (twoWallets ctx)
        _ <- hwAnswer ctx wallet ["switch", "--wallet", "1", "--portfolio", "0"]
        hwAnswer ctx wallet ["deletewallet", "bc560f95", "--wallet", "0"]
          `shouldReturn` ResponseDeleted "wallet0 has been deleted, with all of its portfolios"

    it "deletes the last wallet, the one of the active portfolio, and says that the first portfolio created next becomes the active one" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (testPortfolio ctx)
        hwAnswer ctx wallet ["deletewallet", "bc560f95", "--wallet", "0"]
          `shouldReturn` ResponseDeleted "wallet0 has been deleted, with all of its portfolios. The database has no wallet left: the first portfolio created next becomes the active one."

    it "leaves no active portfolio once the last wallet is deleted: switch is refused, with how to make a wallet" $
      inScratchDirectory $ \dir -> do
        wallet <- inWallet dir (testPortfolio ctx)
        _ <- hwAnswer ctx wallet ["deletewallet", "bc560f95", "--wallet", "0"]
        hwAnswer ctx wallet ["switch", "--portfolio", "0"]
          `shouldReturn` ResponseError "There is no wallet yet: newportfolio makes one, and recover or importwallet bring one back"
