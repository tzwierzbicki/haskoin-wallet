{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A sweep of an old key, prepared by preparesweep and signed by
-- signsweep, into a portfolio of the wallet or elsewhere. hw finds the
-- portfolio and the network of the sweep from the --sweep-to addresses,
-- whichever portfolio is active. When they are in several portfolios, it
-- takes the active one if it is one of them, and otherwise asks for a
-- switch.
module Haskoin.Wallet.Workflow.SweepSpec (spec) where

import Data.IORef (modifyIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxFile (..))
import Haskoin.Wallet.Workflow.TestUtils
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  sweepSpec ctx

-- | A coin of 20000 satoshis at the first address of the second wallet,
-- another old wallet
otherOldCoin :: Ctx -> Tx
otherOldCoin ctx = fundingTx ctx 11 [(externalAddr2, 20000)]

-- | A chain whose block 800000 holds the coins of both old wallets
twoOldWalletsChain :: Ctx -> Chain
twoOldWalletsChain ctx = Chain 800002 [(oldCoin ctx, Just 800000), (otherOldCoin ctx, Just 800000)]

-- The coin of 30000 at the address of the key is swept at 10 satoshis a
-- byte: one input and one output make 192 bytes, a fee of 1920, and 28080
-- for the destination.
sweepSpec :: Ctx -> Spec
sweepSpec ctx =
  describe "A sweep of an old key" $ do
    it "is prepared by preparesweep, signed by signsweep with the key, sent, and paid to the wallet" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        pendingState prepared `shouldBe` Just (False, False)
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]
        nosig <- nosigOf prepared
        signed <- txOf =<< answer home ["signsweep", nosig, "--key-file", keys] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["broadcast", nosig, "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (externalAddr 0))])]
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 28080)]

    -- The keys are typed on the terminal, as secret as a mnemonic
    it "is shown by signsweep before it asks for the private keys" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig, "--key-file", "-"] [wikiKeyWif]
        let (beforeKeys, fromKeys) = Text.breakOn "Private keys" (runErr run)
        beforeKeys `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00028080 btc"
        fromKeys `shouldSatisfy` (not . Text.null)

    -- The best block of the server is 800002, mined three minutes before
    -- the test. One time in ten the lock time is below it, by up to 99
    -- blocks. The wallet has not synced: the sweep reads the server.
    it "gets the lock time of the best block of the server, or one up to 99 blocks below it" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        summaryLockTime (txInfoSummary prepared) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

    -- Nine hours: the server is behind the chain, which no sync of the
    -- wallet can help, so there is nothing to ask
    it "gets a lock time of 0 when the best block of the server is more than eight hours old, without a question" $
      withChainMinedAgo ctx [] (9 * 3600) (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        summaryLockTime (txInfoSummary prepared) `shouldBe` 0

    -- Without the key, hw counts it as compressed, which it is: the sweep
    -- is the same 192 bytes
    it "takes the coin of --coin, without its key or its address" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparesweep", "--coin", coinArg (oldCoin ctx) 0, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]

    it "takes the coins of the addresses in the --address-file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (textOf wikiAddr <> " old wallet\n")
        prepared <- txOf =<< answer home ["preparesweep", "--address-file", home </> "addresses.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]

    -- Two inputs and one output make 340 bytes, a fee of 3400: 30000 +
    -- 20000 - 3400 = 46600
    it "takes the coins of the --address-file addresses along with those of --sweep-from" $
      withChain ctx (twoOldWalletsChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (Text.unpack externalAddr2Text <> "\n")
        prepared <- txOf =<< answer home ["preparesweep", "--sweep-from", textOf wikiAddr, "--address-file", home </> "addresses.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 46600)]

    -- The keys would be typed for nothing
    it "is refused an --address-file that does not exist, before the keys are typed" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["preparesweep", "--key-file", "-", "--address-file", home </> "addresses.txt", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` printed (noSuchFile (home </> "addresses.txt"))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- The last letter of the address on the third line is wrong
    it "is refused a line of the --address-file with words but no address, which is named" $
      withHome $ \home -> do
        newTestPortfolio home
        writeFile (home </> "addresses.txt") ("# The old wallet\n" <> textOf wikiAddr <> "\n1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmX old wallet\n")
        (errorOf <$> hwJson home ["preparesweep", "--address-file", home </> "addresses.txt", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] [])
          `shouldReturn` Right "No address on line 3: 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmX old wallet"

    -- Two outputs make 226 bytes, a fee of 2260, which leaves 30000 - 2260 -
    -- 27500 = 240 for the second destination
    it "is refused when what remains for a destination is below the dust limit" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", fixedTo (otherAddr 0) "27500sat", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] [])
          `shouldReturn` Right "What remains for each address without an amount, 240 satoshis, is below the dust limit of 546"

    it "that pays outside the wallet is refused without an --output file" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--fee-rate", "10"] [])
          `shouldReturn` Right "The sweep does not involve this wallet: give an --output file to write it to"

    -- The wallet keeps no trace of it: the sweep is signed from its file,
    -- and broadcast sends it on the network the file names, since the
    -- wallet cannot tell it
    it "that pays outside the wallet is written to its file only, signed from it, and sent on the network it names" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 28080)]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        pendingState signed `shouldBe` Just (True, False)
        sent <- answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]
        (ResponseSentTx btc . txHash <$> published) `shouldBe` [sent]

    -- The keys would be typed for nothing
    it "is refused an --output file that exists, before the keys are typed" $
      withHome $ \home -> do
        newTestPortfolio home
        writeFile (home </> "sweep.json") ""
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", textOf (otherAddr 0), "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` printed (alreadyExists (home </> "sweep.json"))
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- The keys would be typed for nothing
    it "is refused an address that is not one, before the keys are typed" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", "nonsense", "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` printed "The address nonsense is not valid on btc"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- One input and two outputs make 226 bytes, a fee of 2260: 30000 - 10000
    -- - 2260 = 17740 for the address without an amount
    it "pays a --sweep-to address its fixed amount, and what remains to the address without one" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", fixedTo (otherAddr 0) "0.0001", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 10000)]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 17740)]

    it "is of the portfolio of a --sweep-to address that has an amount" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", fixedTo (externalAddr 0) "0.0001", "--sweep-to", textOf (otherAddr 0), "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right mainPlace
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(externalAddr 0, 10000)]

    -- The keys would be typed for nothing
    it "is refused --sweep-to addresses that all have an amount, before the keys are typed" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", fixedTo (otherAddr 0) "0.0001", "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` printed "Give at least one address without an amount, to receive what remains once the fixed amounts and the fee are paid"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- A rate of 2^64 wrapped around to zero, and others to any fee
    it "is refused a fee rate past 32 bits, rather than wrapping around" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per virtual byte"

    -- The wallet has portfolio0 and portfolio1 of the test seed, and the
    -- sweep is pending in portfolio1, which receive gave out the address of
    -- while it was active. portfolio0 is active when the sweep is signed.
    it "is signed by the portfolio that holds it, whichever is active" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        portfolio1Addr <- receiveWith home []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", portfolio1Addr, "--fee-rate", "10"] []
        switchTo home 0 0
        signed <- answer home ["signsweep", nosig, "--key-file", keys] []
        placeOf signed `shouldBe` Right portfolio1Place
        pendingState <$> txOf signed `shouldReturn` Just (True, False)

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- portfolio1, and the active portfolio is portfolio0
    it "is of the portfolio it pays, whichever is active" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["receive"] []
        switchTo home 0 1
        _ <- answer home ["receive"] []
        switchTo home 0 0
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", "17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "--fee-rate", "10"] [])
          `shouldReturn` Right portfolio1Place

    -- The second test mnemonic is wallet1, whose first receiving address
    -- is 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2, and the active portfolio is
    -- of wallet0
    it "is of the portfolio of another wallet it pays, whichever is active" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        _ <- answer home ["receive"] []
        switchTo home 0 0
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", "1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2", "--fee-rate", "10"] [])
          `shouldReturn` Right (Place (PortfolioRef 1 0) btc)

    -- The bip84 output of 31 bytes in place of a legacy one of 34: one
    -- input and one output make 10 + 148 + 31 = 189 bytes, a fee of 1890,
    -- and 30000 - 1890 = 28110 for the destination
    it "pays a bip84 address of the portfolio, whose output is smaller" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        bip84Addr <- receiveWith home ["--type", "bip84"]
        keys <- writeKeyFile home
        prepared <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", bip84Addr, "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right mainPlace
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(segwitExternalAddr 0, 28110)]

    -- With no account at all, hw sweeps as a utility, through files: on the
    -- --network given, then on the network the file names
    it "that pays outside the wallet is prepared, signed and sent from files in a database without wallets" $
      withChain ctx (oldKeyChain ctx) $ \home chain -> do
        keys <- writeKeyFile home
        prepared <- answer home (outsideSweep home keys) []
        prepared `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays ctx <$> published) `shouldBe` [([OutPoint (txHash (oldCoin ctx)) 0], [(28080, Right (otherAddr 0))])]
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- The configuration file of the test home names btc, and the key has a
    -- coin on Bitcoin. Keys, coins and the legacy addresses of Bitcoin Cash
    -- read the same on Bitcoin: the default network could sweep the coins
    -- of another chain than the one meant.
    it "that pays outside the wallet needs the --network it is for, whatever the default network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--output", home </> "sweep.json", "--fee-rate", "10"] [])
          `shouldReturn` Right "Give the --network of a sweep that pays no portfolio of the wallet: the default-network of the configuration file does not apply to it"

    -- The default network is not a --network given: the address is of
    -- the bip44 account of portfolio0 on btc
    it "that pays the wallet is on the network of its address, whatever the default network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        setDefaultNetwork home (Just "bch")
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] [])
          `shouldReturn` Right mainPlace

    -- hw stores the first receiving address of portfolio0 on bch as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. The store has
    -- the coin of the key on Bitcoin Cash too.
    it "that pays the wallet finds its portfolio by a Bitcoin Cash address written without its prefix" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", "qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "--fee-rate", "10"] [])
          `shouldReturn` Right (Place mainPortfolioRef bch)

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the same address in the
    -- legacy form, which Bitcoin reads too. The sweep is of portfolio0 on
    -- bch all the same, rather than outside the wallet on the default
    -- network btc.
    it "that pays the wallet finds its portfolio by a Bitcoin Cash address in the legacy form" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] [])
          `shouldReturn` Right (Place mainPortfolioRef bch)

    it "that pays outside the wallet is shown from its file, outside the wallet, on its network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        reviewed <- answer home ["show", home </> "sweep.json"] []
        reviewed `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        (paidToOthers <$> txOf reviewed) `shouldReturn` Map.fromList [(otherAddr 0, 28080)]

    it "that pays outside the wallet is not imported by importtx" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        (errorOf <$> hwJson home ["importtx", home </> "sweep.json"] [])
          `shouldReturn` Right "The transaction does not involve this wallet. It may be of a portfolio this database does not have yet: newportfolio --existing adds the next portfolio of a mnemonic, and importwallet the portfolios of a file"

    -- No key of the wallet signs it: the mnemonic would be typed for nothing
    it "that pays outside the wallet is refused by signtx, before the mnemonic is asked" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        run <- hw home ["signtx", "--input", home </> "sweep.json", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printed "The transaction does not involve this wallet"
        run `shouldNotSatisfy` askedMnemonic

    -- A file of another wallet names no network, and the configuration
    -- names no default network: the default network is then btc
    it "that pays outside the wallet is signed on btc from a file of another wallet, which names no network, without a default network" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        setDefaultNetwork home Nothing
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        written <- readTxFile ctx (home </> "sweep.json")
        writeOtherWalletTxFile ctx (home </> "other.psbt") (txFileData written)
        signed <- answer home ["signsweep", "--input", home </> "other.psbt", "--output", home </> "signed.psbt", "--key-file", keys] []
        signed `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        pendingState <$> txOf signed `shouldReturn` Just (True, False)

    -- portfolio0 and portfolio1 of the test wallet, each with the first
    -- receiving address of its bip44 account on btc, and portfolio0 of the
    -- wallet of the second test mnemonic, which the sweep does not pay,
    -- active
    it "that pays two portfolios is refused when the active portfolio is neither, naming them and saying to switch" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        portfolio0Addr <- receiveWith home []
        switchTo home 0 1
        portfolio1Addr <- receiveWith home []
        switchTo home 1 0
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", portfolio0Addr, "--sweep-to", portfolio1Addr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The destinations of the sweep are in several portfolios, portfolio0 of wallet0 on btc and portfolio1 of wallet0 on btc: switch to the one to use first"

    -- The user who switched to a portfolio means that one
    it "that pays two portfolios is of the active one, when it is one of them" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        portfolio0Addr <- receiveWith home []
        switchTo home 0 1
        portfolio1Addr <- receiveWith home []
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", portfolio0Addr, "--sweep-to", portfolio1Addr, "--fee-rate", "10"] [])
          `shouldReturn` Right portfolio1Place

    -- The first receiving addresses of portfolio0 on btc and on bch. A
    -- sweep is on one network: the default network btc would read the
    -- address of bch as one of Bitcoin.
    it "that pays the portfolio on two networks is refused, naming them" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        btcAddr <- receiveWith home []
        cashAddr <- receiveWith home ["--network", "bch"]
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", btcAddr, "--sweep-to", cashAddr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The sweep pays addresses of the wallet on btc and bch: a sweep is on one network"

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the address of portfolio0 on
    -- bch in the legacy form, which reads on btc as well
    it "that pays the portfolio on two networks is refused when the address of Bitcoin Cash is in the legacy form" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        btcAddr <- receiveWith home []
        _ <- answer home ["receive", "--network", "bch"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", btcAddr, "--sweep-to", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] [])
          `shouldReturn` Right "The sweep pays addresses of the wallet on btc and bch: a sweep is on one network"

    -- btctest and btctest4 share their keys: receive hands out the same
    -- first address on each, which the default network btc does not have
    it "that pays an address two test networks share is refused without --network, naming the networks" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        sharedAddr <- receiveWith home ["--network", "btctest"]
        _ <- answer home ["receive", "--network", "btctest4"] []
        (errorOf <$> hwJson home ["preparesweep", "--coin", coinArg (oldCoin ctx) 0, "--sweep-to", sharedAddr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The destinations of the sweep are on btctest and btctest4: give the --network"

    -- The coin is taken by its outpoint, which reads the same on every
    -- network of the fake store
    it "that pays an address two test networks share is of the --network named" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        sharedAddr <- receiveWith home ["--network", "btctest"]
        _ <- answer home ["receive", "--network", "btctest4"] []
        (placeOf <$> hwJson home ["preparesweep", "--coin", coinArg (oldCoin ctx) 0, "--sweep-to", sharedAddr, "--network", "btctest4", "--fee-rate", "10"] [])
          `shouldReturn` Right (Place mainPortfolioRef btcTest4)

    it "that pays the wallet is refused a --network other than the one of its address" $
      withChain ctx (oldKeyChain ctx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--network", "bch", "--fee-rate", "10"] [])
          `shouldReturn` Right "The destinations of the sweep are on btc, not on the --network bch"
  where
    -- The sweep of the old key to another wallet, written to sweep.json
    outsideSweep home keys =
      ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"]
