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

import qualified Data.ByteString as BS
import Data.IORef (modifyIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database.Portfolio (Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..), TxType (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Sync (nothingToSyncAfter)
import Haskoin.Wallet.Tx.SignData (TxFile (..), TxSignData (..), noLabel)
import Haskoin.Wallet.Tx.Sweep (UnsweptCoin (..), UnsweptReason (..))
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  sweepSpec
  dustSweepSpec
  ownAddressSpec
  pipedKeysSpec
  typedKeysSpec
  preparedTwiceSpec
  wrongSignerSpec
  unreadKeySpec
  notSweptSpec

-- | A coin of 20000 satoshis at the first address of the second wallet,
-- another old wallet
otherOldCoin :: Tx
otherOldCoin = fundingTx 11 [(externalAddr2, 20000)]

-- | A chain whose block 800000 holds the coins of both old wallets
twoOldWalletsChain :: Chain
twoOldWalletsChain = Chain 800002 [(oldCoin, Just 800000), (otherOldCoin, Just 800000)]

-- | Two small coins of the second old wallet, at its first address: 600
-- and 900 satoshis
smallOldCoins :: Tx
smallOldCoins = fundingTx 12 [(externalAddr2, 600), (externalAddr2, 900)]

-- | A chain whose block 800000 holds the coin of 30000 of the old key and
-- the two small coins
smallCoinsChain :: Chain
smallCoinsChain = Chain 800002 [(oldCoin, Just 800000), (smallOldCoins, Just 800000)]

-- The coin of 30000 at the address of the key is swept at 10 satoshis a
-- byte: one input and one output make 192 bytes, a fee of 1920, and 28080
-- for the destination.
sweepSpec :: Spec
sweepSpec =
  describe "A sweep of an old key" $ do
    it "is prepared by preparesweep, signed by signsweep with the key, sent, and paid to the wallet" $
      withChain oldKeyChain $ \home chain -> do
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
        (spendsAndPays <$> published) `shouldBe` [([OutPoint (txHash oldCoin) 0], [(28080, Right (externalAddr 0))])]
        modifyIORef chain mine
        (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 28080)]

    -- The keys are typed on the terminal, as secret as a mnemonic
    it "is shown by signsweep before it asks for the private keys" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig, "--key-file", "-"] [wikiKeyWif]
        let (beforeKeys, fromKeys) = Text.breakOn "Private keys" (runErr run)
        beforeKeys `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00028080 btc"
        fromKeys `shouldSatisfy` (not . Text.null)

    -- One input and two outputs make 226 bytes, a fee of 2260: 30000 - 10000
    -- - 2260 = 17740 for the address of another wallet. The coin of 30000 is
    -- at 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK.
    it "shows every destination and what it spends, before signsweep asks for the private keys" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        nosig <- nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", fixedTo (externalAddr 0) "0.0001", "--sweep-to", textOf (otherAddr 0), "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig, "--key-file", "-"] [wikiKeyWif]
        let beforeKeys = fst (Text.breakOn "Private keys" (runErr run))
        filter (\l -> any (`Text.isPrefixOf` l) ["  Spends", "  From", "  To", "     "]) (Text.lines beforeKeys)
          `shouldBe` [ "  Spends   : 0.00030000 btc of coins that are not this portfolio's",
                       "  From     : 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK  0.00030000 btc",
                       "  To       : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00010000 btc  bip44/0/0",
                       "             1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  0.00017740 btc"
                     ]

    -- 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2 holds nothing on this chain. The
    -- user may believe that it was swept.
    it "names a --sweep-from address that holds no coin, below the transaction" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-from", "1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n\n1 address given holds no coin and is not in the sweep:\n  1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2\n"

    it "gives the --sweep-from addresses that hold no coin in its --json answer" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- answer home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-from", "1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        prepared `shouldSatisfy` \case ResponseSweep _ _ _ empty _ _ -> empty == [externalAddr2]; _ -> False

    it "says nothing of addresses without a coin when every address given holds one" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        runOut run `shouldNotSatisfy` Text.isInfixOf "holds no coin"

    -- The best block of the server is 800002, mined three minutes before
    -- the test. One time in ten the lock time is below it, by up to 99
    -- blocks. The wallet has not synced: the sweep reads the server.
    it "gets the lock time of the best block of the server, or one up to 99 blocks below it" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        summaryLockTime (txInfoSummary prepared) `shouldSatisfy` (\lockTime -> lockTime <= 800002 && lockTime >= 800002 - 99)

    -- Nine hours: the server is behind the chain, which no sync of the
    -- wallet can help, so there is nothing to ask
    it "gets a lock time of 0 when the best block of the server is more than eight hours old, without a question" $
      withChainMinedAgo [] (9 * 3600) oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        summaryLockTime (txInfoSummary prepared) `shouldBe` 0

    -- Without the key, hw cannot tell how the public key of the coin is
    -- written, and counts the longer one: 10 + 180 + 34 = 224 bytes, a fee
    -- of 2240, and 30000 - 2240 = 27760
    it "takes the coin of --coin, without its key or its address" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparesweep", "--coin", coinArg oldCoin 0, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 27760)]

    -- The plain case of an old paper wallet: its key is uncompressed and
    -- stays away from the online computer, and 1 satoshi for each byte is
    -- the lowest rate. Without the key, hw counts the longer input: 10 +
    -- 180 + 34 = 224 bytes, a fee of 224, and 30000 - 224 = 29776. Signed,
    -- the sweep is those 224 bytes, or one less: it pays the rate asked,
    -- so a node that asks for 1 satoshi for each byte takes it.
    it "prepared from the address of an uncompressed key, without the key, pays the rate asked once the key signs it" $
      withChain uncompressedKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--sweep-from", textOf wikiUncompressedAddr, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "1"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 29776)]
        nosig <- nosigOf prepared
        _ <- answer home ["signsweep", nosig, "--key-file", keys] []
        listed <- hw home ["pendingtxs"] []
        runOut listed `shouldSatisfy` Text.isInfixOf "signed, not sent, 1 sat/vB"

    -- The same 224 bytes: 30000 - 2240 = 27760
    it "takes the coins of the addresses in the --address-file" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (textOf wikiAddr <> " old wallet\n")
        prepared <- txOf =<< answer home ["preparesweep", "--address-file", home </> "addresses.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 27760)]

    -- Two inputs of keys that were not given, 180 bytes each, and one
    -- output make 10 + 360 + 34 = 404 bytes, a fee of 4040: 30000 + 20000 -
    -- 4040 = 45960
    it "takes the coins of the --address-file addresses along with those of --sweep-from" $
      withChain twoOldWalletsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (Text.unpack externalAddr2Text <> "\n")
        prepared <- txOf =<< answer home ["preparesweep", "--sweep-from", textOf wikiAddr, "--address-file", home </> "addresses.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 45960)]

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

    -- A user with a paper wallet has the key in hand, not its address. The
    -- error goes to the screen and to the log of a script.
    it "is refused a private key as --sweep-from, told where the keys go, and does not print the key" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["preparesweep", "--sweep-from", Text.unpack wikiKeyWif, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "This is a private key, not an address. Give the keys with --key-file, or the address of the key with --sweep-from"
        run `shouldNotSatisfy` printed "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617"

    it "is refused a private key in the --address-file, told where the keys go, and does not print the key" $
      withHome $ \home -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        run <- hw home ["preparesweep", "--address-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "Line 1 holds a private key, not an address. Give the keys with --key-file, or the address of the key in the --address-file"
        run `shouldNotSatisfy` printed "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98617"

    it "does not print a private key given as --sweep-from in its --json error" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["preparesweep", "--sweep-from", Text.unpack wikiKeyWif, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] [])
          `shouldReturn` Right "This is a private key, not an address. Give the keys with --key-file, or the address of the key with --sweep-from"

    -- Two outputs make 226 bytes, a fee of 2260, which leaves 30000 - 2260 -
    -- 27500 = 240 for the second destination
    it "is refused when what remains for a destination is below the dust limit" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", fixedTo (otherAddr 0) "27500sat", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] [])
          `shouldReturn` Right "What remains for each address without an amount, 240 satoshis, is below the dust limit of 546"

    it "that pays outside the wallet is refused without an --output file" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--fee-rate", "10"] [])
          `shouldReturn` Right "The sweep does not involve this wallet: give an --output file to write it to"

    -- The wallet keeps no trace of it: the sweep is signed from its file,
    -- and broadcast sends it on the network the file names, since the
    -- wallet cannot tell it
    it "that pays outside the wallet is written to its file only, signed from it, and sent on the network it names" $
      withChain oldKeyChain $ \home chain -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 28080)]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        pendingState signed `shouldBe` Just (True, False)
        sent <- answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays <$> published) `shouldBe` [([OutPoint (txHash oldCoin) 0], [(28080, Right (otherAddr 0))])]
        ((\tx -> ResponseSentTx btc (txHash tx) nothingToSyncAfter) <$> published) `shouldBe` [sent]

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
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", fixedTo (otherAddr 0) "0.0001", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 10000)]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 17740)]

    it "is of the portfolio of a --sweep-to address that has an amount" $
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "18446744073709551616"] [])
          `shouldReturn` Right "The fee rate must be at most 4294967295 satoshis per virtual byte"

    -- The wallet has portfolio0 and portfolio1 of the test seed, and the
    -- sweep is pending in portfolio1, which receive gave out the address of
    -- while it was active. portfolio0 is active when the sweep is signed.
    it "is signed by the portfolio that holds it, whichever is active" $
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        bip84Addr <- receiveWith home ["--type", "bip84"]
        keys <- writeKeyFile home
        prepared <- answer home ["preparesweep", "--key-file", keys, "--sweep-to", bip84Addr, "--fee-rate", "10"] []
        placeOf prepared `shouldBe` Right mainPlace
        paidToWallet <$> txOf prepared `shouldReturn` Map.fromList [(segwitExternalAddr 0, 28110)]

    -- With no account at all, hw sweeps as a utility, through files: on the
    -- --network given, then on the network the file names
    it "that pays outside the wallet is prepared, signed and sent from files in a database without wallets" $
      withChain oldKeyChain $ \home chain -> do
        keys <- writeKeyFile home
        prepared <- answer home (outsideSweep home keys) []
        prepared `shouldSatisfy` \case ResponseSweep Nothing net _ _ _ _ -> net == btc; _ -> False
        signed <- txOf =<< answer home ["signsweep", "--input", home </> "sweep.json", "--output", home </> "signed.psbt", "--key-file", keys] []
        pendingState signed `shouldBe` Just (True, False)
        _ <- answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
        published <- inMempool <$> readIORef chain
        (spendsAndPays <$> published) `shouldBe` [([OutPoint (txHash oldCoin) 0], [(28080, Right (otherAddr 0))])]
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- The configuration file of the test home names btc, and the key has a
    -- coin on Bitcoin. Keys, coins and the legacy addresses of Bitcoin Cash
    -- read the same on Bitcoin: the default network could sweep the coins
    -- of another chain than the one meant.
    it "that pays outside the wallet needs the --network it is for, whatever the default network" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--output", home </> "sweep.json", "--fee-rate", "10"] [])
          `shouldReturn` Right "Give the --network of a sweep that pays no portfolio of the wallet: the default-network of the configuration file does not apply to it"

    -- The default network is not a --network given: the address is of
    -- the bip44 account of portfolio0 on btc
    it "that pays the wallet is on the network of its address, whatever the default network" $
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "bch"] []
        keys <- writeKeyFile home
        (placeOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] [])
          `shouldReturn` Right (Place mainPortfolioRef bch)

    it "that pays outside the wallet is shown from its file, outside the wallet, on its network" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        reviewed <- answer home ["show", home </> "sweep.json"] []
        reviewed `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        (paidToOthers <$> txOf reviewed) `shouldReturn` Map.fromList [(otherAddr 0, 28080)]

    -- The coin of 30000 pays 28080 and 1920 of fee, on 192 bytes: 10
    -- satoshis a byte. The portfolio neither gains nor loses.
    it "that pays outside the wallet is headed External, with the total it spends and no sign" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        run <- hw home (outsideSweep home keys) []
        Text.drop 17 (head (Text.lines (runOut run))) `shouldBe` "  External      0.00030000 btc  not signed, 10 sat/vB"

    -- preparesweep knows when it prepared the sweep, which no file holds
    it "that pays outside the wallet starts with a time when preparesweep answers, not with words for a missing one" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        run <- hw home (outsideSweep home keys) []
        let firstLine = head (Text.lines (runOut run))
        firstLine `shouldNotSatisfy` Text.isPrefixOf "from a file"
        firstLine `shouldNotSatisfy` Text.isPrefixOf "time unknown"

    it "that pays outside the wallet says from a file where the time goes, when show reads its file" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        run <- hw home ["show", home </> "sweep.json"] []
        head (Text.lines (runOut run)) `shouldBe` "from a file        External      0.00030000 btc  not signed, 10 sat/vB"

    it "that pays outside the wallet is of the type external in its --json answer" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        prepared <- txOf =<< answer home (outsideSweep home keys) []
        txRow prepared `shouldBe` (TxExternal, 0, Nothing)

    it "that pays outside the wallet is not imported by importtx" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        (errorOf <$> hwJson home ["importtx", home </> "sweep.json"] [])
          `shouldReturn` Right "The transaction does not involve this wallet. It may be of a portfolio this database does not have yet: newportfolio --existing adds the next portfolio of a mnemonic, and importwallet the portfolios of a file. A sweep that pays no address of the wallet stays in its file: hw signsweep --input signs it there, and hw broadcast --input sends it"

    -- No key of the wallet signs it: the mnemonic would be typed for nothing
    it "that pays outside the wallet is refused by signtx, before the mnemonic is asked" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        run <- hw home ["signtx", "--input", home </> "sweep.json", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printed "The transaction does not involve this wallet"
        run `shouldSatisfy` printedSentence ("The private keys of its coins sign a sweep: hw signsweep --input " <> Text.pack (home </> "sweep.json"))
        run `shouldNotSatisfy` askedMnemonic

    -- A file of another wallet names no network, and the configuration
    -- names no default network: the default network is then btc
    it "that pays outside the wallet is signed on btc from a file of another wallet, which names no network, without a default network" $
      withChain oldKeyChain $ \home _ -> do
        setDefaultNetwork home Nothing
        keys <- writeKeyFile home
        _ <- answer home (outsideSweep home keys) []
        written <- readTxFile (home </> "sweep.json")
        writeOtherWalletTxFile (home </> "other.psbt") (txFileData written)
        signed <- answer home ["signsweep", "--input", home </> "other.psbt", "--output", home </> "signed.psbt", "--key-file", keys] []
        signed `shouldSatisfy` \case ResponseOutsideTx net _ -> net == btc; _ -> False
        pendingState <$> txOf signed `shouldReturn` Just (True, False)

    -- portfolio0 and portfolio1 of the test wallet, each with the first
    -- receiving address of its bip44 account on btc, and portfolio0 of the
    -- wallet of the second test mnemonic, which the sweep does not pay,
    -- active
    it "that pays two portfolios is refused when the active portfolio is neither, naming them and saying to switch" $
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
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
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        btcAddr <- receiveWith home []
        cashAddr <- receiveWith home ["--network", "bch"]
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", btcAddr, "--sweep-to", cashAddr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The sweep pays addresses of the wallet on btc and bch: a sweep is on one network"

    -- 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the address of portfolio0 on
    -- bch in the legacy form, which reads on btc as well
    it "that pays the portfolio on two networks is refused when the address of Bitcoin Cash is in the legacy form" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        btcAddr <- receiveWith home []
        _ <- answer home ["receive", "--network", "bch"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", btcAddr, "--sweep-to", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ", "--fee-rate", "10"] [])
          `shouldReturn` Right "The sweep pays addresses of the wallet on btc and bch: a sweep is on one network"

    -- btctest and btctest4 share their keys: receive hands out the same
    -- first address on each, which the default network btc does not have
    it "that pays an address two test networks share is refused without --network, naming the networks" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        sharedAddr <- receiveWith home ["--network", "btctest"]
        _ <- answer home ["receive", "--network", "btctest4"] []
        (errorOf <$> hwJson home ["preparesweep", "--coin", coinArg oldCoin 0, "--sweep-to", sharedAddr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The destinations of the sweep are on btctest and btctest4: give the --network"

    -- The coin is taken by its outpoint, which reads the same on every
    -- network of the fake store
    it "that pays an address two test networks share is of the --network named" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        sharedAddr <- receiveWith home ["--network", "btctest"]
        _ <- answer home ["receive", "--network", "btctest4"] []
        (placeOf <$> hwJson home ["preparesweep", "--coin", coinArg oldCoin 0, "--sweep-to", sharedAddr, "--network", "btctest4", "--fee-rate", "10"] [])
          `shouldReturn` Right (Place mainPortfolioRef btcTest4)

    -- The default network chose between them before, without a word: a
    -- sweep on the wrong one takes the coins of another chain
    it "that pays an address two test networks share is refused without --network when one of them is the default network" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        sharedAddr <- receiveWith home ["--network", "btctest"]
        _ <- answer home ["receive", "--network", "btctest4"] []
        setDefaultNetwork home (Just "btctest4")
        (errorOf <$> hwJson home ["preparesweep", "--coin", coinArg oldCoin 0, "--sweep-to", sharedAddr, "--fee-rate", "10"] [])
          `shouldReturn` Right "The destinations of the sweep are on btctest and btctest4: give the --network"

    -- The last letter of the first address of the wallet is wrong. The
    -- answer asked for a --network, then for an --output, and only then
    -- said that the address was not one.
    it "is refused a --sweep-to address that reads on no network, before a --network or the keys are asked for" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLX", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "Error: The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLX is not valid on any network"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    it "is refused a --sweep-to address with an amount that reads on no network, named without its amount" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", "-", "--sweep-to", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLX:0.001", "--sweep-to", textOf (otherAddr 0), "--fee-rate", "10"] [])
          `shouldReturn` Right "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLX is not valid on any network"

    it "is refused a private key as --sweep-to, and does not print the key" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", "-", "--sweep-to", Text.unpack wikiKeyWif, "--fee-rate", "10"] [])
          `shouldReturn` Right "This is a private key, not an address"

    -- The store refuses every connection. Without --fee-rate, the answer
    -- ended with "Give a --fee-rate to go on without the rates recent
    -- blocks paid", and with one the sweep failed all the same.
    it "is refused when the store cannot be reached, as a command that needs the server, with no advice of a --fee-rate" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        setConfigKey home "host" "http://127.0.0.1:1/"
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-to", textOf (externalAddr 0)] [wikiKeyWif]
        run `shouldSatisfy` printedSentence "Error: The store at http://127.0.0.1:1/ could not be reached: the connection failed. A sweep reads its coins from the server, and cannot be prepared without it"
        run `shouldNotSatisfy` printedSentence "Give a --fee-rate"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    it "is refused a --key-file that does not exist, in the words of the other files, with its name" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["preparesweep", "--key-file", home </> "missing.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] [])
          `shouldReturn` Right (Text.pack ("The file " <> home </> "missing.txt" <> " could not be read: no such file or directory"))

    -- The sweep is long, and the error came below it
    it "is refused by signsweep with a --key-file that does not exist, before the sweep is shown" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        run <- hw home ["signsweep", nosig, "--key-file", home </> "missing.txt"] []
        run `shouldSatisfy` printedSentence (Text.pack ("Error: The file " <> home </> "missing.txt" <> " could not be read: no such file or directory"))
        run `shouldNotSatisfy` printed "1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK"

    -- The file has the fingerprint of the portfolio, which the wallet
    -- does not keep with the transaction: broadcast took the file for
    -- another version, and answered "The transaction already exists"
    it "is sent by broadcast --input from the signed file that signsweep --output wrote of the pending sweep" $
      withChain oldKeyChain $ \home chain -> do
        nosig <- sweepPrepared home
        _ <- answer home ["signsweep", nosig, "--key-file", home </> "keys.txt", "--output", home </> "signed.psbt"] []
        sent <- txOf =<< answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
        pendingState sent `shouldBe` Just (True, True)
        published <- inMempool <$> readIORef chain
        (spendsAndPays <$> published) `shouldBe` [([OutPoint (txHash oldCoin) 0], [(28080, Right (externalAddr 0))])]

    it "that pays the wallet is refused a --network other than the one of its address" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--network", "bch", "--fee-rate", "10"] [])
          `shouldReturn` Right "The destinations of the sweep are on btc, not on the --network bch"
  where
    -- The sweep of the old key to another wallet, written to sweep.json
    outsideSweep home keys =
      ["preparesweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.json", "--fee-rate", "10"]

-- The old wallet holds 30000 at one address, and 600 and 900 at another.
-- The sweep is prepared from the addresses, without the keys, so each
-- legacy coin counts for 180 bytes: 1800 satoshis at 10 satoshis a byte,
-- more than the coins of 600 and of 900 are worth. With them, the sweep is
-- 10 + 3 × 180 + 34 = 584 bytes, a fee of 5840, and 31500 - 5840 = 25660
-- for the wallet. Without them, it is 10 + 180 + 34 = 224 bytes, a fee of
-- 2240, and 30000 - 2240 = 27760 for the wallet: 2100 more. The two small
-- coins add 2 × 1800 = 3600 of fee for 1500 of coins.
dustSweepSpec :: Spec
dustSweepSpec =
  describe "The coins worth less than their fee, swept by preparesweep or not" $ do
    it "are listed on stderr with what the destination gets with them and without them, and asked about" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home sweepOfBoth ["n"]
        run `shouldSatisfy` printedSentence "2 coins cost more to spend than they are worth at 10 sat/vB, 0.00001800 btc each:"
        run `shouldSatisfy` printedSentence "Spending them adds 0.00003600 btc of fee for 0.00001500 btc of coins. What remains for the addresses without an amount is 0.00025660 btc instead of 0.00027760 btc, 0.00002100 btc less."
        runErr run `shouldSatisfy` Text.isInfixOf dustQuestion

    it "stay at their address when the user answers anything but yes" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home sweepOfBoth ["n"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 27760)]

    it "are swept too when the user answers yes" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home sweepOfBoth ["y"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 25660)]

    -- The answer typed is for a question that must not come
    it "stay at their address with --leave-dust, which no typed yes changes" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home (sweepOfBoth <> ["--leave-dust"]) ["y"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 27760)]

    it "are swept too with --spend-dust, which no typed no changes" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home (sweepOfBoth <> ["--spend-dust"]) ["n"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 25660)]

    it "are not asked about with --leave-dust or --spend-dust" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home (sweepOfBoth <> ["--leave-dust"]) []
        runErr run `shouldNotSatisfy` Text.isInfixOf dustQuestion

    -- A sweep that left a coin without a word would read as one that
    -- swept it
    it "are named below the sweep when they stay, the largest first, each by its coin, its address and its amount" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home (sweepOfBoth <> ["--leave-dust"]) []
        runOut run
          `shouldSatisfy` Text.isInfixOf
            ( Text.pack $
                "\n\n2 coins are worth less than their fee and are not in the sweep:\n"
                  <> ("  " <> coinArg smallOldCoins 1 <> "\n")
                  <> "    at 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2  0.00000900 btc\n"
                  <> ("  " <> coinArg smallOldCoins 0 <> "\n")
                  <> "    at 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2  0.00000600 btc\n"
            )

    it "are given in the --json answer when they stay, the largest first" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- answer home (sweepOfBoth <> ["--leave-dust"]) []
        unsweptOf prepared
          `shouldBe` [ UnsweptCoin (OutPoint (txHash smallOldCoins) 1) externalAddr2 900 WorthLessThanItsFee,
                       UnsweptCoin (OutPoint (txHash smallOldCoins) 0) externalAddr2 600 WorthLessThanItsFee
                     ]

    it "are not named when they are swept" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home (sweepOfBoth <> ["--spend-dust"]) []
        runOut run `shouldNotSatisfy` Text.isInfixOf "not in the sweep"

    -- The user who names a coin may not know what its input costs. Swept
    -- without a question, the three coins would leave 25660. The answer no
    -- leaves the coin of 30000 alone: 27760 for the wallet.
    it "are asked about when --coin gives them too" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparesweep", "--coin", coinArg oldCoin 0, "--coin", coinArg smallOldCoins 0, "--coin", coinArg smallOldCoins 1, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] ["n"]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 27760)]

    -- --yes is about the fee rate: it must not spend 3600 of fee for 1500
    -- of coins on its own
    it "are asked about with --yes too, which does not answer for them" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home (sweepOfBoth <> ["--yes"]) ["n"]
        runErr run `shouldSatisfy` Text.isInfixOf dustQuestion

    -- At 1 satoshi a byte, each input costs 180: the coins of 600 and of 900
    -- are worth more
    it "are not asked about at a fee rate that makes every coin worth its fee" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-from", Text.unpack externalAddr2Text, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "1"] []
        runErr run `shouldNotSatisfy` Text.isInfixOf "Spend"
        runOut run `shouldNotSatisfy` Text.isInfixOf "not in the sweep"
  where
    -- The sweep of both addresses of the old wallet to the wallet
    sweepOfBoth =
      ["preparesweep", "--sweep-from", textOf wikiAddr, "--sweep-from", Text.unpack externalAddr2Text, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"]
    -- The coins a sweep just prepared leaves at their addresses
    unsweptOf = \case ResponseSweep _ _ _ _ unswept _ -> unswept; _ -> []

-- A sweep is signed with private keys from outside the wallet. The user has
-- none for an address of the wallet, so a sweep of one could not be signed
-- as its next step says, and its coins would stay locked by it. sendall and
-- preparetx move those coins. 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is the
-- first receiving address of the bip44 account of portfolio0 on btc, and
-- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc that of its bip84 account.
ownAddressSpec :: Spec
ownAddressSpec =
  describe "A sweep from an address of the wallet" $ do
    it "is refused, naming the portfolio of the address and the commands that move its coins" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["preparesweep", "--sweep-from", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] [])
          `shouldReturn` Right "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is of this wallet, in portfolio0 of wallet0 on btc. A sweep is for coins from outside the wallet, which signsweep signs with their private keys. To move coins of this wallet, use sendall or preparetx: --spend-from chooses the coins of an address."

    -- The case of the review: from an address of the wallet to another one
    it "is refused when it pays the wallet too, and nothing is kept pending" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        bip84Addr <- receiveWith home ["--type", "bip84"]
        run <- hw home ["preparesweep", "--sweep-from", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--sweep-to", bip84Addr, "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is of this wallet, in portfolio0 of wallet0 on btc."
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "names every address of the wallet among those given" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--type", "bip84"] []
        run <- hw home ["preparesweep", "--sweep-from", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--sweep-from", textOf wikiAddr, "--sweep-from", "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "The addresses 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj and bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc are of this wallet, in portfolio0 of wallet0 on btc."

    it "is refused for an address of the wallet in the --address-file" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "addresses.txt") (textOf wikiAddr <> "\n1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\n")
        run <- hw home ["preparesweep", "--address-file", home </> "addresses.txt", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is of this wallet, in portfolio0 of wallet0 on btc."

    -- The keys would be typed for nothing
    it "is refused before the keys are typed" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-from", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is of this wallet, in portfolio0 of wallet0 on btc."
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- The first output of the transaction pays 50000 to
    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj: hw learns the address of the coin
    -- from the server
    it "is refused for a coin of --coin that is at an address of the wallet" $
      withChain (confirmedChain dustCoinsTx) $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <- hw home ["preparesweep", "--coin", coinArg dustCoinsTx 0, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is of this wallet, in portfolio0 of wallet0 on btc."

    -- The address of the bip44 account on btc reads on Bitcoin Cash too,
    -- where no account of the wallet has it: coins sent to it on the wrong
    -- chain are from outside the wallet. The sweep goes on, and finds no
    -- coin at the address on this chain.
    it "is not refused on a network where the wallet does not have the address" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["preparesweep", "--sweep-from", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--sweep-to", textOf (otherAddr 0), "--network", "bch", "--output", home </> "sweep.psbt", "--fee-rate", "10"] [])
          `shouldReturn` Right "There are no coins to sweep in those addresses"

-- | A home with the sweep of the coin of the old key prepared, to pay the
-- wallet 28080: its nosigHash
sweepPrepared :: FilePath -> IO String
sweepPrepared home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  keys <- writeKeyFile home
  nosigOf =<< txOf =<< answer home ["preparesweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] []

-- A pipe has an end, so the keys are all of it, as a key file has them:
-- one or more on each line. The coin of 30000 is at the address of the
-- second key piped. Swept at 10 satoshis a byte, 192 bytes cost 1920, and
-- 28080 remain.
pipedKeysSpec :: Spec
pipedKeysSpec =
  describe "The private keys of a sweep, piped to hw" $ do
    it "are read by signsweep from every line" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        signed <- txOf =<< answer home ["signsweep", nosig] [otherKeyWif, wikiKeyWif]
        pendingState signed `shouldBe` Just (True, False)

    it "are read by preparesweep --key-file - from every line" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        prepared <- txOf =<< answer home ["preparesweep", "--key-file", "-", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] [otherKeyWif, wikiKeyWif]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]

    it "are refused when a line has words but no key, which is named by its number" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        (errorOf <$> hwJson home ["signsweep", nosig] [wikiKeyWif, "nonsense"])
          `shouldReturn` Right "No private key on line 2"

    -- The question about the dust coins comes after the keys, when the
    -- input has ended: --leave-dust leaves the coins of 600 and of 900. The
    -- key tells that the coin of 30000 has a compressed public key: 192
    -- bytes cost 1920, and 28080 remain.
    it "leave no line of the input to a later question, which an option answers" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home sweepWithKeys [wikiKeyWif])
          `shouldReturn` Right "Cancelled: the input ended before the answer. In a script, --spend-dust or --leave-dust answers this question."
        prepared <- txOf =<< answer home (sweepWithKeys <> ["--leave-dust"]) [wikiKeyWif]
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 28080)]
  where
    -- The sweep of the address of the small coins and of the coins of the
    -- keys typed, to the wallet
    sweepWithKeys = ["preparesweep", "--key-file", "-", "--sweep-from", Text.unpack externalAddr2Text, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"]

-- A terminal has no end. signsweep knows the coins of the sweep, and reads
-- lines of keys until each coin has its key. preparesweep cannot know: the
-- keys tell it the coins, so it reads lines until an empty one. The coin
-- of 30000 is at 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK, the address of the
-- key of the wiki.
typedKeysSpec :: Spec
typedKeysSpec =
  describe "The private keys of a sweep, typed on a terminal" $ do
    it "are complete for signsweep once the coin has its key: one line, and no other is asked for" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        run <- hwAtKeyboard [] home ["signsweep", nosig] [(keysPrompt, line wikiKeyWif)]
        runExit run `shouldBe` ExitSuccess
        runErr run `shouldNotSatisfy` Text.isInfixOf "More private keys"
        onePendingState home `shouldReturn` Just (True, False)

    it "are asked for by signsweep on another line while the coin has no key" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        run <- hwAtKeyboard [] home ["signsweep", nosig] [(keysPrompt, line otherKeyWif), ("or Enter to stop: ", line wikiKeyWif)]
        runErr run `shouldSatisfy` Text.isInfixOf "The coin has no key yet. More private keys, or Enter to stop: "
        onePendingState home `shouldReturn` Just (True, False)

    it "sign nothing when the user stops with an empty line before the coin has its key, and signsweep names the coin" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        run <- hwAtKeyboard [] home ["signsweep", nosig] [(keysPrompt, line otherKeyWif), ("or Enter to stop: ", "\n")]
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printedSentence "No private key for input 0 (1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK)"
        onePendingState home `shouldReturn` Just (False, False)

    it "are typed for preparesweep on as many lines as the user likes, counted after each, until an empty line" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <-
          hwAtKeyboard
            []
            home
            ["preparesweep", "--key-file", "-", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"]
            [ (keysPrompt, line otherKeyWif),
              ("1 private key read. More private keys, or Enter to continue: ", line wikiKeyWif),
              ("2 private keys read. More private keys, or Enter to continue: ", "\n")
            ]
        runExit run `shouldBe` ExitSuccess
        (paidToWallet <$> onePendingTx home) `shouldReturn` Map.fromList [(externalAddr 0, 28080)]

    -- A list of keys pasted from another wallet has a key on each line.
    -- The shell would show the second line, run it as a command and keep
    -- it in its history.
    it "leave no line for the shell when the user pasted more lines than signsweep reads" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        (run, left) <- hwThenShell home ["signsweep", nosig] [(keysPrompt, line wikiKeyWif <> line otherKeyWif)]
        runExit run `shouldBe` ExitSuccess
        left `shouldBe` ""

    it "leave no line for the shell when signsweep stops on a line that has no key" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        (run, left) <- hwThenShell home ["signsweep", nosig] [(keysPrompt, "nonsense\n" <> line wikiKeyWif)]
        run `shouldSatisfy` printedSentence "No private key on line 1"
        left `shouldBe` ""

    -- The keys, the empty line that ends them, then a yes, all pasted at
    -- once. The yes must not answer the question about the coins of 600 and
    -- of 900: the user answers no, and the coin of 30000 alone pays 1920
    -- for 192 bytes. With the two small coins, 180 bytes each without their
    -- key, 31500 would pay 5520 for 552 bytes, and 25980 would remain.
    it "leave no line pasted after them to answer the question about the dust coins" $
      withChain smallCoinsChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        run <-
          hwAtKeyboard
            []
            home
            ["preparesweep", "--key-file", "-", "--sweep-from", Text.unpack externalAddr2Text, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"]
            [(keysPrompt, line wikiKeyWif <> "\ny\n"), (dustQuestion, "n\n")]
        runExit run `shouldBe` ExitSuccess
        (paidToWallet <$> onePendingTx home) `shouldReturn` Map.fromList [(externalAddr 0, 28080)]
  where
    -- The end of the first prompt of the keys
    keysPrompt = "separated by spaces): "
    -- A line as the keyboard sends it
    line :: Text -> BS.ByteString
    line text = encodeUtf8 text <> "\n"

-- The best block of the store is nine hours old, so each sweep has the
-- lock time 0: the same command builds the same transaction. The coin of
-- 30000 at 10 satoshis a byte leaves 28080, and at 20 it leaves 26160.
preparedTwiceSpec :: Spec
preparedTwiceSpec =
  describe "A sweep prepared a second time" $ do
    let again home rate = ["preparesweep", "--key-file", home </> "keys.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", rate]
    -- The answer was "The transaction already exists"
    it "is refused when it is the same sweep, naming the pending sweep, signsweep and deletetx" $
      withChainMinedAgo [] (9 * 3600) oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        let short = Text.pack (shortOf nosig)
        (errorOf <$> hwJson home (again home "10") [])
          `shouldReturn` Right ("The wallet holds this transaction already, as the pending transaction " <> short <> ". hw signsweep " <> short <> " signs it, and hw deletetx " <> short <> " deletes it")

    -- pendingtxs shows a signed transaction by its txid
    it "is refused once the first one is signed, naming it by its txid, and broadcast in place of signsweep" $
      withChainMinedAgo [] (9 * 3600) oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        txid <- txidOf =<< txOf =<< answer home ["signsweep", nosig, "--key-file", home </> "keys.txt"] []
        let short = Text.pack (shortOf txid)
        (errorOf <$> hwJson home (again home "10") [])
          `shouldReturn` Right ("The wallet holds this transaction already, as the pending transaction " <> short <> ". hw broadcast " <> short <> " sends it, and hw deletetx " <> short <> " deletes it")

    -- The answer was "A coin referenced by the transaction is locked", and
    -- unlockcoin knows nothing of a coin outside the wallet. The coin is
    -- named in full: the wallet does not hold it, so it has no short id.
    it "is refused at another fee rate, naming the coin, the pending sweep that spends it, and deletetx" $
      withChainMinedAgo [] (9 * 3600) oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        let short = Text.pack (shortOf nosig)
        (errorOf <$> hwJson home (again home "20") [])
          `shouldReturn` Right ("The transaction spends the coin " <> Text.pack (coinArg oldCoin 0) <> ", which the pending transaction " <> short <> " spends already. hw deletetx " <> short <> " deletes that transaction and frees the coin")

    it "is prepared at another fee rate once deletetx deleted the first one" $
      withChainMinedAgo [] (9 * 3600) oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        _ <- answer home ["deletetx", shortOf nosig, "--yes"] []
        prepared <- txOf =<< answer home (again home "20") []
        paidToWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 26160)]

-- pendingtxs lists sweeps and payments together. The mnemonic signs the
-- coins of the wallet, and the private keys sign the coins of a sweep: a
-- secret typed for the wrong one is typed for nothing.
wrongSignerSpec :: Spec
wrongSignerSpec =
  describe "A transaction given to the wrong signing command" $ do
    it "is refused by signtx when it is a pending sweep, before the mnemonic, naming signsweep" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        run <- hw home ["signtx", nosig] seedOnce
        run `shouldSatisfy` printedSentence ("Error: The transaction spends no coin of this wallet, so the mnemonic signs nothing in it. The private keys of its coins sign such a transaction, a sweep: hw signsweep " <> Text.pack nosig)
        run `shouldNotSatisfy` askedMnemonic

    it "is refused by signtx when it is the file of a sweep that pays the wallet, before the mnemonic, naming signsweep --input" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        _ <- answer home ["exporttx", nosig, home </> "sweep.psbt"] []
        run <- hw home ["signtx", "--input", home </> "sweep.psbt", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printedSentence ("a sweep: hw signsweep --input " <> Text.pack (home </> "sweep.psbt"))
        run `shouldNotSatisfy` askedMnemonic

    -- The answer was "No private key for input 0", after the keys
    it "is refused by signsweep when it is a pending payment, before the keys, naming signtx" $
      withChain paidChain $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        run <- hw home ["signsweep", nosig] [wikiKeyWif]
        run `shouldSatisfy` printedSentence ("Error: Every coin the transaction spends is of this wallet: the mnemonic signs them, not private keys. Sign the transaction with hw signtx " <> Text.pack nosig)
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    -- A transaction of two signers: the coin of 50000 of the wallet and
    -- the coin of 30000 of the old key pay 70000, and 10000 of fee
    it "is refused by signtx while a coin from outside the wallet is not signed, before the mnemonic" $
      withHome $ \home -> do
        newTestPortfolio home
        writeTxFile (home </> "both.psbt") bothSigners
        run <- hw home ["signtx", "--input", home </> "both.psbt", "--output", home </> "signed.psbt"] seedOnce
        run `shouldSatisfy` printedSentence "Error: The coin of input 1 (1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK) is not of this wallet, and the input is not signed yet: hw signs the coins of the wallet once the other signers have signed theirs"
        run `shouldNotSatisfy` askedMnemonic
  where
    bothSigners =
      TxSignData (spendingTx [(txHash payment, 0), (txHash oldCoin, 0)] [(otherAddr 0, 70000)]) [payment, oldCoin] [legacyPath (Deriv :/ 0 :/ 0)] [] (Just 10) Nothing [] noLabel

-- The keys are typed hidden, so the user cannot see a typo. A word that
-- did not read as a key was dropped as a label: a mistyped key among good
-- ones ended as "No private key for input 0", as if the key was missing.
unreadKeySpec :: Spec
unreadKeySpec =
  describe "A word of the private keys of a sweep that was meant as a key and is not one" $ do
    -- The key of the wiki with its last character changed, after a key
    -- that reads
    it "stops signsweep, named by its place and its line, with the reason: its checksum is wrong" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        (errorOf <$> hwJson home ["signsweep", nosig] [otherKeyWif <> " KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618"])
          `shouldReturn` Right "Word 2 of line 1 has the form of a private key, but its checksum is wrong, as when a character is mistyped. No key was read"
        onePendingState home `shouldReturn` Just (False, False)

    -- The key of the wiki as a test network writes it
    it "stops signsweep when it is a key of a test network, which is said" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        (errorOf <$> hwJson home ["signsweep", nosig] ["cMzLdeGd5vEqxB8B6VFQoRopQ3sLAAvEzDAoQgvX54xwofSWj1fx"])
          `shouldReturn` Right "Word 1 of line 1 is a private key of a test network, and the keys are read for btc. No key was read"

    it "stops preparesweep too, in a key file, by its line there" $
      withChain oldKeyChain $ \home _ -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        writeFile (home </> "keys.txt") ("# The old wallet\n" <> Text.unpack wikiKeyWif <> "\nKwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618\n")
        (errorOf <$> hwJson home ["preparesweep", "--key-file", home </> "keys.txt", "--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"] [])
          `shouldReturn` Right "Word 1 of line 3 has the form of a private key, but its checksum is wrong, as when a character is mistyped. No key was read"

    it "is not printed back, on the screen or in the error" $
      withChain oldKeyChain $ \home _ -> do
        nosig <- sweepPrepared home
        run <- hw home ["signsweep", nosig] ["KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618"]
        run `shouldNotSatisfy` printed "KwdMAjGmerYanjeui5SHS7JkmpZvVipYvB2LJGU1ZxJwYvP98618"

-- A sweep signs each coin with one private key, in the ways of a legacy
-- address and of a SegWit address of a key. The answer for another
-- address came after the keys and the server, as "Unsupported output
-- script for the coin at ...".
notSweptSpec :: Spec
notSweptSpec =
  describe "An address whose coins a sweep cannot sign" $ do
    let taproot = "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
        refusal = "Error: hw cannot sweep the Taproot address bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0. A sweep takes the coins of legacy, native SegWit and nested SegWit addresses, which one private key signs"
    -- The home has no store: nothing was asked of the server
    it "is refused as --sweep-from when it is a Taproot address, by that name, before the keys and the server" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["preparesweep", "--key-file", "-", "--sweep-from", taproot, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence refusal
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

    it "is refused in the --address-file when it is a Taproot address, among addresses that a sweep takes" $
      withHome $ \home -> do
        newTestPortfolio home
        writeFile (home </> "addresses.txt") (textOf wikiAddr <> "\n" <> taproot <> "\n")
        run <- hw home ["preparesweep", "--address-file", home </> "addresses.txt", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] []
        run `shouldSatisfy` printedSentence refusal

    it "is refused when it is the address of a SegWit script, which one key does not sign" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["preparesweep", "--sweep-from", "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3", "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--output", home </> "sweep.psbt", "--fee-rate", "10"] [])
          `shouldReturn` Right "hw cannot sweep the address bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3, which is not the address of one key. A sweep takes the coins of legacy, native SegWit and nested SegWit addresses, which one private key signs"
