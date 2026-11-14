{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What show shows of a portfolio, from the word typed: an address by its
-- path, derived as an offline computer checks it, and cut when the wallet
-- does not hold it ("Haskoin.Wallet.CutAddress"), the newest receiving
-- address without a word, a transaction by either of its hashes, a coin,
-- a transaction file by its name, a file of a wallet, as importwallet
-- would take it, and a database file, with the wallets and the portfolios
-- it holds. Then what show refuses. The addresses typed in full, and
-- the QR codes show draws, are with the other screens in
-- "Haskoin.Wallet.Workflow.OutputSpec". What a file holds, and how show
-- reads the files of other wallets, is in
-- "Haskoin.Wallet.Workflow.SigningSpec".
--
-- A path and the newest address are of the active portfolio, which
-- switch changes, and of the account --network and --type pick in it. A
-- transaction is shown as each portfolio that holds it sees it, whichever
-- is active, and an address as the portfolio that holds it. On the
-- screen, a view of another portfolio than the active one says so.
-- --portfolio names one portfolio of a file of a wallet, and
-- goes with nothing else. An address and a coin tell where they are, but the
-- test networks share their keys: the wallet may hold the same address,
-- and the same coin, on two of them, and --network then says which.
module Haskoin.Wallet.Workflow.ShowSpec (spec) where

import Control.Monad (forM_, replicateM_)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), Place (..), PortfolioRef (..))
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..), TxType (..))
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), PortfolioShown (..), WalletShown (..))
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Keys (AccountKind (..))
import Haskoin.Wallet.Migration.TestUtils (version2Database)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Tx.SignData (TxSignData (..), noLabel)
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  pathSpec
  newestSpec
  txSpec ctx
  coinSpec ctx
  addressSpec
  testNetworksSpec ctx
  fileSpec ctx
  walletFileSpec
  databaseFileSpec
  refusedSpec

{- An address by its path -}

pathSpec :: Spec
pathSpec =
  describe "An address shown by its path" $ do
    -- The signer never ran receive: it derives the address from the key.
    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 shows its first 12 characters and
    -- its last 12.
    it "is the one receive gave out online at the path receive showed, cut on the signer that never handed it out" $
      withHome $ \signer -> withHome $ \online -> do
        newTestPortfolio signer
        _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
        _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
        replicateM_ 3 $ answer online ["receive"] []
        (addressesOf <$> hwJson online ["receive"] []) `shouldReturn` Right [("1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1", "")]
        (cutAddressOf <$> hwJson signer ["show", "/0/3"] []) `shouldReturn` Right "1yQZuJjA6w7h.........Cv22rKCas7F1"

    -- The ends of 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, the change address
    -- /1/0 of the bip44 account on btc
    it "is a change address for a path on the change chain, before the portfolio spends" $
      withHome $ \home -> do
        newTestPortfolio home
        (cutAddressOf <$> hwJson home ["show", "/1/0"] []) `shouldReturn` Right "17KiDLpE3r92..........tgHqEVJrznvn"

    -- portfolio0 of wallet0 is named without its wallet
    it "is shown cut under its path, then its portfolio, network and type, when the wallet does not hold it" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["show", "/0/3"] []
        take 2 (Text.lines (runOut run))
          `shouldBe` [ "bip44/0/3  1yQZuJjA6w7h.........Cv22rKCas7F1",
                       "  Account  : portfolio0  btc  bip44"
                     ]

    it "comes cut with a note on how to check it, and to give out the address as receive showed it" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["show", "/0/3"] []
        run
          `shouldSatisfy` printedSentence
            "Compare both ends with the address that receive gave out at bip44/0/3 on the online computer. If they match, that address is of this account, and you can give it out. If they differ, the online computer may have been tampered with, and you should not give the address out. Nobody can make another address with the same ends. This screen is for checking only, so the middle of the address is hidden. Give out the address as receive showed it."

    it "is cut in JSON too, which holds nowhere the whole address, so that no script pays it" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["show", "/0/3", "--json"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1")

    it "is shown as receive gave it out, with its label, when the wallet holds it" $
      withHome $ \home -> do
        newTestPortfolio home
        given <- hwJson home ["receive", "--label", "rent"] []
        hwJson home ["show", "/0/0"] [] `shouldReturn` given

    -- The ends of 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, the first receiving
    -- address of the bip44 account of portfolio1 on btc, m/44'/0'/1'
    it "is of the portfolio switch made active" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        (cutAddressOf <$> hwJson home ["show", "/0/0"] []) `shouldReturn` Right "17Hc4H2HwzLf..........QPGpNEvK2NUo"

    -- The ends of 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, the first receiving
    -- address of the bip44 account of portfolio0 on btc
    it "is of the active portfolio, portfolio0, the first one created, in a wallet of several portfolios" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        (cutAddressOf <$> hwJson home ["show", "/0/0"] []) `shouldReturn` Right "1KEn7jEXa7KC..........qRJBLnPMmrLj"

    -- hw stores the address /0/0 of the bip44 account on Bitcoin Cash as
    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u. Cut, it
    -- shows bitcoincash:q and 13 characters, then the last 14.
    it "is of the account of the --network named, written as that network writes it" $
      withHome $ \home -> do
        newTestPortfolio home
        (cutAddressOf <$> hwJson home ["show", "/0/0", "--network", "bch"] []) `shouldReturn` Right "bitcoincash:qzvzk6v4gdxt70..............pfzercknlz3k4u"

    -- The ends of bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc, the first
    -- receiving address of the bip84 account on btc, m/84'/0'/0': its
    -- first 17 characters and its last 13
    it "is of the account of the --type named, bip84 on btc" $
      withHome $ \home -> do
        newTestPortfolio home
        (cutAddressOf <$> hwJson home ["show", "/0/0", "--type", "bip84"] []) `shouldReturn` Right "bc1qynnywxcvs9dwu............eyprgqeacxwtc"

    -- The ends of bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc again, though
    -- the default type is bip44: the path names its account
    it "is of the account of the type the path starts with, as the views write it" $
      withHome $ \home -> do
        newTestPortfolio home
        (cutAddressOf <$> hwJson home ["show", "bip84/0/0"] []) `shouldReturn` Right "bc1qynnywxcvs9dwu............eyprgqeacxwtc"

    it "is refused for a type hw holds no account of, naming the types it holds" $
      withHome $ \home -> do
        newTestPortfolio home
        run <- hw home ["show", "bip49/0/3"] []
        runExit run `shouldBe` ExitFailure 1
        runErr run `shouldSatisfy` Text.isInfixOf "The path bip49/0/3 is of no type of account hw holds: a path starts with bip44 or bip84, such as bip84/0/3, or with no type, such as /0/3"

    it "hands out nothing: the wallet has no address after it, and receive starts at the first" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["show", "/0/5"] []
        _ <- answer home ["show", "/1/5"] []
        (addressesOf <$> hwJson home ["addresses"] []) `shouldReturn` Right []
        (addressesOf <$> hwJson home ["addresses", "--change"] []) `shouldReturn` Right []
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 0, "")]

    -- A third chain, a hardened step, one step, three steps, no leading
    -- slash, a master key, and the first hardened index: each has a slash,
    -- which no address, hash or coin has
    it "is refused for a path that is not that of an address, with the two forms" $
      withHome $ \home -> do
        newTestPortfolio home
        forM_ ["/2/0", "/0/3'", "/0", "/0/3/1", "0/3", "m/0/3", "/0/2147483648"] $ \path -> do
          run <- hw home ["show", path] []
          runExit run `shouldBe` ExitFailure 1
          runErr run `shouldSatisfy` Text.isInfixOf ("The path " <> Text.pack path <> " is not that of an address: /0/i is the receiving address i, and /1/i the change address i")

{- The newest receiving address -}

newestSpec :: Spec
newestSpec =
  describe "The newest receiving address, shown without a word" $ do
    it "is the one receive gave out last, with its label" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--label", "rent"] []
        (addressesOf <$> hwJson home ["show"] []) `shouldReturn` Right [(externalAddrsText !! 1, "rent")]

    it "hands out no new address" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        _ <- answer home ["show"] []
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 1, "")]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- the bip44 account of portfolio1 on btc
    it "is that of the portfolio switch made active" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        _ <- answer home ["receive"] []
        switchTo home 0 1
        _ <- answer home ["receive"] []
        (addressesOf <$> hwJson home ["show"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "")]

    -- receive gave out an address of the bip44 account only
    it "is that of the account --type picks, which may have given out none" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["show", "--type", "bip84"] []) `shouldReturn` Right "The account has no receiving address yet: receive gives out the first one"

    it "is refused while the account has given out none" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["show"] []) `shouldReturn` Right "The account has no receiving address yet: receive gives out the first one"

{- A transaction -}

txSpec :: Ctx -> Spec
txSpec ctx =
  describe "A transaction shown by its hash" $ do
    -- The paid chain pays 50000 to the first receiving address of the bip44
    -- account of portfolio0 on btc
    it "is a transaction of the history by its txid, as its portfolio sees it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (heldTxsOf <$> hwJson home ["show", txidArg (payment ctx)] []) `shouldReturn` Right [(mainPlace, 50000)]

    it "is found by the short id of its txid, its first 8 digits" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (heldTxsOf <$> hwJson home ["show", shortOf (txidArg (payment ctx))] []) `shouldReturn` Right [(mainPlace, 50000)]

    -- The payment spends the coin of 50000, pays 20000 and gets 27740 back
    -- as change: 50000 - 27740 = 22260 leave the portfolio, 20000 paid and
    -- 2260 of fee
    it "is a pending transaction by its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (heldTxsOf <$> hwJson home ["show", nosig] []) `shouldReturn` Right [(mainPlace, -22260)]

    it "is a pending transaction by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        (heldTxsOf <$> hwJson home ["show", txid] []) `shouldReturn` Right [(mainPlace, -22260)]

    -- The payment pays 50000 to portfolio0 and 30000 to portfolio1
    it "is shown once for each portfolio that holds it, with what it pays each" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        (heldTxsOf <$> hwJson home ["show", txidArg (twoPortfoliosTx ctx)] []) `shouldReturn` Right [(mainPlace, 50000), (portfolio1Place, 30000)]

    it "is shown once for each portfolio that holds it, whichever is active" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        (heldTxsOf <$> hwJson home ["show", txidArg (twoPortfoliosTx ctx)] []) `shouldReturn` Right [(mainPlace, 50000), (portfolio1Place, 30000)]

    -- portfolio1 is active, so the view of portfolio0 says it is not
    it "says on the screen which of its views is of another portfolio than the active one" $
      withChain ctx (confirmedChain (twoPortfoliosTx ctx)) $ \home _ -> do
        syncedTwoPortfolios home
        switchTo home 0 1
        run <- hw home ["show", txidArg (twoPortfoliosTx ctx)] []
        filter (Text.isPrefixOf "  Portfolio") (Text.lines (runOut run))
          `shouldBe` [ "  Portfolio: portfolio0, not the active portfolio1",
                       "  Portfolio: portfolio1"
                     ]

    -- The payment pays portfolio0 and portfolio1 of wallet0, and portfolio0
    -- of wallet1, the wallet of the second test mnemonic, 20000 at
    -- 1ExqWGU8LFmemQzHVq6DszGDa3j2K1CsR2. receive gives out the first
    -- address of each while it is active.
    it "is shown once for each portfolio that holds it, of every wallet" $
      withChain ctx (confirmedChain (twoWalletsTx ctx)) $ \home _ -> do
        newTestPortfolio home
        newTestPortfolio home
        newPortfolioOf home testMnemonic2
        forM_ [(0, 0), (0, 1), (1, 0)] $ \(wallet, portfolio) -> do
          switchTo home wallet portfolio
          answer home ["receive"] []
        _ <- answer home ["sync"] []
        (heldTxsOf <$> hwJson home ["show", txidArg (twoWalletsTx ctx)] [])
          `shouldReturn` Right [(mainPlace, 50000), (portfolio1Place, 30000), (Place (PortfolioRef 1 0) btc, 20000)]

    it "is refused when the wallet does not have it, by its short id as by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["show", "ab000000"] []) `shouldReturn` Right "The wallet has no transaction ab000000"
        (errorOf <$> hwJson home ["show", "ab00000000000000000000000000000000000000000000000000000000000000"] [])
          `shouldReturn` Right "The wallet has no transaction ab00000000000000000000000000000000000000000000000000000000000000"

-- | A payment from another wallet to portfolio0 and portfolio1 of the test
-- wallet, 50000 and 30000, and to portfolio0 of the second test wallet,
-- 20000
twoWalletsTx :: Ctx -> Tx
twoWalletsTx ctx = fundingTx ctx 4 [(externalAddr 0, 50000), (account1Addr, 30000), (externalAddr2, 20000)]

{- A coin -}

coinSpec :: Ctx -> Spec
coinSpec ctx =
  describe "A coin shown by TXID:INDEX" $ do
    -- The coin of 50000 of the paid chain, three blocks deep
    it "is the coin as coins lists it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (coinsShown <$> hwJson home ["show", coinArg (payment ctx) 0] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Nothing, True)]

    it "is found by the short id of its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (coinsShown <$> hwJson home ["show", shortCoinArg (payment ctx) 0] []) `shouldReturn` Right [(OutPoint (txHash (payment ctx)) 0, 50000, 3, Nothing, True)]

    -- The wallet holds this one coin, so coins --verbose lists it alone.
    -- --network keeps the listing to the one place of the coin, which it
    -- shows without a title. The coin is at the first receiving address of
    -- the bip44 account of portfolio0 on btc.
    it "is shown with every detail, as coins --verbose shows it on its network, then with the account that holds it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        verbose <- hw home ["coins", "--verbose", "--network", "btc"] []
        shown <- hw home ["show", coinArg (payment ctx) 0] []
        Text.lines (runOut shown) `shouldBe` Text.lines (runOut verbose) <> ["  Account : portfolio0  btc  bip44"]

    it "is refused when the wallet does not hold it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["show", "ab000000:0"] []) `shouldReturn` Right "The wallet has no coin ab000000:0"

{- An address by its text -}

addressSpec :: Spec
addressSpec =
  describe "An address shown by its text" $ do
    -- The address is the first receiving address of the bip84 account of
    -- portfolio1 on btc, m/84'/0'/1'/0/0, which receive gave out while
    -- portfolio1 was active. portfolio0 is active when it is shown.
    it "is shown with the account that holds it, of whatever portfolio and type" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        addr <- receiveWith home ["--type", "bip84"]
        switchTo home 0 0
        (accountRefOf <$> hwJson home ["show", addr] []) `shouldReturn` Right (AccountRef portfolio1Place Bip84)

    -- The same address of portfolio1, shown while portfolio0 is active
    it "says on the screen that its account is not of the active portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        addr <- receiveWith home ["--type", "bip84"]
        switchTo home 0 0
        run <- hw home ["show", addr] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Account     : portfolio1, not the active portfolio0  btc  bip84\n"

{- An address and a coin on two test networks -}

testNetworksSpec :: Ctx -> Spec
testNetworksSpec ctx =
  describe "An address or a coin the wallet has on two test networks" $ do
    it "is refused without --network when the default network has none of them, naming the networks" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        (errorOf <$> hwJson home ["show", Text.unpack testnetExternalAddrText] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4: give the --network"

    it "is the address of the --network named" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        (placeOf <$> hwJson home ["show", Text.unpack testnetExternalAddrText, "--network", "btctest4"] []) `shouldReturn` Right (Place mainPortfolioRef btcTest4)

    it "is refused on a --network that has no row of it, naming those that have" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        (errorOf <$> hwJson home ["show", Text.unpack testnetExternalAddrText, "--network", "btcreg"] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, not on the --network btcreg"

    it "is a coin refused without --network when the default network has none of it, naming the networks" $
      withChainServing ctx [btcTest, btcTest4] [] (confirmedChain (testnetCoinTx ctx)) $ \home _ -> do
        testnetCoinSynced home
        (errorOf <$> hwJson home ["show", coinArg (testnetCoinTx ctx) 0] [])
          `shouldReturn` Right ("The coin " <> Text.pack (coinArg (testnetCoinTx ctx) 0) <> " is on btctest and btctest4: give the --network")

    it "is the coin of the --network named" $
      withChainServing ctx [btcTest, btcTest4] [] (confirmedChain (testnetCoinTx ctx)) $ \home _ -> do
        testnetCoinSynced home
        (coinNetworksOf <$> hwJson home ["show", coinArg (testnetCoinTx ctx) 0, "--network", "btctest4"] [])
          `shouldReturn` Right [("btctest4", OutPoint (txHash (testnetCoinTx ctx)) 0)]

{- A transaction file -}

-- | A payment of another wallet, unsigned, that pays 20000 to the first
-- receiving address of the bip44 account of portfolio0 on btc and 10000
-- to that of portfolio1, 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, from a coin
-- of 50000 of its own: it spends from no portfolio of the test wallet
twoPortfoliosPaidFile :: Ctx -> TxSignData
twoPortfoliosPaidFile ctx =
  TxSignData (spendingTx ctx [(txHash funding, 0)] [(externalAddr 0, 20000), (account1Addr, 10000), (otherAddr 1, 19000)]) [funding] [] [] Nothing Nothing [] noLabel
  where
    funding = fundingTx ctx 6 [(otherAddr 0, 50000)]

-- | A home with portfolio0 and portfolio1 of the test wallet, each of
-- which gave out its first receiving address while it was active, and
-- the file of 'twoPortfoliosPaidFile' in it: its path. portfolio0 is
-- active.
withTwoPortfoliosPaidFile :: Ctx -> FilePath -> IO FilePath
withTwoPortfoliosPaidFile ctx home = do
  newTestPortfolio home
  newTestPortfolio home
  forM_ [0, 1] $ \portfolio -> do
    switchTo home 0 portfolio
    answer home ["receive"] []
  switchTo home 0 0
  writeTxFile ctx (home </> "incoming.psbt") (twoPortfoliosPaidFile ctx)
  return (home </> "incoming.psbt")

fileSpec :: Ctx -> Spec
fileSpec ctx =
  describe "A transaction file shown by its name" $ do
    -- hw runs in the home. The payment of preparetx is from portfolio0.
    it "is read from the directory the command runs in, when its name has no directory" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", home </> "unsigned.psbt"] []
        (placeOf <$> hwJson home ["show", "unsigned.psbt"] []) `shouldReturn` Right mainPlace

    -- The bare name is taken for the short id of a transaction the wallet
    -- does not have
    it "is read with its directory when its name looks like a short id" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--output", home </> "a1075db5"] []
        (placeOf <$> hwJson home ["show", "./a1075db5"] []) `shouldReturn` Right mainPlace
        (errorOf <$> hwJson home ["show", "a1075db5"] []) `shouldReturn` Right "The wallet has no transaction a1075db5"

    -- portfolio0 pays 20000 of its coin of 50000 to the first receiving
    -- address of portfolio1, with 2260 of fee: 22260 leave it. The signer
    -- holds both portfolios, and not the payment. It gave out the address
    -- of portfolio1 the payment pays: a database knows an address of its
    -- portfolios once it has handed it out.
    it "is shown as the portfolio that pays sees it, when it moves funds between two portfolios" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        twoPortfoliosSigner signer
        (seenBy <$> hwJson signer ["show", online </> "transfer.psbt"] []) `shouldReturn` Right (mainPlace, TxDebit, -22260)

    -- The portfolio that pays locks its coins and signs
    it "is shown as the portfolio that pays sees it, though the portfolio it pays is active" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        twoPortfoliosSigner signer
        switchTo signer 0 1
        (seenBy <$> hwJson signer ["show", online </> "transfer.psbt"] []) `shouldReturn` Right (mainPlace, TxDebit, -22260)

    -- Its paths and its labels are of portfolio0, which pays it
    it "says on the screen that the portfolio that pays is not the active one, when the portfolio it pays is active" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        twoPortfoliosSigner signer
        switchTo signer 0 1
        run <- hw signer ["show", online </> "transfer.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0, not the active portfolio1\n"

    it "names no other portfolio on the screen when the portfolio that pays is active" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        twoPortfoliosSigner signer
        run <- hw signer ["show", online </> "transfer.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  Portfolio: portfolio0\n"

    -- The address paid is the first receiving address of the bip44
    -- account of portfolio1, which the signer gave out. No file says so:
    -- the database of the signer does.
    it "names the portfolio of the address it pays after the path of that address, on the screen" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        twoPortfoliosSigner signer
        run <- hw signer ["show", online </> "transfer.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00020000 btc  bip44/0/0 of portfolio1\n"

    -- The coin spent is at the first receiving address of portfolio0
    it "names the portfolio that pays after the paths of its own addresses, when the portfolio it pays is active" $
      withHome $ \signer -> withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        twoPortfoliosSigner signer
        switchTo signer 0 1
        run <- hw signer ["show", online </> "transfer.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  From     : 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0 of portfolio0\n"

    -- The online computer holds portfolio0 alone, so the address is one
    -- of another wallet to it: it has no path there
    it "shows the address it pays without a path on a computer that does not hold the portfolio of that address" $
      withChain ctx (paidChain ctx) $ \online _ -> do
        transferFile online
        run <- hw online ["show", online </> "transfer.psbt"] []
        runOut run `shouldSatisfy` Text.isInfixOf "\n  To       : 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo  0.00020000 btc\n"

    -- The user who switched to a portfolio means that one
    it "is shown as the active portfolio sees it, when it pays several portfolios and spends from none, the active one among them" $
      withHome $ \home -> do
        file <- withTwoPortfoliosPaidFile ctx home
        switchTo home 0 1
        (seenBy <$> hwJson home ["show", file] []) `shouldReturn` Right (portfolio1Place, TxCredit, 10000)

    -- portfolio0 of wallet1, the wallet of the second test mnemonic, is
    -- active
    it "is refused when it pays several portfolios and spends from none, the active one none of them, naming them and saying to switch" $
      withHome $ \home -> do
        file <- withTwoPortfoliosPaidFile ctx home
        newPortfolioOf home testMnemonic2
        switchTo home 1 0
        (errorOf <$> hwJson home ["show", file] [])
          `shouldReturn` Right "The transaction pays several portfolios, portfolio0 of wallet0 on btc and portfolio1 of wallet0 on btc: switch to the one to use first"
  where
    -- The online computer holds portfolio0 alone: it pays an address of
    -- portfolio1 as any other
    transferFile online = do
      synced online
      _ <- answer online ["preparetx", fixedTo account1Addr "0.0002", "--fee-rate", "10", "--output", online </> "transfer.psbt"] []
      return ()
    twoPortfoliosSigner signer = do
      newTestPortfolio signer
      newTestPortfolio signer
      switchTo signer 0 1
      _ <- answer signer ["receive"] []
      switchTo signer 0 0
    -- Where show shows the transaction, and what it is to that portfolio
    seenBy = \case
      ResponseTx place info _ -> Right (place, summaryType (txInfoSummary info), summaryAmount (txInfoSummary info))
      other -> Left (show other)

{- A file of a wallet -}

-- | A file of a wallet as exportwallet writes it for portfolio0 of the test
-- wallet: the keys of its twelve accounts, an address of its bip44 account
-- on btc labelled Wages, and a transaction there labelled Salary
mainWalletFile :: Text.Text
mainWalletFile = Text.unlines (portfolio0KeyRecords <> mainLabelRecords)

-- | The same file with the key of the bip44 account on btc alone, one
-- account of the twelve of portfolio0: a part of a portfolio, which
-- importwallet does not create
mainAccountFile :: Text.Text
mainAccountFile = Text.unlines (take 1 portfolio0KeyRecords <> mainLabelRecords)

-- | The records of the keys of the twelve accounts of portfolio0 of the
-- test wallet, as exportwallet writes them, bip44 first on each network.
-- The test networks share the keys of their paths, m/44'/1'/0' and
-- m/84'/1'/0'.
portfolio0KeyRecords :: [Text.Text]
portfolio0KeyRecords =
  [ record testAccountXPub "pkh([bc560f95/44'/0'/0'])" "btc" "44",
    record testSegwitAccountXPub "wpkh([bc560f95/84'/0'/0'])" "btc" "84",
    record testAccountXPubBch "pkh([bc560f95/44'/145'/0'])" "bch" "44",
    record testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest" "44",
    record testSegwitAccountXPubTest "wpkh([bc560f95/84'/1'/0'])" "btctest" "84",
    record testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btctest4" "44",
    record testSegwitAccountXPubTest "wpkh([bc560f95/84'/1'/0'])" "btctest4" "84",
    record testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "btcreg" "44",
    record testSegwitAccountXPubTest "wpkh([bc560f95/84'/1'/0'])" "btcreg" "84",
    record testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "bchtest" "44",
    record testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "bchtest4" "44",
    record testAccountXPubTest "pkh([bc560f95/44'/1'/0'])" "bchreg" "44"
  ]
  where
    record key origin net purpose =
      "{\"type\":\"xpub\",\"ref\":\"" <> key <> "\",\"origin\":\"" <> origin <> "\",\"network\":\"" <> net <> "\",\"purpose\":" <> purpose <> "}"

-- | The labels of 'mainWalletFile', of its bip44 account on btc: an
-- address labelled Wages and a transaction labelled Salary
mainLabelRecords :: [Text.Text]
mainLabelRecords =
  [ "{\"type\":\"addr\",\"ref\":\"1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj\",\"label\":\"Wages\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\",\"keypath\":\"/0/0\"}",
    "{\"type\":\"tx\",\"ref\":\"0000000000000000000000000000000000000000000000000000000000000001\",\"label\":\"Salary\",\"origin\":\"pkh([bc560f95/44'/0'/0'])\",\"network\":\"btc\"}"
  ]

walletFileSpec :: Spec
walletFileSpec =
  describe "A file of a wallet shown by its name" $ do
    -- A copy of the database holds wallets and portfolios too, and a
    -- backup has one file of each kind under the same name
    it "says first that it is a BIP 329 file, with its name, so that it is not taken for a database" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        run <- hw home ["show", "wallet.jsonl"] []
        take 3 (Text.lines (runOut run))
          `shouldBe` [ "BIP 329 file  wallet.jsonl",
                       "  A file of a wallet, shown as importwallet would take it",
                       ""
                     ]

    -- The same records under the name of a copy of the database
    it "is told by what it holds, not by its name: one named as a database is still shown as a BIP 329 file" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "wallet.sqlite") mainWalletFile
        run <- hw home ["show", "wallet.sqlite"] []
        take 1 (Text.lines (runOut run)) `shouldBe` ["BIP 329 file  wallet.sqlite"]

    -- The file names the twelve accounts of portfolio0, and labels in one
    -- of them. The three lines of the heading come first.
    it "is shown wallet by wallet and account by account, as importwallet would take each, with the labels of the file under its account" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        run <- hw home ["show", "wallet.jsonl"] []
        Text.unlines (drop 3 (Text.lines (runOut run)))
          `shouldBe` Text.unlines
            [ "wallet0  bc560f95  not in the database, and importwallet creates it",
              "  portfolio0  importwallet creates it, 12 accounts to add",
              "",
              "portfolio0  btc  bip44",
              "  Address labels",
              "    bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  \"Wages\"",
              "  Transaction labels",
              "    0000000000000000000000000000000000000000000000000000000000000001  \"Salary\""
            ]

    it "leaves the database without the wallet and the portfolio it shows" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        _ <- answer home ["show", "wallet.jsonl"] []
        (portfolioRefsOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right []

    -- importwallet creates a portfolio only whole
    it "is refused when it gives a part of a portfolio the database does not have, as importwallet refuses it" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "account.jsonl") mainAccountFile
        (errorOf <$> hwJson home ["show", "account.jsonl"] [])
          `shouldReturn` Right "The file gives 1 of the 12 accounts of portfolio0 of the wallet bc560f95, which the database does not have: importwallet creates a portfolio only whole, with every account. Write the file with exportwallet on the computer that has the whole portfolio"

    it "says of a portfolio the database holds whole that importwallet adds nothing to it" $
      withHome $ \home -> do
        newTestPortfolio home
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        run <- hw home ["show", "wallet.jsonl"] []
        run `shouldSatisfy` printed "wallet0  bc560f95  in the database\n  portfolio0  nothing to add\n"

    it "is refused with --network, since each record of the file names its network" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        (errorOf <$> hwJson home ["show", "wallet.jsonl", "--network", "btc"] [])
          `shouldReturn` Right "show takes no --network with a file of a wallet: each of its records names its network"

    it "is refused with --portfolio of a portfolio the file has nothing of, naming those it has" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        (errorOf <$> hwJson home ["show", "wallet.jsonl", "--portfolio", "1"] [])
          `shouldReturn` Right "The file has no portfolio1: it has portfolio0"

    -- A key alone, as another wallet writes it, is one account, of no
    -- portfolio
    it "is refused when it holds a key alone and no record" $
      withHome $ \home -> do
        TextIO.writeFile (home </> "key.txt") (testAccountXPub <> "\n")
        (errorOf <$> hwJson home ["show", "key.txt"] [])
          `shouldReturn` Right "The file key.txt could not be read: Neither a transaction file, a PSBT in bytes or in base64 text or the hexadecimal of a transaction, nor a file of a wallet that importwallet reads, nor an SQLite database"

{- A database file -}

-- Each home has portfolio0 of the test wallet, whose seed has the
-- fingerprint bc560f95. backup copies its database to copy.sqlite in the
-- home, where hw runs.
databaseFileSpec :: Spec
databaseFileSpec =
  describe "A database file shown by its name" $ do
    it "is shown with each wallet it holds, by its index and the fingerprint of its seed, and the portfolios of each" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["backup", "copy.sqlite"] []
        (walletsShownOf <$> hwJson home ["show", "copy.sqlite"] [])
          `shouldReturn` Right [(0, "bc560f95", [0])]

    -- A file of a wallet holds wallets and portfolios too, and a backup
    -- has one file of each kind under the same name
    it "says first that it is an SQLite database, with its name, its format version and what it holds, so that it is not taken for a BIP 329 file" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["backup", "copy.sqlite"] []
        run <- hw home ["show", "copy.sqlite"] []
        take 3 (Text.lines (runOut run))
          `shouldBe` [ "SQLite database  copy.sqlite",
                       "  A database of hw at format version 5, with 1 wallet and 1 portfolio",
                       ""
                     ]

    -- The copy is made with the name of a file of labels
    it "is told by what it holds, not by its name: one named as a file of labels is still shown as an SQLite database" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["backup", "labels.jsonl"] []
        run <- hw home ["show", "labels.jsonl"] []
        take 1 (Text.lines (runOut run)) `shouldBe` ["SQLite database  labels.jsonl"]

    -- portfolio1 is made after the copy
    it "is shown as it was copied, without the portfolio the database got since" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["backup", "copy.sqlite"] []
        newTestPortfolio home
        (walletsShownOf <$> hwJson home ["show", "copy.sqlite"] [])
          `shouldReturn` Right [(0, "bc560f95", [0])]

    -- The second home has no wallet, and gets none
    it "is shown from a home that holds none of its wallets, and leaves that home without a wallet" $
      withHome $ \home -> withHome $ \other -> do
        newTestPortfolio home
        _ <- answer home ["backup", "copy.sqlite"] []
        _ <- answer other ["show", home </> "copy.sqlite"] []
        (portfolioRefsOf <$> hwJson other ["portfolios"] []) `shouldReturn` Right []

    it "marks the active portfolio of the database it holds, the one switch made active before the copy" $
      withHome $ \home -> do
        newTestPortfolio home
        newTestPortfolio home
        switchTo home 0 1
        _ <- answer home ["backup", "copy.sqlite"] []
        (activeShownOf <$> hwJson home ["show", "copy.sqlite"] [])
          `shouldReturn` Right [(0, False), (1, True)]

    it "is refused with --network, since its wallets are on every network" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["backup", "copy.sqlite"] []
        (errorOf <$> hwJson home ["show", "copy.sqlite", "--network", "btc"] [])
          `shouldReturn` Right "show takes no --network with copy.sqlite, which is an SQLite database: its wallets and their portfolios are on every network"

    -- The two options name a wallet and a portfolio of a file of a
    -- wallet. The refusal says which kind of file this one is.
    it "is refused with --wallet or --portfolio, which go with a BIP 329 file of a wallet" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["backup", "copy.sqlite"] []
        forM_ [["--wallet", "bc560f95"], ["--portfolio", "0"]] $ \option ->
          (errorOf <$> hwJson home (["show", "copy.sqlite"] <> option) [])
            `shouldReturn` Right "show takes no --wallet and no --portfolio with copy.sqlite, which is an SQLite database. They name one wallet and one portfolio of a BIP 329 file of a wallet. Every wallet and every portfolio of a database is shown"

    -- A database of hw 0.9, which this hw does not open either
    it "is refused when it is at another format version, with its version, and hw exits with a failure" $
      withHome $ \home -> do
        inDatabaseFile (home </> "old.sqlite") version2Database
        run <- hw home ["show", "old.sqlite"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printedSentence "The SQLite database old.sqlite cannot be shown: it is a database of hw at format version 2, made by an earlier hw, and show reads version 5 only"
  where
    -- The wallets of the file, each by its index, the fingerprint of its
    -- seed and the indexes of its portfolios
    walletsShownOf (ResponseDatabaseFile _ shown) =
      Right [(walletShownIndex w, fingerprintToText (walletShownFingerprint w), portfolioShownIndex <$> walletShownPortfolios w) | w <- databaseShownWallets shown]
    walletsShownOf other = Left (show other)
    -- The portfolios of the file, each by its index and whether it is
    -- marked as the active one
    activeShownOf (ResponseDatabaseFile _ shown) =
      Right [(portfolioShownIndex p, portfolioShownActive p) | w <- databaseShownWallets shown, p <- walletShownPortfolios w]
    activeShownOf other = Left (show other)

{- What show refuses -}

refusedSpec :: Spec
refusedSpec =
  describe "show" $ do
    -- An address, a coin, a path, and the newest address without a word
    it "refuses --portfolio with anything but a file of a wallet, where it names a portfolio of the file" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        forM_ [["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"], ["ab000000:0"], ["/0/0"], []] $ \shown ->
          (errorOf <$> hwJson home (["show"] <> shown <> ["--portfolio", "0"]) [])
            `shouldReturn` Right "show takes --wallet and --portfolio only with a file of a wallet, where they name one wallet and one portfolio of the file. The rest is shown in the active portfolio, or where it is"

    -- An address, a coin, a path, and the newest address without a word
    it "refuses --wallet with anything but a file of a wallet, where it names a wallet of the file" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        forM_ [["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"], ["ab000000:0"], ["/0/0"], []] $ \shown ->
          (errorOf <$> hwJson home (["show"] <> shown <> ["--wallet", "bc560f95"]) [])
            `shouldReturn` Right "show takes --wallet and --portfolio only with a file of a wallet, where they name one wallet and one portfolio of the file. The rest is shown in the active portfolio, or where it is"

    it "refuses --type with an address, which is of the type of its account" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--type", "bip44"] [])
          `shouldReturn` Right "show takes no --type with an address"

    -- The path names the type of its account already
    it "refuses --type with a path that starts with its type" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["show", "bip84/0/0", "--type", "bip84"] [])
          `shouldReturn` Right "show takes no --type with a path that starts with its type"

    it "refuses --type with a coin, which is of the type of its account" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["show", "ab000000:0", "--type", "bip44"] [])
          `shouldReturn` Right "show takes no --type with a coin"

    it "refuses --type with a file, which tells the types of its accounts" $
      withHome $ \home -> do
        newTestPortfolio home
        TextIO.writeFile (home </> "wallet.jsonl") mainWalletFile
        (errorOf <$> hwJson home ["show", "wallet.jsonl", "--type", "bip44"] [])
          `shouldReturn` Right "show takes no --type with a file"

    -- The JSON file of a transaction of an earlier hw is such a file
    it "refuses a file that is none of the three kinds of file it shows, naming the three" $
      withHome $ \home -> do
        newTestPortfolio home
        let file = home </> "earlier.json"
        writeFile file "{\"tx\": \"0100000000000000000000\", \"txinputs\": []}\n"
        (errorOf <$> hwJson home ["show", file] [])
          `shouldReturn` Right ("The file " <> Text.pack file <> " could not be read: Neither a transaction file, a PSBT in bytes or in base64 text or the hexadecimal of a transaction, nor a file of a wallet that importwallet reads, nor an SQLite database")

    -- A short id with its last digit missing, which no file of the home is
    -- named
    it "names the five things it shows for a word that is none of them and names no file" $
      withHome $ \home -> do
        newTestPortfolio home
        (errorOf <$> hwJson home ["show", "a1075db"] [])
          `shouldReturn` Right "a1075db is not a path, an address, a transaction or a coin, and no file has that name: show takes a path such as /0/3, an address of the wallet, a txid or a nosigHash, a coin TXID:INDEX, or a file of a transaction, of a wallet or of a database, each hash in full or by its short id, its first 8 digits"
