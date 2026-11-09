{-# LANGUAGE OverloadedStrings #-}

-- | What show shows of the wallet, from the word typed: an address by its
-- path, derived as an offline computer checks it, and cut when the wallet
-- does not hold it ("Haskoin.Wallet.CutAddress"), the newest receiving
-- address without a word, a transaction by either of its hashes, and a
-- coin. Then what show refuses. The addresses typed in full, and the QR
-- codes show draws, are with the other screens in
-- "Haskoin.Wallet.Workflow.OutputSpec".
module Haskoin.Wallet.Workflow.ShowSpec (spec) where

import Control.Monad (forM_, replicateM_)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
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
        createMain signer
        _ <- answer signer ["exportacc", signer </> "main.json"] []
        _ <- answer online ["importacc", signer </> "main.json"] []
        replicateM_ 3 $ answer online ["receive"] []
        (addressesOf <$> hwJson online ["receive"] []) `shouldReturn` Right [("1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1", "")]
        (cutAddressOf <$> hwJson signer ["show", "/0/3"] []) `shouldReturn` Right "1yQZuJjA6w7h.........Cv22rKCas7F1"

    -- The ends of 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, the change address
    -- /1/0 of main
    it "is a change address for a path on the change chain, before the account spends" $
      withHome $ \home -> do
        createMain home
        (cutAddressOf <$> hwJson home ["show", "/1/0"] []) `shouldReturn` Right "17KiDLpE3r92..........tgHqEVJrznvn"

    it "is shown cut under its path, then its account and network, when the wallet does not hold it" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["show", "/0/3"] []
        take 3 (Text.lines (runOut run))
          `shouldBe` [ "/0/3  1yQZuJjA6w7h.........Cv22rKCas7F1",
                       "  Account  : main",
                       "  Network  : btc"
                     ]

    it "comes cut with a note on how to check it, and to give out the address as receive showed it" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["show", "/0/3"] []
        run
          `shouldSatisfy` printedSentence
            "Compare both ends with the address that receive gave out at /0/3 on the online computer. If they match, that address is of this account, and you can give it out. If they differ, the online computer may have been tampered with, and you should not give the address out. Nobody can make another address with the same ends. This screen is for checking only, so the middle of the address is hidden. Give out the address as receive showed it."

    it "is cut in JSON too, which holds nowhere the whole address, so that no script pays it" $
      withHome $ \home -> do
        createMain home
        run <- hw home ["show", "/0/3", "--json"] []
        runOut run `shouldSatisfy` (not . Text.isInfixOf "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1")

    it "is shown as receive gave it out, with its label, when the wallet holds it" $
      withHome $ \home -> do
        createMain home
        given <- hwJson home ["receive", "--label", "rent"] []
        hwJson home ["show", "/0/0"] [] `shouldReturn` given

    -- The ends of 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo, the first receiving
    -- address of second, account 1 of the seed
    it "is of the --account named, which a wallet of several accounts needs" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        (cutAddressOf <$> hwJson home ["show", "/0/0", "--account", "second"] []) `shouldReturn` Right "17Hc4H2HwzLf..........QPGpNEvK2NUo"
        (errorOf <$> hwJson home ["show", "/0/0"] []) `shouldReturn` Right "Specify which account to use"

    -- hw stores the address /0/0 of cash, the Bitcoin Cash account of the
    -- test wallet, as bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u.
    -- Cut, it shows bitcoincash:q and 13 characters, then the last 14.
    it "is written as the network of the account writes it" $
      withHome $ \home -> do
        _ <- answer home ["createacc", "cash", "--network", "bch"] seedTwice
        (cutAddressOf <$> hwJson home ["show", "/0/0"] []) `shouldReturn` Right "bitcoincash:qzvzk6v4gdxt70..............pfzercknlz3k4u"

    it "hands out nothing: the wallet has no address after it, and receive starts at the first" $
      withHome $ \home -> do
        createMain home
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
        createMain home
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
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["receive", "--label", "rent"] []
        (addressesOf <$> hwJson home ["show"] []) `shouldReturn` Right [(externalAddrsText !! 1, "rent")]

    it "hands out no new address" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        _ <- answer home ["show"] []
        (addressesOf <$> hwJson home ["receive"] []) `shouldReturn` Right [(externalAddrsText !! 1, "")]

    -- 17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo is the first receiving address of
    -- second, account 1 of the seed
    it "is that of the --account named" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["createacc", "second"] seedTwice
        _ <- answer home ["receive", "--account", "main"] []
        _ <- answer home ["receive", "--account", "second"] []
        (addressesOf <$> hwJson home ["show", "--account", "second"] []) `shouldReturn` Right [("17Hc4H2HwzLfgmr7zYqrhXQPGpNEvK2NUo", "")]

    it "is refused while the account has given out none" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["show"] []) `shouldReturn` Right "The account has no receiving address yet: receive gives out the first one"

{- A transaction -}

txSpec :: Ctx -> Spec
txSpec ctx =
  describe "A transaction shown by its hash" $ do
    -- The paid chain pays 50000 to the first receiving address of main
    it "is a transaction of the history by its txid, as its account sees it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (heldTxsOf <$> hwJson home ["show", txidArg (payment ctx)] []) `shouldReturn` Right [("main", 50000)]

    it "is found by the short id of its txid, its first 8 digits" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (heldTxsOf <$> hwJson home ["show", shortOf (txidArg (payment ctx))] []) `shouldReturn` Right [("main", 50000)]

    -- The payment spends the coin of 50000, pays 20000 and gets 27740 back
    -- as change: 50000 - 27740 = 22260 leave the account, 20000 paid and
    -- 2260 of fee
    it "is a pending transaction by its nosigHash" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (heldTxsOf <$> hwJson home ["show", nosig] []) `shouldReturn` Right [("main", -22260)]

    it "is a pending transaction by its txid once signed" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        _ <- paymentSigned home
        txid <- txidOf =<< onePendingTx home
        (heldTxsOf <$> hwJson home ["show", txid] []) `shouldReturn` Right [("main", -22260)]

    -- The payment pays 50000 to the first receiving address of main and
    -- 30000 to the first receiving address of second
    it "is shown once for each account that holds it, with what it pays each" $
      withChain ctx (twoAccountsChain ctx) $ \home _ -> do
        twoAccountsSynced home
        (heldTxsOf <$> hwJson home ["show", txidArg (twoAccountsPayment ctx)] []) `shouldReturn` Right [("main", 50000), ("second", 30000)]

    it "is shown as the --account named sees it, and as that account only" $
      withChain ctx (twoAccountsChain ctx) $ \home _ -> do
        twoAccountsSynced home
        (heldTxsOf <$> hwJson home ["show", txidArg (twoAccountsPayment ctx), "--account", "second"] []) `shouldReturn` Right [("second", 30000)]

    it "is refused with an --account that does not hold it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        _ <- answer home ["createacc", "second"] seedTwice
        (errorOf <$> hwJson home ["show", txidArg (payment ctx), "--account", "second"] [])
          `shouldReturn` Right ("The transaction " <> Text.pack (txidArg (payment ctx)) <> " is not in the account second")

    it "is refused when the wallet does not have it, by its short id as by its txid" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["show", "ab000000"] []) `shouldReturn` Right "The wallet has no transaction ab000000"
        (errorOf <$> hwJson home ["show", "ab00000000000000000000000000000000000000000000000000000000000000"] [])
          `shouldReturn` Right "The wallet has no transaction ab00000000000000000000000000000000000000000000000000000000000000"

-- | A payment from another wallet of 50000 to the first receiving address
-- of main and 30000 to the first receiving address of second
twoAccountsPayment :: Ctx -> Tx
twoAccountsPayment ctx = fundingTx ctx 3 [(externalAddr 0, 50000), (account1Addr, 30000)]

-- | A chain whose block 800000 holds that payment, two blocks below its
-- best
twoAccountsChain :: Ctx -> Chain
twoAccountsChain = confirmedChain . twoAccountsPayment

-- | A home with the accounts main and second, each with its first
-- receiving address given out, synced with the chain of the store
twoAccountsSynced :: FilePath -> IO ()
twoAccountsSynced home = do
  createMain home
  _ <- answer home ["createacc", "second"] seedTwice
  _ <- answer home ["receive", "--account", "main"] []
  _ <- answer home ["receive", "--account", "second"] []
  _ <- answer home ["sync"] []
  return ()

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

    -- The wallet holds this one coin, so coins --verbose lists it alone
    it "is shown with every detail, as coins --verbose shows it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        verbose <- hw home ["coins", "--verbose"] []
        shown <- hw home ["show", coinArg (payment ctx) 0] []
        runOut shown `shouldBe` runOut verbose

    it "is refused when the wallet does not hold it" $
      withChain ctx (paidChain ctx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["show", "ab000000:0"] []) `shouldReturn` Right "The wallet has no coin ab000000:0"

{- What show refuses -}

refusedSpec :: Spec
refusedSpec =
  describe "show" $ do
    it "refuses --account with an address, which tells its account" $
      withHome $ \home -> do
        createMain home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["show", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "--account", "main"] [])
          `shouldReturn` Right "An address tells its account, so show takes no --account with it"

    it "refuses --account with a coin, which tells its account" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["show", "ab000000:0", "--account", "main"] [])
          `shouldReturn` Right "A coin tells its account, so show takes no --account with it"

    -- A short id with its last digit missing
    it "names the four things it shows for a word that is none of them" $
      withHome $ \home -> do
        createMain home
        (errorOf <$> hwJson home ["show", "a1075db"] [])
          `shouldReturn` Right "a1075db is not a path, an address, a transaction or a coin: show takes a path such as /0/3, an address of the wallet, a txid or a nosigHash, or a coin TXID:INDEX, each hash in full or by its short id, its first 8 digits"
