{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The networks hw hides. A portfolio has accounts on eight networks, and
-- the key shown-networks of the configuration file lists those hw shows:
-- btc and bch in a new file. The others are hidden, not disabled.
--
-- What these tests hold hw to:
--
-- * sync and recover still work on a hidden network, so that its funds
--   are there the day the user shows it. Their status line names it,
--   since it tells what the user is waiting for.
-- * Every answer leaves a hidden network out: portfolios, the listings,
--   and what sync and recover say once done, a store that is down
--   included, which is then no failure.
-- * A command cannot name a hidden network with --network, nor work on
--   what the wallet has there alone. The refusal gives the command that
--   shows the network.
-- * setdefault shows and hides networks, and never hides the default one.
--
-- The homes of these tests set the networks they show ('setShownNetworks').
-- Most hide Bitcoin Cash, whose addresses no other network shares, with
-- the coin of 'cashCoinTx' on it: 50000 at the first receiving address of
-- portfolio0 there, bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u.
-- The test networks share their keys, so the tests of an address on
-- several networks hide those. What the shell offers is in
-- "Haskoin.Wallet.Workflow.CommandLineSpec".
module Haskoin.Wallet.Workflow.HiddenNetworksSpec (spec) where

import Control.Monad (void)
import qualified Data.Aeson as Json
import Data.Text (Text)
import qualified Data.Text as Text
import Haskoin
import Haskoin.Wallet.Database.Portfolio (Place (..), accountRefNetwork)
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Directory (removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = prepareContext $ \ctx -> do
  newHomeSpec
  namedSpec
  setDefaultSpec
  answersSpec ctx
  lookedForSpec ctx
  syncSpec ctx
  recoverSpec ctx

-- | A home with portfolio0 of the test wallet, its first receiving
-- addresses on btc and on bch handed out, synced with the chain while it
-- shows both networks, and then hiding bch: it shows btc alone
cashThenHidden :: FilePath -> IO ()
cashThenHidden home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  _ <- answer home ["receive", "--network", "bch"] []
  _ <- answer home ["sync"] []
  setShownNetworks home ["btc"]

{- A new home -}

newHomeSpec :: Spec
newHomeSpec =
  describe "The networks hw shows in a new home" $ do
    -- The home of a test comes with a configuration file. Without it, hw
    -- writes its own, as on a computer it never ran on.
    it "are btc and bch, in the configuration file hw writes" $
      withHome $ \home -> do
        removeFile (home </> ".hw" </> "config.json")
        _ <- hw home ["portfolios"] []
        configKeyOf home "shown-networks" `shouldReturn` Just (Json.toJSON (["btc", "bch"] :: [Text]))

    it "must include the default network: a file that hides it stops hw before it runs, naming the keys to change" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        setDefaultNetwork home (Just "btctest4")
        run <- hw home ["portfolios"] []
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The key \"default-network\" names btctest4, which the key \"shown-networks\" does not list. Add \"btctest4\" to \"shown-networks\", or set \"default-network\" to btc or bch"

{- A hidden network named with --network -}

namedSpec :: Spec
namedSpec =
  describe "A hidden network named with --network" $ do
    -- The home has no portfolio: the refusal comes before receive looks
    -- for one
    it "is refused before the command runs, with the networks hw shows and the command that shows it" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        (errorOf <$> hwJson home ["receive", "--network", "btctest4"] [])
          `shouldReturn` Right "The network btctest4 is hidden. hw shows btc and bch. To show btctest4 too, run: hw setdefault --show-network btctest4"

    it "makes the command exit with 1" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        run <- hw home ["receive", "--network", "btctest4"] []
        runExit run `shouldBe` ExitFailure 1

    -- send pays, so it must not pay on a network the user hid
    it "is refused by send, before it prepares anything or asks for the mnemonic" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        newTestPortfolio home
        (errorOf <$> hwJson home ["send", fixedTo (otherAddr 0) "0.0002", "--network", "btctest4", "--fee-rate", "10"] seedOnce)
          `shouldReturn` Right "The network btctest4 is hidden. hw shows btc and bch. To show btctest4 too, run: hw setdefault --show-network btctest4"

    -- sync works on the hidden networks without the option
    it "is refused by sync too, which names one network with --network" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        newTestPortfolio home
        (errorOf <$> hwJson home ["sync", "--network", "btctest4"] [])
          `shouldReturn` Right "The network btctest4 is hidden. hw shows btc and bch. To show btctest4 too, run: hw setdefault --show-network btctest4"

    it "is taken once setdefault --show-network shows it" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        newTestPortfolio home
        _ <- answer home ["setdefault", "--show-network", "btctest4"] []
        (placeOf <$> hwJson home ["receive", "--network", "btctest4"] []) `shouldReturn` Right (Place mainPortfolioRef btcTest4)

{- setdefault -}

setDefaultSpec :: Spec
setDefaultSpec =
  describe "The networks setdefault shows and hides" $ do
    let showingBtcAndBch home = setShownNetworks home ["btc", "bch"]
        shownIn home = configKeyOf home "shown-networks"
        names = Just . Json.toJSON :: [Text] -> Maybe Json.Value
    -- btcreg is typed first, and comes after btctest4 in a portfolio
    it "are written whole to the key shown-networks, in the order of the networks of a portfolio" $
      withHome $ \home -> do
        showingBtcAndBch home
        _ <- answer home ["setdefault", "--show-network", "btcreg", "--show-network", "btctest4"] []
        shownIn home `shouldReturn` names ["btc", "bch", "btctest4", "btcreg"]

    it "are said whole in the answer, with the file they were written to" $
      withHome $ \home -> do
        showingBtcAndBch home
        run <- hw home ["setdefault", "--show-network", "btctest4"] []
        runOut run `shouldBe` "shown-networks: btc, bch, btctest4\nWritten to " <> Text.pack (home </> ".hw" </> "config.json") <> "\n"

    it "lose the network --hide-network names" $
      withHome $ \home -> do
        showingBtcAndBch home
        _ <- answer home ["setdefault", "--hide-network", "bch"] []
        shownIn home `shouldReturn` names ["btc"]

    it "stay as they are when --show-network names one of them" $
      withHome $ \home -> do
        showingBtcAndBch home
        _ <- answer home ["setdefault", "--show-network", "bch"] []
        shownIn home `shouldReturn` names ["btc", "bch"]

    -- The default network of the home is btc
    it "keep the default network: hiding it is refused, with the command that names another default" $
      withHome $ \home -> do
        showingBtcAndBch home
        (errorOf <$> hwJson home ["setdefault", "--hide-network", "btc"] [])
          `shouldReturn` Right "The network btc is the default network, which hw always shows. To hide it, name another default in the same command, as in: hw setdefault --hide-network btc --network bch"
        shownIn home `shouldReturn` names ["btc", "bch"]

    it "lose the default network when --network names another default in the same command" $
      withHome $ \home -> do
        showingBtcAndBch home
        _ <- answer home ["setdefault", "--hide-network", "btc", "--network", "bch"] []
        shownIn home `shouldReturn` names ["bch"]
        configKeyOf home "default-network" `shouldReturn` Just (Json.String "bch")

    it "do not take a hidden network as the default: it is refused, with the command that shows it too" $
      withHome $ \home -> do
        showingBtcAndBch home
        (errorOf <$> hwJson home ["setdefault", "--network", "btctest4"] [])
          `shouldReturn` Right "The network btctest4 is hidden, and the default network must be one hw shows. To show it and make it the default, run: hw setdefault --show-network btctest4 --network btctest4"
        configKeyOf home "default-network" `shouldReturn` Just (Json.String "btc")

    it "take a hidden network as the default when --show-network shows it in the same command" $
      withHome $ \home -> do
        showingBtcAndBch home
        _ <- answer home ["setdefault", "--show-network", "btctest4", "--network", "btctest4"] []
        shownIn home `shouldReturn` names ["btc", "bch", "btctest4"]
        configKeyOf home "default-network" `shouldReturn` Just (Json.String "btctest4")

    it "cannot gain and lose the same network in one command" $
      withHome $ \home -> do
        showingBtcAndBch home
        (errorOf <$> hwJson home ["setdefault", "--show-network", "btctest4", "--hide-network", "btctest4"] [])
          `shouldReturn` Right "The network btctest4 is given to both --show-network and --hide-network"

{- The answers -}

-- The home synced the coin of 50000 on bch, then hid bch.
answersSpec :: Ctx -> Spec
answersSpec ctx =
  describe "A hidden network that holds funds" $ do
    it "has no line in portfolios" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (networksShownOf <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, ["btc"])]

    it "has no section in coins, which lists none of its coins" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (coinNetworksOf <$> hwJson home ["coins"] []) `shouldReturn` Right []

    -- The portfolio gave out an address on btc and one on bch
    it "has no section in addresses" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (fmap (map ((.name) . accountRefNetwork . fst)) . addressGroupsOf <$> hwJson home ["addresses"] []) `shouldReturn` Right ["btc"]

    it "has no section in txs, which lists none of its transactions" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (txsOf <$> hwJson home ["txs"] []) `shouldReturn` Right []

    -- A payment of 20000 prepared on bch before the network is hidden
    it "has none of its pending transactions in pendingtxs" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        setShownNetworks home ["btc", "bch"]
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", "--fee-rate", "10"] []
        setShownNetworks home ["btc"]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "shows its funds again once the network is shown" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        _ <- answer home ["setdefault", "--show-network", "bch"] []
        (spendableOn bch <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]

{- What the user names -}

lookedForSpec :: Ctx -> Spec
lookedForSpec ctx =
  describe "What the user names that the wallet has on a hidden network alone" $ do
    let showBch = "To show bch, run: hw setdefault --show-network bch"
    it "is an address refused, with its network and the command that shows it" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (errorOf <$> hwJson home ["show", "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"] [])
          `shouldReturn` Right ("The address bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u is on bch, which is hidden. " <> showBch)

    it "is a coin refused, with its network and the command that shows it" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (errorOf <$> hwJson home ["lockcoin", coinArg (cashCoinTx ctx) 0] [])
          `shouldReturn` Right ("The coin " <> Text.pack (coinArg (cashCoinTx ctx) 0) <> " is on bch, which is hidden. " <> showBch)

    it "is a transaction refused, with its network and the command that shows it" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        (errorOf <$> hwJson home ["show", txidArg (cashCoinTx ctx)] [])
          `shouldReturn` Right ("The transaction " <> Text.pack (txidArg (cashCoinTx ctx)) <> " is on bch, which is hidden. " <> showBch)

    -- A payment prepared on bch before the network is hidden: signtx
    -- refuses it before it asks for the mnemonic
    it "is a pending transaction refused, with its network and the command that shows it" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        cashThenHidden home
        setShownNetworks home ["btc", "bch"]
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--network", "bch", "--fee-rate", "10"] []
        setShownNetworks home ["btc"]
        (errorOf <$> hwJson home ["signtx", nosig] [])
          `shouldReturn` Right ("The pending transaction " <> Text.pack nosig <> " is on bch, which is hidden. " <> showBch)

    -- btctest and btctest4 share their keys: the address was handed out
    -- on both, and both are hidden
    it "is an address two hidden test networks share, refused with both named and the command that shows the first" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        setShownNetworks home ["btc", "bch"]
        (errorOf <$> hwJson home ["show", "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"] [])
          `shouldReturn` Right "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, which are hidden. To show one of them, run for instance: hw setdefault --show-network btctest"

    -- With both shown, show asks for --network. With btctest hidden, the
    -- address is on one network as far as hw shows: btctest4.
    it "is found on the one shown network among those that share it, without --network" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive", "--network", "btctest"] []
        _ <- answer home ["receive", "--network", "btctest4"] []
        setShownNetworks home ["btc", "bch", "btctest4"]
        (placeOf <$> hwJson home ["show", "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"] []) `shouldReturn` Right (Place mainPortfolioRef btcTest4)

{- sync -}

-- The homes show btc alone, and never handed out an address on bch.
syncSpec :: Ctx -> Spec
syncSpec ctx =
  describe "sync with a hidden network" $ do
    let showingBtc home = do
          newTestPortfolio home
          setShownNetworks home ["btc"]
    -- The sync finds the address paid on bch by itself, within the gap
    it "still syncs it: its funds are there once the network is shown, with no other sync" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        showingBtc home
        _ <- answer home ["sync"] []
        setShownNetworks home ["btc", "bch"]
        (spendableOn bch <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]

    it "gives no sync of it in its answer" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        showingBtc home
        (fmap (map ((.name) . placeNetwork . fst)) . syncedOf <$> hwJson home ["sync"] []) `shouldReturn` Right ["btc"]

    it "names it on the status line of a terminal, which tells what the user is waiting for" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        showingBtc home
        run <- hwOnTerminal home ["sync"] []
        runErr run `shouldSatisfy` Text.isInfixOf "\rportfolio0 bch: checking the store\ESC[0K"

    it "does not name it as not synced when its store is down" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        showingBtc home
        (notSyncedOf <$> hwJson home ["sync"] []) `shouldReturn` Right []

    -- A store that is down on a shown network makes sync exit with 1
    it "exits with 0 when its store is down, and the shown networks are synced" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        showingBtc home
        run <- hw home ["sync"] []
        runExit run `shouldBe` ExitSuccess

    -- The host serves btc alone: the seven other networks are hidden
    it "does not name it among the networks the host does not serve" $
      withChainServing ctx [btc] [] (paidChain ctx) $ \home _ -> do
        showingBtc home
        (notServedOf <$> hwJson home ["sync"] []) `shouldReturn` Right []

{- recover -}

-- The homes show btc alone, and have no wallet before recover.
recoverSpec :: Ctx -> Spec
recoverSpec ctx =
  describe "recover with a hidden network" $ do
    -- portfolio0 of the test mnemonic was paid on bch, and nowhere else
    it "still searches it: a portfolio that was paid there alone is found" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        setShownNetworks home ["btc"]
        (recoveredOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right (Just 0, [0])

    it "names the shown networks alone as searched" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        setShownNetworks home ["btc"]
        (searchedOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right ["btc"]

    it "gives no sync of it in its answer" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        setShownNetworks home ["btc"]
        (fmap (map ((.name) . placeNetwork . fst)) . syncedOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right ["btc"]

    it "does not name it as left out when its store is down" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        setShownNetworks home ["btc"]
        (leftOutOf <$> hwJson home ["recover"] seedOnce) `shouldReturn` Right []

    it "exits with 0 when its store is down, and the shown networks are searched" $
      withChainDownOn ctx [bch] (paidChain ctx) $ \home _ -> do
        setShownNetworks home ["btc"]
        run <- hw home ["recover"] seedOnce
        runExit run `shouldBe` ExitSuccess

    -- The funds are kept in the database all the same
    it "leaves what it found there for the day the network is shown" $
      withChain ctx (confirmedChain (cashCoinTx ctx)) $ \home _ -> do
        setShownNetworks home ["btc"]
        void $ answer home ["recover"] seedOnce
        setShownNetworks home ["btc", "bch"]
        (spendableOn bch <$> hwJson home ["portfolios"] []) `shouldReturn` Right [(mainPortfolioRef, 50000)]
