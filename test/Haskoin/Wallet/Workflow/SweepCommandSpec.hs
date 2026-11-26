{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A sweep made with sweep, the command that prepares a sweep, signs it
-- with the private keys of its coins and sends it.
--
-- sweep must do what preparesweep, signsweep and broadcast do when they
-- are run one after the other with the same options. So the sweeps of
-- this spec are made both ways, and each expectation, written out, is
-- held against both ('sweeps'): the two ways agree on every rule tested
-- here, and neither is the measure of the other.
--
-- Then what the one command alone does: the keys read first, the sweep
-- shown once, and the question that is the confirmation; what a sweep
-- that was not signed leaves in the wallet, which is nothing; and what
-- one that was signed and not sent leaves, which is what signsweep leaves
-- of a sweep that pays a portfolio, and nothing of one that pays none.
--
-- The coin of 30000 of the old key is at
-- 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK. With its key, its input counts 148
-- bytes: one input and one output make 10 + 148 + 34 = 192 bytes, a fee
-- of 1920 at 10 satoshis a byte, and 30000 - 1920 = 28080 for the
-- destination.
module Haskoin.Wallet.Workflow.SweepCommandSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.IORef (modifyIORef, readIORef)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Response (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Store (PublishFailure (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  bothWaysSpec
  outsideSpec
  screenSpec
  typedKeysSpec
  pipedKeysSpec
  notSignedSpec
  notSentSpec

-- | Two small coins at the address of the other key,
-- 1BgGZ9tcN4rm9KBzDn7KprQz87SZ26SAMH: 600 and 900 satoshis
smallKeyCoins :: Tx
smallKeyCoins = fundingTx 13 [(otherKeyAddr, 600), (otherKeyAddr, 900)]

-- | A chain whose block 800000 holds the coin of 30000 of the old key and
-- the two small coins of the other key
twoKeysChain :: Chain
twoKeysChain = Chain 800002 [(oldCoin, Just 800000), (smallKeyCoins, Just 800000)]

-- | A home with a portfolio that has handed out its first address,
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj, which the sweeps pay
withFirstAddress :: FilePath -> IO ()
withFirstAddress home = do
  newTestPortfolio home
  _ <- answer home ["receive"] []
  return ()

-- | The sweep of the tests that name none, to the first address of the
-- portfolio at 10 satoshis a byte, as typed after the name of the command
-- and its keys
plain :: [String]
plain = ["--sweep-to", textOf (externalAddr 0), "--fee-rate", "10"]

-- | What the network is handed of the plain sweep: the coin of the old
-- key, and 28080 for the first address of the portfolio
plainHanded :: ([OutPoint], [(Word64, Either String Address)])
plainHanded = ([OutPoint (txHash oldCoin) 0], [(28080, Right (externalAddr 0))])

-- | The coins a transaction spends, sorted, and what it pays, by amount
handed :: Tx -> ([OutPoint], [(Word64, Either String Address)])
handed tx = let (spent, paid) = spendsAndPays tx in (sort spent, paid)

{- The same sweep, both ways -}

-- | A sweep the tests make both ways
data Sweeping = Sweeping
  { -- | What the sweep shows, in the words of its test
    sweepRule :: String,
    -- | The chain the store serves
    sweepChain :: Chain,
    -- | Write the file of the keys in the home, and give its path
    sweepKeys :: FilePath -> IO FilePath,
    -- | The options of the sweep, after its keys
    sweepOptions :: [String],
    -- | What the user types for the questions of those options
    sweepTyped :: [Text],
    -- | What the network must be handed: the coins the transaction spends,
    -- sorted, and what it pays, by amount
    sweepHanded :: ([OutPoint], [(Word64, Either String Address)])
  }

-- | The sweeps made both ways, one for each kind of option of sweep
sweeps :: [Sweeping]
sweeps =
  [ Sweeping
      { sweepRule = "pays the destination what the coin holds, less the fee",
        sweepChain = oldKeyChain,
        sweepKeys = writeKeyFile,
        sweepOptions = plain,
        sweepTyped = [],
        sweepHanded = plainHanded
      },
    -- One input and two outputs: 10 + 148 + 2 × 34 = 226 bytes, a fee of
    -- 2260, and 30000 - 10000 - 2260 = 17740 for the address without an
    -- amount
    Sweeping
      { sweepRule = "pays a destination with an amount that amount, and the rest to the one without",
        sweepChain = oldKeyChain,
        sweepKeys = writeKeyFile,
        sweepOptions = ["--sweep-to", fixedTo (externalAddr 0) "0.0001", "--sweep-to", textOf (otherAddr 0), "--fee-rate", "10"],
        sweepTyped = [],
        sweepHanded = ([OutPoint (txHash oldCoin) 0], [(10000, Right (externalAddr 0)), (17740, Right (otherAddr 0))])
      },
    -- 192 bytes × 20 = 3840 of fee, and 30000 - 3840 = 26160
    Sweeping
      { sweepRule = "pays the fee rate of --fee-rate for each byte",
        sweepChain = oldKeyChain,
        sweepKeys = writeKeyFile,
        sweepOptions = ["--sweep-to", textOf (externalAddr 0), "--fee-rate", "20"],
        sweepTyped = [],
        sweepHanded = ([OutPoint (txHash oldCoin) 0], [(26160, Right (externalAddr 0))])
      },
    -- 192 × 101 = 19392 of fee, and 30000 - 19392 = 10608
    Sweeping
      { sweepRule = "pays a fee rate above 100 once the user answers yes",
        sweepChain = oldKeyChain,
        sweepKeys = writeKeyFile,
        sweepOptions = ["--sweep-to", textOf (externalAddr 0), "--fee-rate", "101"],
        sweepTyped = ["y"],
        sweepHanded = ([OutPoint (txHash oldCoin) 0], [(10608, Right (externalAddr 0))])
      },
    Sweeping
      { sweepRule = "pays a fee rate above 100 without a question, with --yes",
        sweepChain = oldKeyChain,
        sweepKeys = writeKeyFile,
        sweepOptions = ["--sweep-to", textOf (externalAddr 0), "--fee-rate", "101", "--yes"],
        sweepTyped = [],
        sweepHanded = ([OutPoint (txHash oldCoin) 0], [(10608, Right (externalAddr 0))])
      },
    -- The input of a coin costs 148 × 10 = 1480, more than the coins of
    -- 600 and of 900 are worth. They stay, and the coin of 30000 pays
    -- 1920 for 192 bytes.
    Sweeping
      { sweepRule = "leaves the coins worth less than their fee when the user answers no",
        sweepChain = twoKeysChain,
        sweepKeys = writeBothKeysFile,
        sweepOptions = plain,
        sweepTyped = ["n"],
        sweepHanded = plainHanded
      },
    Sweeping
      { sweepRule = "leaves the coins worth less than their fee with --leave-dust",
        sweepChain = twoKeysChain,
        sweepKeys = writeBothKeysFile,
        sweepOptions = plain <> ["--leave-dust"],
        sweepTyped = [],
        sweepHanded = plainHanded
      },
    -- Three inputs and one output: 10 + 3 × 148 + 34 = 488 bytes, a fee
    -- of 4880, and 30000 + 600 + 900 - 4880 = 26620
    Sweeping
      { sweepRule = "sweeps the coins worth less than their fee too, with --spend-dust",
        sweepChain = twoKeysChain,
        sweepKeys = writeBothKeysFile,
        sweepOptions = plain <> ["--spend-dust"],
        sweepTyped = [],
        sweepHanded =
          ( sort [OutPoint (txHash oldCoin) 0, OutPoint (txHash smallKeyCoins) 0, OutPoint (txHash smallKeyCoins) 1],
            [(26620, Right (externalAddr 0))]
          )
      }
  ]

-- | The two ways to sweep, each with how the tests name it
ways :: [(PayWith, String)]
ways = [(WithSend, "sweep"), (WithThreeCommands, "preparesweep, signsweep and broadcast")]

bothWaysSpec :: Spec
bothWaysSpec =
  forM_ ways $ \(way, name) ->
    describe ("A sweep into a portfolio, made with " <> name) $ do
      forM_ sweeps $ \sweep ->
        it sweep.sweepRule $
          withChain sweep.sweepChain $ \home chain -> do
            withFirstAddress home
            keys <- sweep.sweepKeys home
            _ <- sweptWith way home keys sweep.sweepOptions sweep.sweepTyped
            published <- inMempool <$> readIORef chain
            (handed <$> published) `shouldBe` [sweep.sweepHanded]

      it "is answered with the transaction signed and sent" $
        withChain oldKeyChain $ \home _ -> do
          withFirstAddress home
          keys <- writeKeyFile home
          sent <- sweptWith way home keys plain []
          pendingState sent `shouldBe` Just (True, True)

      -- The command that sends syncs the wallet, so the sweep is in the
      -- history at once
      it "is no longer pending once it is sent, with no sync run by the user" $
        withChain oldKeyChain $ \home _ -> do
          withFirstAddress home
          keys <- writeKeyFile home
          _ <- sweptWith way home keys plain []
          (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

      it "is held by the wallet as signed and sent when the store was away for the sync that follows the sending" $
        withChainAwayAfterSending oldKeyChain $ \home _ -> do
          withFirstAddress home
          keys <- writeKeyFile home
          _ <- sweptWith way home keys plain []
          onePendingState home `shouldReturn` Just (True, True)

      it "keeps the label of --label" $
        withChain oldKeyChain $ \home _ -> do
          withFirstAddress home
          keys <- writeKeyFile home
          _ <- sweptWith way home keys (plain <> ["--label", "old paper wallet"]) []
          (txInfoLabel <$> latestTx home) `shouldReturn` "old paper wallet"

      it "gives the portfolio 28080 to spend, once it has a block and a sync has seen it" $
        withChain oldKeyChain $ \home chain -> do
          withFirstAddress home
          keys <- writeKeyFile home
          _ <- sweptWith way home keys plain []
          modifyIORef chain mine
          (spendableOn btc <$> hwJson home ["sync"] []) `shouldReturn` Right [(mainPortfolioRef, 28080)]

{- A sweep that pays no portfolio -}

-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is an address of another wallet. The
-- sweep pays it 28080, on the network of --network.
outsideSpec :: Spec
outsideSpec = do
  let outside = ["--sweep-to", textOf (otherAddr 0), "--network", "btc", "--fee-rate", "10"]
      outsideHanded = ([OutPoint (txHash oldCoin) 0], [(28080, Right (otherAddr 0))])
      noPendingTx home = (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

  describe "A sweep that pays no portfolio of the wallet, made with sweep" $ do
    it "is handed to the network of --network" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        keys <- writeKeyFile home
        _ <- answer home (["sweep", "--key-file", keys] <> outside) ["y"]
        ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [outsideHanded]

    it "is answered with the network and the txid of the transaction sent" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        keys <- writeKeyFile home
        sent <- answer home (["sweep", "--key-file", keys] <> outside) ["y"]
        published <- inMempool <$> readIORef chain
        sent `shouldSatisfy` \case ResponseSentTx net txid _ -> net == btc && [txid] == (txHash <$> published); _ -> False

    it "leaves no pending transaction in the wallet" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        keys <- writeKeyFile home
        _ <- answer home (["sweep", "--key-file", keys] <> outside) ["y"]
        noPendingTx home

    -- hw is then a utility: the database has no wallet
    it "needs no portfolio" $
      withChain oldKeyChain $ \home chain -> do
        keys <- writeKeyFile home
        _ <- answer home (["sweep", "--key-file", keys] <> outside) ["y"]
        ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [outsideHanded]

    -- The keys and the legacy addresses of Bitcoin Cash read the same on
    -- Bitcoin: a guess would sweep the coins of the other chain
    it "is refused without --network, as preparesweep refuses it" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["sweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--fee-rate", "10"] ["y"])
          `shouldReturn` Right "Give the --network of a sweep that pays no portfolio of the wallet: the default-network of the configuration file does not apply to it"

    -- The address is a legacy one, valid on both chains, and the sweep is
    -- on bch: a user who counts in btc means the other chain
    it "is refused a --unit that names the coin of the other chain, and an amount typed in it" $
      withChain oldKeyChain $ \home _ -> do
        keys <- writeKeyFile home
        (errorOf <$> hwJson home ["sweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "bch", "--fee-rate", "10", "--unit", "btc"] ["y"])
          `shouldReturn` Right "The unit btc, given with --unit, is the unit of Bitcoin, and the network is bch: give --unit bch, bit or sat"
        (errorOf <$> hwJson home ["sweep", "--key-file", keys, "--sweep-to", fixedTo (otherAddr 0) "0.0001btc", "--sweep-to", textOf (otherAddr 1), "--network", "bch", "--fee-rate", "10"] ["y"])
          `shouldReturn` Right "The amount 0.0001btc is in btc, the unit of Bitcoin, and the network is bch: give it in bch, bit or sat"

  -- The same sweep through the files of the three commands
  describe "A sweep that pays no portfolio of the wallet, made with preparesweep, signsweep and broadcast" $
    it "hands the network the same transaction" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        keys <- writeKeyFile home
        _ <- answer home (["preparesweep", "--key-file", keys, "--output", home </> "sweep.psbt"] <> outside) []
        _ <- answer home ["signsweep", "--input", home </> "sweep.psbt", "--output", home </> "signed.psbt", "--key-file", keys] []
        _ <- answer home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []
        ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [outsideHanded]

{- What the command shows -}

-- Each test runs the plain sweep with the key in a file, and a yes typed
-- at the question.
screenSpec :: Spec
screenSpec =
  describe "sweep, on the screen" $ do
    let run home = do
          withFirstAddress home
          keys <- writeKeyFile home
          hw home (["sweep", "--key-file", keys] <> plain) ["y"]
        beforeQuestion shown = fst (Text.breakOn "Sign this sweep" (runErr shown))

    it "shows the address swept and its amount before it asks whether to sign and send" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        beforeQuestion shown `shouldSatisfy` Text.isInfixOf "  From     : 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK  0.00030000 btc\n"

    it "shows the destination and what it gets before it asks" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        beforeQuestion shown `shouldSatisfy` Text.isInfixOf "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00028080 btc"

    it "asks whether to sign the sweep and send it, and says that this cannot be undone" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        runErr shown `shouldSatisfy` Text.isInfixOf sweepQuestion

    -- The address swept is on one line of the sweep, and nowhere else
    it "shows the sweep once" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        Text.count "1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK" (runOut shown <> runErr shown) `shouldBe` 1

    -- The one question was the answer
    it "does not ask whether to send a second time, as broadcast does" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        shown `shouldNotSatisfy` printed sendQuestion

    it "answers with the txid of the transaction it sent, not with the sweep again" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        txid <- txidOf =<< latestTx home
        take 1 (Text.lines (runOut shown)) `shouldBe` ["Sent, txid " <> Text.pack txid]

    -- The coin of 28080 is not confirmed yet. The sweep spends a coin
    -- of a key outside the wallet, so the coin waits for a block before
    -- the wallet spends it.
    it "ends with the portfolio and what it holds now, after a blank line" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        drop 1 (Text.lines (runOut shown)) `shouldBe` ["", "portfolio0  btc", "  total 0.00028080 btc, spendable 0.00000000 btc, unspendable 0.00028080 btc, 1 coin"]

    it "names no next step, since it synced the wallet" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        runOut shown `shouldNotSatisfy` Text.isInfixOf "hw sync"

    it "exits as a success" $
      withChain oldKeyChain $ \home _ -> do
        shown <- run home
        runExit shown `shouldBe` ExitSuccess

    -- A sweep that left a coin without a word would read as one that
    -- swept it. The coins of 600 and of 900 stay.
    it "names the coins worth less than their fee that it leaves, before it asks" $
      withChain twoKeysChain $ \home _ -> do
        withFirstAddress home
        keys <- writeBothKeysFile home
        shown <- hw home (["sweep", "--key-file", keys, "--leave-dust"] <> plain) ["y"]
        beforeQuestion shown `shouldSatisfy` Text.isInfixOf "2 coins are worth less than their fee and are not in the sweep:"

    describe "with --yes" $ do
      let runYes home = do
            withFirstAddress home
            keys <- writeKeyFile home
            hw home (["sweep", "--key-file", keys, "--yes"] <> plain) []

      it "does not ask whether to sign and send" $
        withChain oldKeyChain $ \home _ -> do
          shown <- runYes home
          runErr shown `shouldNotSatisfy` Text.isInfixOf "Sign this sweep"

      it "sends the sweep" $
        withChain oldKeyChain $ \home chain -> do
          _ <- runYes home
          ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [plainHanded]

      -- The answer is a txid alone: without the sweep, the user would not
      -- see what was sent
      it "shows the sweep all the same" $
        withChain oldKeyChain $ \home _ -> do
          shown <- runYes home
          runErr shown `shouldSatisfy` Text.isInfixOf "  From     : 1LoVGDgRs9hTfTNJNuXKSpywcbdvwRXpmK  0.00030000 btc\n"

{- The keys, typed on a terminal -}

-- Without --key-file, the keys are typed, as for signsweep. The keys tell
-- the coins, so sweep reads lines of keys until an empty one, as
-- preparesweep --key-file - does.
typedKeysSpec :: Spec
typedKeysSpec =
  describe "The private keys of sweep, typed on a terminal" $ do
    it "are asked for without --key-file, on lines counted after each, until an empty line, and the question comes after them" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        run <-
          hwAtKeyboard
            []
            home
            ("sweep" : plain)
            [ (keysPrompt, line wikiKeyWif),
              ("1 private key read. More private keys, or Enter to continue: ", "\n"),
              (sweepQuestion, "y\n")
            ]
        runExit run `shouldBe` ExitSuccess
        ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [plainHanded]

    -- The key, the empty line that ends the keys, then a yes, all pasted
    -- at once. The yes must not answer the question: the user answers no.
    it "leave no line pasted after them to answer whether to sign and send" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        run <- hwAtKeyboard [] home ("sweep" : plain) [(keysPrompt, line wikiKeyWif <> "\ny\n"), (sweepQuestion, "n\n")]
        runExit run `shouldBe` ExitFailure 1
        (inMempool <$> readIORef chain) `shouldReturn` []

    -- A second key pasted after the empty line is read by nothing. The
    -- shell would show it, run it as a command and keep it in its history.
    it "leave no line for the shell when the user pasted a key that sweep does not read" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        (_, left) <- hwThenShell home ("sweep" : plain) [(keysPrompt, line wikiKeyWif <> "\n" <> line otherKeyWif), (sweepQuestion, "n\n")]
        left `shouldBe` ""
  where
    -- The end of the first prompt of the keys
    keysPrompt = "separated by spaces): "
    -- A line as the keyboard sends it
    line :: Text -> BS.ByteString
    line text = encodeUtf8 text <> "\n"

{- The keys, piped to hw -}

-- A pipe has an end, so the keys are all of it, and no line is left for
-- an answer.
pipedKeysSpec :: Spec
pipedKeysSpec =
  describe "The private keys of sweep, piped to hw" $ do
    it "are read from every line, and the sweep is sent with --yes" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        _ <- answer home (["sweep", "--yes"] <> plain) [otherKeyWif, wikiKeyWif]
        ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [plainHanded]

    it "leave no line to answer whether to sign and send: without --yes, the command is cancelled" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        (errorOf <$> hwJson home ("sweep" : plain) [wikiKeyWif])
          `shouldReturn` Right "Cancelled: the input ended before the answer. In a script, --yes answers this question."

    it "send nothing without --yes" $
      withChain oldKeyChain $ \home chain -> do
        withFirstAddress home
        _ <- hwJson home ("sweep" : plain) [wikiKeyWif]
        (inMempool <$> readIORef chain) `shouldReturn` []

{- A sweep that was not signed -}

-- Nothing is kept of a sweep that was not signed: the wallet has no
-- pending transaction, and the network was handed nothing.
notSignedSpec :: Spec
notSignedSpec = do
  let noPendingTx home = (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
      withKeys home = do
        withFirstAddress home
        keys <- writeKeyFile home
        return (["sweep", "--key-file", keys] <> plain)

  describe "sweep, when the user answers no to whether to sign and send" $ do
    it "says that the sweep was not signed and that nothing was sent" $
      withChain oldKeyChain $ \home _ -> do
        sweep <- withKeys home
        (errorOf <$> hwJson home sweep ["n"]) `shouldReturn` Right "The sweep was not signed, and nothing was sent"

    it "exits as a failure" $
      withChain oldKeyChain $ \home _ -> do
        sweep <- withKeys home
        run <- hw home sweep ["n"]
        runExit run `shouldBe` ExitFailure 1

    it "sends nothing" $
      withChain oldKeyChain $ \home chain -> do
        sweep <- withKeys home
        _ <- hwJson home sweep ["n"]
        (inMempool <$> readIORef chain) `shouldReturn` []

    it "keeps no pending transaction" $
      withChain oldKeyChain $ \home _ -> do
        sweep <- withKeys home
        _ <- hwJson home sweep ["n"]
        noPendingTx home

  -- An empty line is not a yes
  describe "sweep, when the user answers the question with an empty line" $
    it "sends nothing" $
      withChain oldKeyChain $ \home chain -> do
        sweep <- withKeys home
        _ <- hwJson home sweep [""]
        (inMempool <$> readIORef chain) `shouldReturn` []

  describe "sweep at a fee rate above 100 that the user turns down" $ do
    let highFee home = do
          withFirstAddress home
          keys <- writeKeyFile home
          return ["sweep", "--key-file", keys, "--sweep-to", textOf (externalAddr 0), "--fee-rate", "101"]

    it "says that nothing was prepared" $
      withChain oldKeyChain $ \home _ -> do
        sweep <- highFee home
        (errorOf <$> hwJson home sweep ["n"]) `shouldReturn` Right "The transaction was not prepared: give a lower --fee-rate"

    it "keeps no pending transaction" $
      withChain oldKeyChain $ \home _ -> do
        sweep <- highFee home
        _ <- hwJson home sweep ["n"]
        noPendingTx home

  -- The other key has no coin on this chain
  describe "sweep with keys that have no coin" $ do
    it "says that there is nothing to sweep, and asks no question" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        run <- hw home ("sweep" : plain) [otherKeyWif]
        run `shouldSatisfy` printedSentence "There are no coins to sweep in those addresses"
        runErr run `shouldNotSatisfy` Text.isInfixOf "Sign this sweep"

    it "keeps no pending transaction" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        _ <- hwJson home ("sweep" : plain) [otherKeyWif]
        noPendingTx home

  -- The configuration names a store that refuses every connection
  describe "sweep when the store cannot be reached" $ do
    let unreachable home = setConfigKey home "host" "http://127.0.0.1:1/"

    it "says so, and that a sweep cannot be prepared without the server" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        unreachable home
        (errorOf <$> hwJson home ("sweep" : plain) [wikiKeyWif]) `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed. A sweep reads its coins from the server, and cannot be prepared without it"

    -- The advice of the fee rates, "Give a --fee-rate to go on", does not
    -- help a sweep: its coins come from the server
    it "does not advise a --fee-rate when none was given" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        unreachable home
        (errorOf <$> hwJson home ["sweep", "--sweep-to", textOf (externalAddr 0)] [wikiKeyWif]) `shouldReturn` Right "The store at http://127.0.0.1:1/ could not be reached: the connection failed. A sweep reads its coins from the server, and cannot be prepared without it"

    it "does not ask for the private keys, which would be typed for nothing" $
      withChain oldKeyChain $ \home _ -> do
        withFirstAddress home
        unreachable home
        run <- hw home ("sweep" : plain) [wikiKeyWif]
        runErr run `shouldNotSatisfy` Text.isInfixOf "Private keys"

  -- sweep signs at once: a coin without its key could not be signed, and
  -- the keys name the coins
  describe "sweep, which takes the coins of its keys alone" $
    forM_ [("--sweep-from", textOf wikiAddr), ("--address-file", "addresses.txt"), ("--coin", coinArg oldCoin 0), ("--output", "sweep.psbt")] $ \(option, value) ->
      it ("refuses " <> option <> ", an option of preparesweep") $
        withChain oldKeyChain $ \home _ -> do
          withFirstAddress home
          run <- hw home (["sweep", option, value] <> plain) []
          run `shouldSatisfy` printed ("Invalid option `" <> Text.pack option <> "'")

{- A sweep that was signed and not sent -}

-- The store takes no transaction. The command has signed the sweep by
-- then.
notSentSpec :: Spec
notSentSpec = do
  let refusing = withChainNotPublishing (NotPublished "bad-txns-inputs-missingorspent") oldKeyChain
      withKeys home = do
        withFirstAddress home
        keys <- writeKeyFile home
        return (["sweep", "--key-file", keys] <> plain)
      -- What the command adds to the failure of a sending: that the
      -- transaction is signed and kept, and the two commands that take it
      -- from there, each with the short id of its txid, its first 8
      -- digits
      keptNote txid =
        ". The transaction is signed and stays pending: broadcast "
          <> Text.pack (shortOf txid)
          <> " sends it again, and deletetx "
          <> Text.pack (shortOf txid)
          <> " deletes it and frees its coins"

  -- The wallet holds the sweep as signsweep leaves one: signed, and not
  -- online
  describe "sweep into a portfolio, of a transaction the store says no node would take" $ do
    it "fails with what the store said, then with what to do with the signed transaction" $
      refusing $ \home _ -> do
        sweep <- withKeys home
        failure <- errorOf <$> hwJson home sweep ["y"]
        txid <- txidOf =<< onePendingTx home
        failure `shouldSatisfy` either (const False) (Text.isSuffixOf (" refused the transaction: bad-txns-inputs-missingorspent" <> keptNote txid))

    it "exits as a failure" $
      refusing $ \home _ -> do
        sweep <- withKeys home
        run <- hw home sweep ["y"]
        runExit run `shouldBe` ExitFailure 1

    it "keeps the transaction signed and not sent, as signsweep leaves one" $
      refusing $ \home _ -> do
        sweep <- withKeys home
        _ <- hwJson home sweep ["y"]
        onePendingState home `shouldReturn` Just (True, False)

  -- The store takes no transaction the first time it is handed one, and
  -- takes it the second time
  describe "A sweep into a portfolio that sweep signed and could not send" $
    it "is sent by broadcast once the store takes it" $
      withChainNotPublishingAtFirst (PublishFailed "the store has no node") oldKeyChain $ \home chain -> do
        sweep <- withKeys home
        _ <- hwJson home sweep ["y"]
        txid <- txidOf =<< onePendingTx home
        _ <- answer home ["broadcast", shortOf txid, "--yes"] []
        ((handed <$>) . inMempool <$> readIORef chain) `shouldReturn` [plainHanded]

  -- No file and no pending transaction hold a sweep that pays no
  -- portfolio: the user has the keys, and runs the command again
  describe "sweep to an address of no portfolio, of a transaction the store says no node would take" $ do
    let outside home = do
          withFirstAddress home
          keys <- writeKeyFile home
          return ["sweep", "--key-file", keys, "--sweep-to", textOf (otherAddr 0), "--network", "btc", "--fee-rate", "10"]

    it "fails with what the store said, then says that nothing is kept and to run sweep again" $
      refusing $ \home _ -> do
        sweep <- outside home
        failure <- errorOf <$> hwJson home sweep ["y"]
        failure
          `shouldSatisfy` either
            (const False)
            ( Text.isSuffixOf
                " refused the transaction: bad-txns-inputs-missingorspent. The wallet keeps nothing of a sweep that pays none of its portfolios: run sweep again to send it"
            )

    it "keeps no pending transaction" $
      refusing $ \home _ -> do
        sweep <- outside home
        _ <- hwJson home sweep ["y"]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []
