{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Live regtest chains for the tests of @Haskoin.Wallet.Regtest@: a
-- Bitcoin Core and a Bitcoin Cash Node, a haskoin-store for each, and the
-- proxy that serves both stores under one host, in one Podman container of
-- the image that @regtest\/build.sh services@ builds. The tests run the hw
-- of the build against that host, and drive the two nodes.
--
-- The fake stores of the other tests answer what a test tells them to.
-- That is how a store that lies is tested, and a failure at one step of a
-- sync. It is also their limit: a fake answers as its author thought a
-- store answers. haskoin-store 1.11 began to refuse a transaction that no
-- node would take, the fakes went on taking every one, and a sync that
-- failed for good on such a transaction passed every test. Here the store
-- is the real one, and so are the nodes behind it: every signature hw
-- makes is checked by a node, and a reorganisation is one a node made.
--
-- The tests are skipped, each marked pending with the reason, when Podman
-- cannot run containers here, when the image is not built, when it was
-- built from another recipe than the one in @regtest\/@, or when
-- @HW_NO_REGTEST@ is set. Nothing is downloaded or built by a test.
--
-- A regtest chain has rules that cost a timeout each to find. They are in
-- @regtest\/README.md@, and the functions here keep them:
--
-- * a chain that replaces another must be longer, since the store keeps
--   the chain it saw first until another has more work ('takeOff');
-- * the store learns of a block and of a transaction a moment after its
--   node, and its health check lags its index, so every change waits for
--   the store ('waitSynced');
-- * the wallet of a node puts its own transactions back in its mempool at
--   start, so a transaction a node must forget pays an address outside it
--   ('forgetMempool', 'outsideAddress').
--
-- Every test starts from two empty chains ('onChains'). The clock of each
-- node is set, and every block has a time that follows from its height
-- alone, so that heights, times and amounts are the same on every run.
-- The clock starts on 1 January 2026, which is long ago for hw: it then
-- takes a lock time of 0 with @--yes@, where it would otherwise take an
-- earlier block one time in ten. What still varies is the id of a payment
-- with change, since hw shuffles its outputs. No test states one.
module Haskoin.Wallet.RegtestUtils
  ( -- * The chains
    Regtest,
    Chain (..),
    networkOf,
    withRegtest,
    onChains,
    regtestHost,

    -- * Histories
    withPaidWallet,
    withTwoComputers,
    withPayer,
    Received (..),
    withReceived,
    replaceWithRival,
    firstChainBack,
    withSignerAndOnline,
    withManyPayments,
    withCoinsOfKey,

    -- * A wallet on the chains
    withHomeOn,
    withWallet,
    recoverWith,
    receiveOn,
    syncNow,
    pay,
    payWithSend,
    payAll,
    prepared,
    signedBy,
    sent,
    replacedWith,
    payOffline,
    sweep,
    keyFileIn,
    txsOn,
    coinsOn,
    spendableOf,
    pendingOf,
    deadOn,
    notPublishedOn,
    conflictedOn,
    sentAgainOn,
    notSyncedOn,
    errorOf,

    -- * The nodes
    rpc,
    heightOf,
    mempoolOf,
    minerAddress,
    outsideAddress,
    otherOutsideAddress,
    outsideKey,
    outsideKeyUncompressed,
    outsideKeyLegacy,
    outsideKeyLegacyUncompressed,
    outsideKeySegwit,
    outsideKeyNested,
    outsideScriptAddress,
    outsideCashScriptAddress,
    outsideSegwitScriptAddress,
    outsideTaprootAddress,
    nodeFeeOf,
    lockTimeOf,
    rawTxOf,
    sendRawTx,
    mine,
    mineNow,
    mineTo,
    mineLeavingMempool,
    payFromNode,
    takeOff,
    giveBack,
    forgetMempool,
    forgetStoreMempool,
    restartNode,
    startAgain,
    waitSynced,

    -- * The services
    stopService,
    startService,
    startStoreWith,
    waitForPeers,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, try)
import Control.Monad (forM, forM_, unless, void, when)
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Char (isDigit, isSpace)
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (dropWhileEnd, isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import Haskoin (Network (..), txHashToHex)
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database.TxInfo (ChainStatus (..), ListedTx (..), PendingStatus (..), TxInfo (..), TxStatus (..), TxSummary (..), TxType, pendingShownHash, pendingSigned)
import Haskoin.Wallet.HwTestUtils (hwJson, setConfigKey, setDefaultNetwork, setShownNetworks, withHome)
import Haskoin.Wallet.Database.Portfolio (Place (..))
import Haskoin.Wallet.Keys (MnemonicPass)
import Haskoin.Wallet.Sync (SyncRes (..))
import Haskoin.Wallet.TestUtils (testMnemonic, testMnemonic2)
import Haskoin.Wallet.Workflow.TestUtils (answer, coinsShown, newPortfolioOf, nosigOf, receiveWith, seedOnceOf, txOf, txidOf)
import Network.HTTP.Client (HttpException, Manager, ManagerSettings (managerIdleConnectionCount), RequestBody (RequestBodyLBS), applyBasicAuth, defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types (statusCode)
import Numeric.Natural (Natural)
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist, listDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (getCurrentPid, readProcessWithExitCode)
import System.Timeout (timeout)
import Test.Hspec (Expectation, pendingWith)

{- The chains -}

-- | One of the two chains of the container
data Chain
  = -- | The regtest chain of Bitcoin Core, which hw calls btcreg
    Btc
  | -- | The regtest chain of Bitcoin Cash Node, which hw calls bchreg
    Bch
  deriving (Eq, Show)

-- | The name hw gives the network of a chain, as @--network@ takes it
networkOf :: Chain -> String
networkOf Btc = "btcreg"
networkOf Bch = "bchreg"

-- | The name of a chain in the commands of the container
chainName :: Chain -> String
chainName Btc = "btc"
chainName Bch = "bch"

-- | The running container, and how to reach it from here
data Regtest = Regtest
  { -- | The name of the container, for @podman exec@
    regtestContainer :: String,
    -- | The host of the two stores, as the configuration of hw takes it
    regtestHost :: String,
    -- | The JSON-RPC address of the node of each chain
    regtestRpc :: Chain -> String,
    -- | The connections to the nodes and to the stores
    regtestManager :: Manager,
    -- | The time the clock of each node is set to, to set it again after
    -- the node starts again
    regtestClock :: IORef [(Chain, Integer)],
    -- | The chains whose node had blocks taken off since the store was
    -- last waited for ('waitSynced')
    regtestUnsettled :: IORef [Chain]
  }

-- | The image the tests run, unless @HW_REGTEST_IMAGE@ names another
defaultImage :: String
defaultImage = "localhost/hw-regtest-services:latest"

-- | The label on every container the tests start. Its value is the
-- process of the test run, so that a later run removes the containers of
-- one that was killed.
testLabel :: String
testLabel = "hw-regtest-test"

-- | Run the tests of a spec with the chains, or with the reason there are
-- none. One container serves every test of the spec, and is removed when
-- they end. Used with 'Test.Hspec.aroundAll'.
withRegtest :: (Either String Regtest -> IO ()) -> IO ()
withRegtest tests = do
  usable <- regtestImage
  case usable of
    Left why -> tests (Left why)
    Right image -> do
      removeStale
      bracket (try (startContainer image)) (either (const (return ())) stopContainer) $ \started ->
        tests $ case started of
          Left failure -> Left ("The regtest container did not start: " <> show (failure :: IOError))
          Right chains -> Right chains

-- | Run one test on two empty chains, with the clock of both nodes at its
-- start, or mark the test pending with the reason there are no chains
onChains :: (Regtest -> IO ()) -> Either String Regtest -> Expectation
onChains _ (Left why) = pendingWith why
onChains test (Right chains) = startAgain chains >> test chains

-- | Start both chains again from their first block, with the clock of
-- both nodes at its start. Every test begins with this. A test also calls
-- it to put a new chain under a wallet that has a history.
startAgain :: Regtest -> IO ()
startAgain chains = do
  _ <- inContainer chains ["regtest-ctl", "reset"]
  writeIORef (regtestClock chains) []
  writeIORef (regtestUnsettled chains) []
  forM_ [Btc, Bch] $ \chain -> setClock chains chain epoch

-- | The image to run, or why the tests cannot run: a variable set to skip
-- them, a Podman that cannot run containers, an image that is not built,
-- or one built from another recipe than the one of this source tree
regtestImage :: IO (Either String String)
regtestImage = do
  skip <- lookupEnv "HW_NO_REGTEST"
  image <- fromMaybe defaultImage <$> lookupEnv "HW_REGTEST_IMAGE"
  case skip of
    Just set | set `notElem` ["", "0"] -> return (Left "HW_NO_REGTEST is set")
    _ -> do
      info <- try (timeout (60 * 1000000) (podman ["info", "--format", "{{.Host.Arch}}"]))
      case info of
        Left failure -> return (Left ("Podman is not available: " <> show (failure :: IOError)))
        Right Nothing -> return (Left "podman info did not answer within a minute")
        Right (Just (ExitFailure _, _, err)) -> return (Left ("Podman cannot run containers: " <> trim err))
        Right (Just (ExitSuccess, _, _)) -> do
          (exists, _, _) <- podman ["image", "exists", image]
          if exists /= ExitSuccess
            then return (Left ("The image " <> image <> " is not built: run regtest/build.sh services"))
            else do
              current <- recipeHash
              (_, built, _) <- podman ["image", "inspect", "--format", "{{index .Labels \"org.haskoin.regtest.recipe\"}}", image]
              return $ case current of
                Just hash | trim built /= hash -> Left ("The image " <> image <> " was built from another recipe than the one in regtest/: run regtest/build.sh services")
                _ -> Right image

-- | The hash of the recipe of the image in this source tree, as
-- @regtest\/build.sh@ makes it: of the Containerfile, then of every file
-- that goes into the image, in the order of their paths. None when the
-- tests do not run from the root of the source tree.
recipeHash :: IO (Maybe String)
recipeHash = do
  here <- doesFileExist ("regtest" </> "Containerfile")
  if not here
    then return Nothing
    else do
      files <- sort <$> filesUnder ("regtest" </> "rootfs")
      contents <- mapM BS.readFile (("regtest" </> "Containerfile") : files)
      return . Just . take 16 $ show (Crypto.hashWith Crypto.SHA256 (BS.concat contents))

-- | Every file under a folder, with the folder in its path
filesUnder :: FilePath -> IO [FilePath]
filesUnder folder = do
  names <- listDirectory folder
  fmap concat . forM names $ \name -> do
    let path = folder </> name
    isFolder <- doesDirectoryExist path
    if isFolder then filesUnder path else return [path]

-- | A number that tells the containers of one test run apart
containerCount :: IORef Int
containerCount = unsafePerformIO (newIORef 0)
{-# NOINLINE containerCount #-}

-- | Start a container of the image, wait until its chains are ready, and
-- read the ports Podman gave it. The chains are in a tmpfs, gone with the
-- container. The ports are random ones of the loopback interface, so that
-- several test runs share a computer. The store waits three seconds, not
-- thirty, for a node to announce a transaction back: a node that refuses
-- one says nothing, and a test of a refusal waits that long.
--
-- Both nodes start with their clock at 'epoch'. The wallet of Bitcoin
-- Core is made when the chains start, and it leaves aside every block
-- dated before its own birth. With the clock set afterwards, it was born
-- today, saw none of the blocks of January, and had no coin to pay with.
--
-- No connection is kept open between two requests. A node that starts
-- again leaves the connections to its published port open on this side,
-- and the next request on one of them waited thirty seconds for an
-- answer.
startContainer :: String -> IO Regtest
startContainer image = do
  pid <- show <$> getCurrentPid
  count <- atomicModifyIORef' containerCount (\n -> (n + 1, n))
  let name = testLabel <> "-" <> pid <> "-" <> show count
  (code, _, err) <-
    podman
      [ "run",
        "--detach",
        "--rm",
        "--name",
        name,
        "--label",
        testLabel <> "=" <> pid,
        "--tmpfs",
        "/data:rw,size=1g,mode=1777",
        "--env",
        "STORE_PUBLISH_TIMEOUT=3",
        "--env",
        "BTC_NODE_OPTIONS=" <> clockOption epoch,
        "--env",
        "BCH_NODE_OPTIONS=" <> clockOption epoch,
        "--publish",
        "127.0.0.1::3000",
        "--publish",
        "127.0.0.1::18443",
        "--publish",
        "127.0.0.1::18453",
        image
      ]
  when (code /= ExitSuccess) $ do
    void $ podman ["rm", "--force", name]
    ioError (userError ("podman run " <> image <> " failed: " <> trim err))
  ready <- waitFor 600 $ (\(found, _, _) -> found == ExitSuccess) <$> podman ["exec", name, "test", "-e", "/run/regtest/ready"]
  unless ready $ do
    (_, out, logErr) <- podman ["logs", "--tail", "20", name]
    void $ podman ["rm", "--force", name]
    ioError (userError ("the chains were not ready within a minute: " <> out <> logErr))
  host <- portOf name "3000"
  core <- portOf name "18443"
  cash <- portOf name "18453"
  manager <- newManager defaultManagerSettings {managerIdleConnectionCount = 0}
  clock <- newIORef []
  unsettled <- newIORef []
  return
    Regtest
      { regtestContainer = name,
        regtestHost = "http://" <> host <> "/",
        regtestRpc = \chain -> "http://" <> (if chain == Btc then core else cash) <> "/",
        regtestManager = manager,
        regtestClock = clock,
        regtestUnsettled = unsettled
      }
  where
    portOf name port = do
      (_, out, _) <- podman ["port", name, port <> "/tcp"]
      return (trim (takeWhile (/= '\n') out))

-- | Remove the container
stopContainer :: Regtest -> IO ()
stopContainer chains = void $ podman ["rm", "--force", "--time", "2", regtestContainer chains]

-- | Remove the containers of the test runs whose process is gone
removeStale :: IO ()
removeStale = do
  (code, out, _) <- podman ["ps", "--all", "--filter", "label=" <> testLabel, "--format", "{{.ID}} {{index .Labels \"" <> testLabel <> "\"}}"]
  when (code == ExitSuccess) $
    forM_ (words <$> lines out) $ \case
      [container, pid] | all isDigit pid -> do
        alive <- doesPathExist ("/proc/" <> pid)
        unless alive . void $ podman ["rm", "--force", container]
      _ -> return ()

-- | Run podman with the arguments
podman :: [String] -> IO (ExitCode, String, String)
podman args = readProcessWithExitCode "podman" args ""

-- | Run a command in the container, which must succeed
inContainer :: Regtest -> [String] -> IO String
inContainer chains = inContainerWith chains []

-- | 'inContainer', with variables added to the environment of the command
inContainerWith :: Regtest -> [(String, String)] -> [String] -> IO String
inContainerWith chains environment command = do
  (code, out, err) <- podman (["exec"] <> concat [["--env", key <> "=" <> value] | (key, value) <- environment] <> [regtestContainer chains] <> command)
  when (code /= ExitSuccess) $
    ioError (userError (unwords command <> " failed in the container: " <> trim (out <> err)))
  return out

-- | Try the check every tenth of a second, the given number of times at
-- most, until it holds
waitFor :: Int -> IO Bool -> IO Bool
waitFor tries check
  | tries <= 0 = return False
  | otherwise = do
      holds <- check
      if holds then return True else threadDelay 100000 >> waitFor (tries - 1) check

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

{- The clock -}

-- | The time the clock of every node starts at: 1 January 2026, 00:00:00
-- UTC
epoch :: Integer
epoch = 1767225600

-- | The option that starts a node with its clock at the time given
clockOption :: Integer -> String
clockOption time = "-mocktime=" <> show time

-- | Set the clock of the node of a chain, and keep the time, to set it
-- again when the node starts again
setClock :: Regtest -> Chain -> Integer -> IO ()
setClock chains chain time = do
  void $ rpc chains chain "setmocktime" [Json.Number (fromIntegral time)]
  times <- readIORef (regtestClock chains)
  writeIORef (regtestClock chains) ((chain, time) : filter ((/= chain) . fst) times)

{- The nodes -}

-- | Call a method of the node of a chain, which must succeed
rpc :: Regtest -> Chain -> Text -> [Json.Value] -> IO Json.Value
rpc chains chain name params = do
  request <- parseRequest (regtestRpc chains chain)
  let body = Json.object ["jsonrpc" Json..= ("1.0" :: Text), "id" Json..= ("test" :: Text), "method" Json..= name, "params" Json..= params]
      posted =
        applyBasicAuth "test" "test" $
          request {method = "POST", requestBody = RequestBodyLBS (Json.encode body), requestHeaders = [("Content-Type", "text/plain")]}
  response <- httpLbs posted (regtestManager chains)
  case Json.decode (responseBody response) of
    Just answered
      | Just result <- field "result" answered, field "error" answered `elem` [Nothing, Just Json.Null] -> return result
      | Just failure <- field "error" answered -> ioError (userError (Text.unpack name <> " failed on the node of " <> chainName chain <> ": " <> show failure))
    _ -> ioError (userError ("The node of " <> chainName chain <> " gave no answer to " <> Text.unpack name))

-- | A field of a JSON object
field :: Text -> Json.Value -> Maybe Json.Value
field key (Json.Object object) = KeyMap.lookup (Key.fromText key) object
field _ _ = Nothing

-- | A JSON text
textOf :: Json.Value -> Text
textOf (Json.String text) = text
textOf other = Text.pack (show other)

-- | The texts of a JSON list
textsOf :: Json.Value -> [Text]
textsOf (Json.Array items) = textOf <$> toList items
textsOf _ = []

-- | The height of the best block of the node of a chain
heightOf :: Regtest -> Chain -> IO Integer
heightOf chains chain = do
  count <- rpc chains chain "getblockcount" []
  case count of
    Json.Number n -> return (round n)
    other -> ioError (userError ("Not a height: " <> show other))

-- | The ids of the transactions in the mempool of the node of a chain
mempoolOf :: Regtest -> Chain -> IO [Text]
mempoolOf chains chain = textsOf <$> rpc chains chain "getrawmempool" []

-- | A new address of the wallet of the node of a chain, which mines and
-- pays the wallets of the tests
minerAddress :: Regtest -> Chain -> IO Text
minerAddress chains chain = textOf <$> rpc chains chain "getnewaddress" []

-- | An address that no wallet of the tests holds, the wallets of the
-- nodes included, for a payment that is nobody's. A node forgets a
-- transaction that pays it, where it would put back one that pays its own
-- wallet ('forgetMempool'). The same legacy address reads on both chains.
-- Its key hash is twenty bytes of 0x11, so nobody has its key.
outsideAddress :: String
outsideAddress = "mh5CE8Nbj38iND267s4XnvhSmhDW7yWc6Q"

-- | Another address that nobody holds: its key hash is twenty bytes of
-- 0x22
otherOutsideAddress :: String
otherOutsideAddress = "midSACfDe3qAxJZZXA9gkwBZgPqJJUpy1w"

-- | A private key that no wallet of the tests holds, as the owner of an
-- old wallet keeps one, for the sweeps: its 32 bytes are 0x01. This is
-- its text for a compressed public key, on the test networks. The
-- addresses of the key below are the ones Bitcoin Core derives from it
-- (@deriveaddresses@), so a sweep that finds a coin there, and that a
-- node takes, has matched the key and the address as a node does.
outsideKey :: String
outsideKey = "cMceqPhHedrhbcR9eXgzmfWy7kRqLyAxMYwFT6ABDWsiwUp9Nsq9"

-- | The same key, in its text for an uncompressed public key, as wallets
-- wrote keys until 2012
outsideKeyUncompressed :: String
outsideKeyUncompressed = "91bMom7Qi9oc2VsLBKHK5EFwrZVjfxmrFAxLb1GDjiCwpGS6u85"

-- | The legacy address of the compressed public key of 'outsideKey':
-- @pkh(KEY)@. The same address reads on both chains.
outsideKeyLegacy :: String
outsideKeyLegacy = "mrcNu71ztWjAQA6ww9kHiW3zBWSQidHXTQ"

-- | The legacy address of the uncompressed public key of 'outsideKey'. Its
-- input is 32 bytes longer than the one of a compressed key, 180 bytes.
outsideKeyLegacyUncompressed :: String
outsideKeyLegacyUncompressed = "mqitioYrN1kLMUngbCDV1k72uPJZe5x22N"

-- | The native SegWit address of 'outsideKey' on btcreg: @wpkh(KEY)@
outsideKeySegwit :: String
outsideKeySegwit = "bcrt1q0xcqpzrky6eff2g52qdye53xkk9jxkvrl4xfg5"

-- | The address of 'outsideKey' on btcreg for SegWit nested in a script
-- address: @sh(wpkh(KEY))@
outsideKeyNested :: String
outsideKeyNested = "2MvtZ4txAvbaWRW2gXRmmrcUpQfsqNgpfUm"

-- | An address of nobody on btcreg that pays the hash of a script: the
-- script that pays 'outsideKey', @sh(pk(KEY))@, as Bitcoin Core derives it
outsideScriptAddress :: String
outsideScriptAddress = "2MtPBzgKuhnYGEk67u43QtTkJE9rq2xpLnV"

-- | The same script address, as Bitcoin Cash Node writes it on bchreg
outsideCashScriptAddress :: String
outsideCashScriptAddress = "bchreg:pqx8nj3x8zx8zv9t4grekxtg9zy3r5m8wvxprwdr99"

-- | An address of nobody on btcreg that pays a SegWit script:
-- @wsh(pk(KEY))@
outsideSegwitScriptAddress :: String
outsideSegwitScriptAddress = "bcrt1q0g8nfnsvxzt8amgutgszrv0fxgwdn9yakprztj29sqzqhpw8gvuqfhcz2l"

-- | An address of nobody on btcreg that pays a Taproot key: @tr(KEY)@
outsideTaprootAddress :: String
outsideTaprootAddress = "bcrt1p33wm0auhr9kkahzd6l0kqj85af4cswn276hsxg6zpz85xe2r0y8s7hfsm7"

-- | What the node of a chain counts for a transaction of its mempool: its
-- fee in satoshis, and its size, in virtual bytes on Bitcoin and in bytes
-- on Bitcoin Cash. A node relays and mines by these two numbers, whatever
-- hw estimated before it signed.
nodeFeeOf :: Regtest -> Chain -> Text -> IO (Integer, Integer)
nodeFeeOf chains chain txid = do
  entry <- rpc chains chain "getmempoolentry" [Json.String txid]
  let sizeKey = case chain of
        Btc -> "vsize"
        Bch -> "size"
  case (field "base" =<< field "fees" entry, field sizeKey entry) of
    (Just (Json.Number fee), Just (Json.Number size)) -> return (round (fee * 100000000), round size)
    _ -> ioError (userError ("The node of " <> chainName chain <> " gave no fee and size for " <> Text.unpack txid <> ": " <> show entry))

-- | The bytes of a transaction that the node of a chain has in its
-- mempool, in hex, to hand it back later
rawTxOf :: Regtest -> Chain -> Text -> IO Text
rawTxOf chains chain txid = textOf <$> rpc chains chain "getrawtransaction" [Json.String txid]

-- | Hand the node of a chain a transaction, in hex. Nothing waits for
-- the store here: a test that hands a node what the chain of the store
-- cannot hold yet mines the block that holds it next, and 'mine' waits.
sendRawTx :: Regtest -> Chain -> Text -> IO ()
sendRawTx chains chain raw = void $ rpc chains chain "sendrawtransaction" [Json.String raw]

-- | The lock time of a transaction, as the node of a chain reads it
lockTimeOf :: Regtest -> Chain -> Text -> IO Integer
lockTimeOf chains chain txid = do
  decoded <- rpc chains chain "getrawtransaction" [Json.String txid, Json.Bool True]
  case field "locktime" decoded of
    Just (Json.Number lockTime) -> return (round lockTime)
    _ -> ioError (userError ("The node of " <> chainName chain <> " gave no lock time for " <> Text.unpack txid))

-- | Mine blocks on a chain, paid to the wallet of its node, and wait for
-- the store
mine :: Regtest -> Chain -> Int -> IO ()
mine chains chain count = mineTo chains chain count =<< minerAddress chains chain

-- | Mine blocks on a chain, paid to the address, and wait for the store.
-- The clock of the node is first set ten minutes for each block after its
-- start, so that the blocks of one call share the time of the last.
mineTo :: Regtest -> Chain -> Int -> Text -> IO ()
mineTo chains chain count address = do
  height <- heightOf chains chain
  setClock chains chain (epoch + 600 * (height + fromIntegral count))
  void $ rpc chains chain "generatetoaddress" [Json.Number (fromIntegral count), Json.String address]
  waitSynced chains chain

-- | Mine one block dated now, by the clock of this computer, and wait for
-- the store. Every other block of the tests is dated in January 2026,
-- which is long ago for hw: it takes such a chain for one that stopped,
-- and gives its payments no lock time. After this block the chain is
-- fresh, and hw sets the lock time it sets on a live network.
mineNow :: Regtest -> Chain -> IO ()
mineNow chains chain = do
  now <- round <$> getPOSIXTime
  setClock chains chain now
  address <- minerAddress chains chain
  void $ rpc chains chain "generatetoaddress" [Json.Number 1, Json.String address]
  waitSynced chains chain

-- | Mine blocks that leave the mempool of the node as it is, and wait for
-- the store. Bitcoin Core is told what a block holds. Bitcoin Cash Node
-- cannot be, so every transaction of its mempool is first made the last a
-- miner would take, and is given its place back afterwards.
mineLeavingMempool :: Regtest -> Chain -> Int -> IO ()
mineLeavingMempool chains chain count = do
  address <- minerAddress chains chain
  height <- heightOf chains chain
  setClock chains chain (epoch + 600 * (height + fromIntegral count))
  case chain of
    Btc -> forM_ [1 .. count] $ \_ -> rpc chains chain "generateblock" [Json.String address, Json.Array mempty]
    Bch -> do
      held <- mempoolOf chains chain
      forM_ held $ \txid -> rpc chains chain "prioritisetransaction" [Json.String txid, Json.Number 0, Json.Number (-100000000000)]
      void $ rpc chains chain "generatetoaddress" [Json.Number (fromIntegral count), Json.String address]
      forM_ held $ \txid -> rpc chains chain "prioritisetransaction" [Json.String txid, Json.Number 0, Json.Number 100000000000]
  waitSynced chains chain

-- | Pay an address from the wallet of the node of a chain, an amount in
-- satoshis, and wait for the store. Returns the id of the payment. The
-- wallet of the node needs coins a hundred blocks old first.
payFromNode :: Regtest -> Chain -> Text -> Word64 -> IO Text
payFromNode chains chain address satoshis = do
  txid <- textOf <$> rpc chains chain "sendtoaddress" [Json.String address, Json.Number (fromIntegral satoshis / 100000000)]
  waitSynced chains chain
  return txid

-- | Take the last blocks off the chain of a node. Their transactions go
-- back to its mempool, when they are still valid there. Returns the hash
-- of the lowest block taken off, for 'giveBack'.
--
-- The store does not follow: it keeps the chain it has until its node
-- shows one with more work. So mine more blocks afterwards than were
-- taken off, and nothing waits for the store here.
takeOff :: Regtest -> Chain -> Int -> IO Text
takeOff chains chain count = do
  height <- heightOf chains chain
  lowest <- textOf <$> rpc chains chain "getblockhash" [Json.Number (fromIntegral (height - fromIntegral count + 1))]
  void $ rpc chains chain "invalidateblock" [Json.String lowest]
  unsettle chains chain
  return lowest

-- | Give a node back the blocks 'takeOff' took off, from the lowest one
giveBack :: Regtest -> Chain -> Text -> IO ()
giveBack chains chain lowest = do
  void $ rpc chains chain "reconsiderblock" [Json.String lowest]
  unsettle chains chain

-- | Note that the chain of a node lost blocks, for the next wait for its
-- store
unsettle :: Regtest -> Chain -> IO ()
unsettle chains chain = atomicModifyIORef' (regtestUnsettled chains) (\chains' -> (chain : filter (/= chain) chains', ()))

-- | Make the node of a chain forget its mempool, and set its clock again.
-- Its store keeps what it has: a store does not learn that its node
-- dropped a transaction.
forgetMempool :: Regtest -> Chain -> IO ()
forgetMempool chains chain = do
  options <- nodeOptions chains chain ""
  _ <- inContainerWith chains [options] ["regtest-ctl", "forget-mempool", chainName chain]
  resetClock chains chain

-- | Make the store of a chain forget its mempool: it starts again with
-- the option that empties it. A store keeps a transaction that its node
-- dropped, for fourteen days by default, and on Bitcoin Cash it then
-- takes no rival of that transaction, though its node does. So a test
-- where the whole network forgets a transaction empties both.
forgetStoreMempool :: Regtest -> Chain -> IO ()
forgetStoreMempool chains chain = do
  let variable = if chain == Btc then "BTC_STORE_OPTIONS" else "BCH_STORE_OPTIONS"
  _ <- inContainerWith chains [(variable, "--wipe-mempool")] ["regtest-ctl", "restart", "store-" <> chainName chain]
  waitSynced chains chain

-- | Start the node of a chain again with more options, such as
-- @-minrelaytxfee=0.0002@, and set its clock again. The options last
-- until the chains are next reset.
restartNode :: Regtest -> Chain -> String -> IO ()
restartNode chains chain more = do
  options <- nodeOptions chains chain more
  _ <- inContainerWith chains [options] ["regtest-ctl", "restart-node", chainName chain]
  resetClock chains chain

-- | The variable that gives the node of a chain its options when it
-- starts again, with its value: the clock of the node as it is now, then
-- the options given. A node that starts with a clock two hours behind
-- its best block refuses to start, since the block then comes from the
-- future.
nodeOptions :: Regtest -> Chain -> String -> IO (String, String)
nodeOptions chains chain more = do
  times <- readIORef (regtestClock chains)
  let variable = if chain == Btc then "BTC_NODE_OPTIONS" else "BCH_NODE_OPTIONS"
  return (variable, clockOption (fromMaybe epoch (lookup chain times)) <> " " <> more)

-- | Set the clock of a node that started again to what it was
resetClock :: Regtest -> Chain -> IO ()
resetClock chains chain = do
  times <- readIORef (regtestClock chains)
  forM_ (lookup chain times) $ setClock chains chain

-- | Wait until the store of a chain has the best block of its node, has
-- every transaction of the mempool of its node, and says it is healthy:
-- a minute at most.
--
-- haskoin-store 1.12.1 drops the transactions of the blocks that a
-- reorganisation takes off, when the new blocks leave them out. It shows
-- the new best block with those transactions for a moment, and drops
-- them 0.1 to 0.8 seconds later. Its node keeps them in its mempool and
-- does not announce them again, so the store never has them back, and a
-- wallet sees a payment dead until a block confirms it. That is a fault
-- of the store, which these tests are not about. So after blocks were
-- taken off a chain, this waits a second for the store to settle. And
-- when the store lacks a transaction of its node, it is started again
-- with the option that asks its node for its whole mempool, once.
waitSynced :: Regtest -> Chain -> IO ()
waitSynced chains chain = do
  unsettled <- atomicModifyIORef' (regtestUnsettled chains) (\chains' -> (filter (/= chain) chains', chain `elem` chains'))
  when unsettled $ threadDelay 1000000
  soon <- waitFor (if unsettled then 1 else 30) (null <$> behind)
  unless soon $ do
    lacking <- lacksTransactions
    when lacking askNodeForMempool
    inStep <- waitFor 600 (null <$> behind)
    unless inStep $ do
      why <- behind
      ioError (userError ("The store of " <> chainName chain <> " is not in step with its node after a minute: " <> unwords why))
  where
    lacksTransactions = any ("its mempool lacks" `isPrefixOf`) <$> behind
    askNodeForMempool = do
      let variable = if chain == Btc then "BTC_STORE_OPTIONS" else "BCH_STORE_OPTIONS"
      void $ inContainerWith chains [(variable, "--sync-mempool")] ["regtest-ctl", "restart", "store-" <> chainName chain]
    -- What the store is behind in, each as a few words: nothing when it
    -- is in step. The store is asked for each transaction of the mempool
    -- of its node by its id, which it answers from its database. Its
    -- list of the mempool is ahead of that by a moment, and a wallet that
    -- synced in that moment saw the transaction dead.
    behind = do
      best <- textOf <$> rpc chains chain "getbestblockhash" []
      pool <- mempoolOf chains chain
      storeBest <- fromStore "/block/best?notx=true"
      held <- if null pool then return (Just (Json.Array mempty)) else fromStore ("/transactions?txids=" <> Text.unpack (Text.intercalate "," pool))
      health <- fromStore "/health"
      let live = [textOf txid | Just (Json.Array txs) <- [held], tx <- toList txs, field "deleted" tx /= Just (Json.Bool True), Just txid <- [field "txid" tx]]
          missing = filter (`notElem` live) pool
      return $
        ["its best block is " <> maybe "unknown" show (field "hash" =<< storeBest) <> " and the node has " <> Text.unpack best <> "." | (field "hash" =<< storeBest) /= Just (Json.String best)]
          <> ["its mempool lacks " <> Text.unpack (Text.intercalate ", " missing) <> "." | not (null missing)]
          <> ["its health check says " <> maybe "nothing" (show . Json.encode) health <> "." | (field "ok" =<< health) /= Just (Json.Bool True)]
    -- Nothing while the store does not answer, as when it is starting
    fromStore path = do
      request <- parseRequest (regtestHost chains <> networkOf chain <> path)
      answered <- try (httpLbs request (regtestManager chains))
      return $ case answered of
        Right response | statusCode (responseStatus response) `elem` [200, 503] -> Json.decode (responseBody response)
        Right _ -> Nothing
        Left failure -> const Nothing (failure :: HttpException)

{- The services -}

-- | Stop a service of the container: @node-btc@, @node-bch@,
-- @store-btc@, @store-bch@ or @proxy@
stopService :: Regtest -> String -> IO ()
stopService chains service = void $ inContainer chains ["regtest-ctl", "stop", service]

-- | Start a service of the container that was stopped
startService :: Regtest -> String -> IO ()
startService chains service = void $ inContainer chains ["regtest-ctl", "start", service]

-- | Start the store of a chain again with other limits on the age of its
-- last block and of its last transaction, in seconds, as a haskoin-store
-- has them by default: it then says it is not healthy on a quiet chain
startStoreWith :: Regtest -> Chain -> Int -> Int -> IO ()
startStoreWith chains chain blockAge txAge = do
  let service = "store-" <> chainName chain
  stopService chains service
  void $ inContainerWith chains [("STORE_BLOCK_TIMEOUT", show blockAge), ("STORE_TX_TIMEOUT", show txAge)] ["regtest-ctl", "start", service]
  up <- waitFor 600 $ do
    request <- parseRequest (regtestHost chains <> networkOf chain <> "/health")
    answered <- try (httpLbs request (regtestManager chains))
    return $ case answered of
      Right response -> statusCode (responseStatus response) `elem` [200, 503]
      Left failure -> const False (failure :: HttpException)
  unless up $ ioError (userError ("The store of " <> chainName chain <> " did not start again"))

-- | Wait until the store of a chain says how many peers it has: none
-- once its node is stopped, one once it is back. The store checks once a
-- second, so its answer is that old at most.
waitForPeers :: Regtest -> Chain -> Int -> IO ()
waitForPeers chains chain count = do
  told <- waitFor 600 $ do
    request <- parseRequest (regtestHost chains <> networkOf chain <> "/health")
    answered <- try (httpLbs request (regtestManager chains))
    return $ case answered of
      Right response -> (field "count" =<< field "peers" =<< Json.decode (responseBody response)) == Just (Json.Number (fromIntegral count))
      Left failure -> const False (failure :: HttpException)
  unless told $ ioError (userError ("The store of " <> chainName chain <> " did not come to " <> show count <> " peers within a minute"))

{- A wallet on the chains -}

-- | Run a test with the home of a wallet of the mnemonic, set up for the
-- chains: the proxy of the container is its store, it shows the two
-- regtest networks and no other, and btcreg is its default network. The
-- wallet has its portfolio0, made without the store.
withWallet :: Regtest -> MnemonicPass -> (FilePath -> IO a) -> IO a
withWallet chains mnemonic test =
  withHomeOn chains $ \home -> do
    newPortfolioOf home mnemonic
    test home

-- | Run a test with a home set up for the chains as 'withWallet' sets it
-- up, and no wallet in it: what recover starts from
withHomeOn :: Regtest -> (FilePath -> IO a) -> IO a
withHomeOn chains test =
  withHome $ \home -> do
    setConfigKey home "host" (Json.String (Text.pack (regtestHost chains)))
    setShownNetworks home ["btcreg", "bchreg"]
    setDefaultNetwork home (Just "btcreg")
    test home

-- | Run recover in a home with the mnemonic, and the options given, and
-- its answer
recoverWith :: FilePath -> MnemonicPass -> [String] -> IO Response
recoverWith home mnemonic options = hwJson home ("recover" : options) (seedOnceOf mnemonic)

-- | The next receiving address of a wallet on a chain, of the type given,
-- @bip44@ or @bip84@
receiveOn :: FilePath -> Chain -> String -> IO Text
receiveOn home chain kind = Text.pack <$> receiveWith home (["--network", networkOf chain] <> ["--type" | kind /= "bip44"] <> [kind | kind /= "bip44"])

-- | Sync a wallet, and its answer
syncNow :: FilePath -> IO Response
syncNow home = hwJson home ["sync"] []

-- | Prepare a payment of a wallet on a chain, with the options of
-- preparetx after the recipients, and its nosigHash. The chains are old
-- for hw, so every payment takes @--yes@, and a lock time of 0.
prepared :: FilePath -> Chain -> [String] -> IO String
prepared home chain options = nosigOf =<< txOf =<< answer home (["preparetx", "--network", networkOf chain, "--yes"] <> options) []

-- | Sign a pending transaction of a wallet with its mnemonic, and its txid
signedBy :: FilePath -> MnemonicPass -> String -> IO String
signedBy home mnemonic nosig = txidOf =<< txOf =<< answer home ["signtx", nosig] (seedOnceOf mnemonic)

-- | Send a signed pending transaction of a wallet, and wait for the store
sent :: Regtest -> Chain -> FilePath -> String -> IO ()
sent chains chain home txid = do
  _ <- answer home ["broadcast", txid, "--yes"] []
  waitSynced chains chain

-- | Prepare, sign and send a payment of a wallet on a chain, and its txid
pay :: Regtest -> Chain -> FilePath -> MnemonicPass -> [String] -> IO String
pay chains chain home mnemonic options = do
  txid <- signedBy home mnemonic =<< prepared home chain options
  sent chains chain home txid
  return txid

-- | Make a payment of a wallet on a chain with send, the one command that
-- prepares it, signs it and sends it: its txid. The options are those of
-- 'pay', and the payment must be the same.
payWithSend :: Regtest -> Chain -> FilePath -> MnemonicPass -> [String] -> IO String
payWithSend chains chain home mnemonic options = do
  txid <- txidOf =<< txOf =<< answer home (["send", "--network", networkOf chain, "--yes"] <> options) (seedOnceOf mnemonic)
  waitSynced chains chain
  return txid

-- | Prepare, sign and send a payment of everything a wallet can spend on
-- a chain to one address, with the options of preparesendall, and its
-- txid. It has one output, so its txid is the same on every run.
payAll :: Regtest -> Chain -> FilePath -> MnemonicPass -> [String] -> IO String
payAll chains chain home mnemonic options = do
  nosig <- nosigOf =<< txOf =<< answer home (["preparesendall", "--network", networkOf chain, "--yes"] <> options) []
  txid <- signedBy home mnemonic nosig
  sent chains chain home txid
  return txid

-- | Prepare, sign and send the replacement of a payment at a higher fee
-- rate, as bumpfee makes it, and its txid
replacedWith :: Regtest -> Chain -> FilePath -> MnemonicPass -> String -> Natural -> IO String
replacedWith chains chain home mnemonic stuck feeRate = do
  nosig <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", show feeRate, "--yes"] []
  txid <- signedBy home mnemonic nosig
  sent chains chain home txid
  return txid

-- | Prepare a payment on the online computer, to a file, sign the file on
-- the signer with the test mnemonic, and send the signed file from the
-- online computer: the txid of the payment. The signer has no store, and
-- knows of the coins only what the file tells it.
payOffline :: Regtest -> Chain -> FilePath -> FilePath -> [String] -> IO String
payOffline chains chain signer online options = do
  let unsigned = online </> "unsigned.psbt"
      signed = signer </> "signed.psbt"
  _ <- answer online (["preparetx", "--network", networkOf chain, "--yes", "--output", unsigned] <> options) []
  txid <- txidOf =<< txOf =<< answer signer ["signtx", "--input", unsigned, "--output", signed] (seedOnceOf testMnemonic)
  _ <- answer online ["broadcast", "--input", signed, "--yes"] []
  waitSynced chains chain
  return txid

-- | Prepare a sweep in a wallet, with the options of preparesweep, sign
-- it with the keys of the file, and send it: its txid
sweep :: Regtest -> Chain -> FilePath -> FilePath -> [String] -> IO String
sweep chains chain home keyFile options = do
  nosig <- nosigOf =<< txOf =<< answer home ("preparesweep" : options) []
  txid <- txidOf =<< txOf =<< answer home ["signsweep", nosig, "--key-file", keyFile] []
  sent chains chain home txid
  return txid

-- | Write a private key to a file of a home, as the owner of an old
-- wallet keeps it, and give the path of the file
keyFileIn :: FilePath -> String -> IO FilePath
keyFileIn home key = do
  let file = home </> "keys.txt"
  writeFile file (key <> "\n")
  return file

-- | The transactions of the active portfolio of a wallet on a chain, the
-- newest first: the type of each, what it adds to the portfolio or takes
-- from it in satoshis, and its confirmations. An unconfirmed one has 0,
-- and a dead one none.
txsOn :: FilePath -> Chain -> IO [(TxType, Integer, Maybe Natural)]
txsOn home chain = do
  listed <- hwJson home ["txs", "--network", networkOf chain] []
  case listed of
    ResponseTxs groups -> return [row (listedTxInfo tx) | group <- groups, tx <- groupRows group]
    other -> fail ("Not a listing of transactions: " <> show other)
  where
    row info = (summaryType (txInfoSummary info), summaryAmount (txInfoSummary info), confirmationsOf (txInfoStatus info))
    confirmationsOf (OnChain status)
      | chainDead status = Nothing
      | otherwise = Just (chainConfirmations status)
    confirmationsOf (Pending _) = Nothing

-- | The dead transactions of the active portfolio of a wallet on a chain:
-- the txid of each, as typed, with the txids of the transactions that
-- replaced it
deadOn :: FilePath -> Chain -> IO [(String, [String])]
deadOn home chain = do
  listed <- hwJson home ["txs", "--network", networkOf chain] []
  case listed of
    ResponseTxs groups ->
      return [(typed (chainTxid status), typed <$> chainReplacedBy status) | group <- groups, tx <- groupRows group, OnChain status <- [txInfoStatus (listedTxInfo tx)], chainDead status]
    other -> fail ("Not a listing of transactions: " <> show other)
  where
    typed = Text.unpack . txHashToHex

-- | The coins of the active portfolio of a wallet on a chain, the newest
-- first: the value of each in satoshis, its confirmations, and whether it
-- can be spent now
coinsOn :: FilePath -> Chain -> IO [(Word64, Natural, Bool)]
coinsOn home chain = do
  shown <- either fail return . coinsShown =<< hwJson home ["coins", "--network", networkOf chain] []
  return [(value, confirmations, spendable) | (_, value, confirmations, _, spendable) <- shown]

-- | What the active portfolio of a wallet can spend now on a chain, in
-- satoshis
spendableOf :: FilePath -> Chain -> IO Word64
spendableOf home chain = sum . map (\(value, _, _) -> value) . filter (\(_, _, spendable) -> spendable) <$> coinsOn home chain

-- | The pending transactions of a wallet: each by the id the listings
-- show, its txid once signed and its nosigHash before, as typed, with
-- whether it is signed, sent, and in conflict
pendingOf :: FilePath -> IO [(String, Bool, Bool, Bool)]
pendingOf home = do
  listed <- hwJson home ["pendingtxs"] []
  case listed of
    ResponsePendingTxs _ held -> return [state status | (_, info) <- held, Pending status <- [txInfoStatus info]]
    other -> fail ("Not the pending transactions: " <> show other)
  where
    state status = (Text.unpack (txHashToHex (pendingShownHash status)), pendingSigned status, pendingOnline status, pendingConflicted status)

-- | What a sync says of a chain in its answer, given what to read of the
-- result of each portfolio there
onChain :: Chain -> (SyncRes -> a) -> Response -> IO [a]
onChain chain reading = \case
  ResponseSync results _ _ _ -> return [reading result | result <- results, (placeNetwork (syncResPlace result)).name == networkOf chain]
  other -> fail ("Not the answer of a sync: " <> show other)

-- | The pending transactions that a sync sent again on a chain and that
-- the store did not publish: the txid of each, as typed, with what the
-- store said
notPublishedOn :: Chain -> Response -> IO [(String, String)]
notPublishedOn chain answered = do
  refused <- concat <$> onChain chain syncResNotPublished answered
  return [(Text.unpack (txHashToHex txid), why) | (txid, why) <- refused]

-- | How many pending transactions a sync found in conflict on a chain
conflictedOn :: Chain -> Response -> IO Natural
conflictedOn chain answered = sum <$> onChain chain syncResConflicted answered

-- | How many pending transactions a sync sent again on a chain, which the
-- store published
sentAgainOn :: Chain -> Response -> IO Natural
sentAgainOn chain answered = sum <$> onChain chain syncResRebroadcast answered

-- | The failure of the store of a chain that a sync left out, if any
notSyncedOn :: Chain -> Response -> IO (Maybe Text)
notSyncedOn chain = \case
  ResponseSync _ _ notSynced _ -> return (lookup (networkOf chain) [(net.name, failure) | (net, failure) <- notSynced])
  other -> fail ("Not the answer of a sync: " <> show other)

-- | The message of a command that failed
errorOf :: Response -> IO Text
errorOf (ResponseError message) = return message
errorOf other = fail ("The command did not fail: " <> show other)

{- Histories -}

-- | Run a test with the wallet of the first test mnemonic, paid once on a
-- chain by the wallet of the node: 100000 satoshis at its first legacy
-- address, in block 102, which is the best block. The wallet is synced.
withPaidWallet :: Regtest -> Chain -> (FilePath -> IO a) -> IO a
withPaidWallet chains chain test = do
  mine chains chain 101
  withWallet chains testMnemonic $ \home -> do
    address <- receiveOn home chain "bip44"
    _ <- payFromNode chains chain address 100000
    mine chains chain 1
    _ <- syncNow home
    test home

-- | 'withPaidWallet', with a second computer that holds the same
-- mnemonic and is synced too: the home of each
withTwoComputers :: Regtest -> Chain -> (FilePath -> FilePath -> IO a) -> IO a
withTwoComputers chains chain test =
  withPaidWallet chains chain $ \home ->
    withWallet chains testMnemonic $ \other -> do
      _ <- syncNow other
      test home other

-- | Run a test with two computers of a payer, who holds the second test
-- mnemonic: the home of each. The payer mined the first block of the
-- chain, and its 50 coins are 101 blocks old, so they can be spent. Both
-- computers are synced.
withPayer :: Regtest -> Chain -> (FilePath -> FilePath -> IO a) -> IO a
withPayer chains chain test =
  withWallet chains testMnemonic2 $ \payer ->
    withWallet chains testMnemonic2 $ \other -> do
      address <- receiveOn payer chain "bip44"
      mineTo chains chain 1 address
      mine chains chain 100
      _ <- syncNow payer
      _ <- syncNow other
      test payer other

-- | A payment that its payer can still take back
data Received = Received
  { -- | The home of the wallet that was paid
    receivedHome :: FilePath,
    -- | The home of the other computer of the payer
    receivedRivalHome :: FilePath,
    -- | The transaction that computer holds, signed and not sent, which
    -- spends the coin of the payment to somebody else: its txid
    receivedRival :: String
  }

-- | Run a test with the wallet of the first test mnemonic, paid 100000
-- satoshis at its first legacy address by the payer of 'withPayer', in
-- block 102, which is the best block. The other computer of the payer
-- holds a rival of that payment: 40000 satoshis to an address of nobody,
-- at 20 satoshis per byte, from the same coin. The wallet is synced.
withReceived :: Regtest -> Chain -> (Received -> IO a) -> IO a
withReceived chains chain test =
  withPayer chains chain $ \payer other ->
    withWallet chains testMnemonic $ \home -> do
      address <- receiveOn home chain "bip44"
      rival <- signedBy other testMnemonic2 =<< prepared other chain [otherOutsideAddress <> ":0.0004", "--fee-rate", "20"]
      _ <- pay chains chain payer testMnemonic2 [Text.unpack address <> ":0.001", "--fee-rate", "10"]
      mine chains chain 1
      _ <- syncNow home
      test (Received home other rival)

-- | Replace the best block of a chain by three, where a rival of a
-- transaction of that block takes its place. The block is taken off, and
-- the node forgets its mempool, so the transaction is in none. Two empty
-- blocks make the new chain the longer one. The store then forgets its
-- mempool too, the home given sends the rival it holds, and a third block
-- confirms it. Returns the hash of the block taken off, for
-- 'firstChainBack'.
replaceWithRival :: Regtest -> Chain -> FilePath -> String -> IO Text
replaceWithRival chains chain rivalHome rival = do
  lowest <- takeOff chains chain 1
  forgetMempool chains chain
  mine chains chain 2
  forgetStoreMempool chains chain
  sent chains chain rivalHome rival
  mine chains chain 1
  return lowest

-- | Bring back the chain that 'replaceWithRival' replaced, longer than
-- the one that took its place: the three blocks of that one are taken
-- off, the block of the first chain is given back, and three blocks are
-- mined on top of it
firstChainBack :: Regtest -> Chain -> Text -> IO ()
firstChainBack chains chain lowest = do
  _ <- takeOff chains chain 3
  giveBack chains chain lowest
  mine chains chain 3

-- | Run a test with a signer and an online computer: the home of each.
-- The signer holds the first test mnemonic and has no store. The online
-- computer holds the same portfolio without its keys, from the file
-- exportwallet wrote on the signer. Its first address of the type given,
-- @bip44@ or @bip84@, was paid 100000 satoshis by the wallet of the node,
-- in block 102, which is the best block. It is synced.
withSignerAndOnline :: Regtest -> Chain -> String -> (FilePath -> FilePath -> IO a) -> IO a
withSignerAndOnline chains chain kind test = do
  mine chains chain 101
  withHome $ \signer -> do
    newPortfolioOf signer testMnemonic
    _ <- answer signer ["exportwallet", signer </> "wallet.jsonl"] []
    withHomeOn chains $ \online -> do
      _ <- answer online ["importwallet", signer </> "wallet.jsonl"] []
      address <- receiveOn online chain kind
      _ <- payFromNode chains chain address 100000
      mine chains chain 1
      _ <- syncNow online
      test signer online

-- | Run a test with the wallet of the first test mnemonic, whose first
-- legacy address the wallet of the node paid the given number of times,
-- 10000 satoshis each time, a payment a transaction. The store answers a
-- question without a limit with a page of a hundred items, so more
-- payments than that show whether hw asks for every one. A block follows
-- every twenty payments, and the last: a node takes no more than 25
-- unconfirmed transactions in a row. The wallet is synced.
withManyPayments :: Regtest -> Chain -> Int -> (FilePath -> IO a) -> IO a
withManyPayments chains chain count test = do
  mine chains chain 101
  withWallet chains testMnemonic $ \home -> do
    address <- receiveOn home chain "bip44"
    forM_ [1 .. count] $ \n -> do
      void $ rpc chains chain "sendtoaddress" [Json.String address, Json.Number 0.0001]
      when (n `mod` 20 == 0 || n == count) $ mine chains chain 1
    _ <- syncNow home
    test home

-- | Run a test with the wallet of the first test mnemonic, which nobody
-- paid, on a chain where the wallet of the node paid 100000 satoshis to
-- each of the addresses given, in block 102, which is the best block.
-- The addresses are those of 'outsideKey', whose coins a sweep moves.
withCoinsOfKey :: Regtest -> Chain -> [String] -> (FilePath -> IO a) -> IO a
withCoinsOfKey chains chain addresses test = do
  mine chains chain 101
  forM_ addresses $ \address -> payFromNode chains chain (Text.pack address) 100000
  mine chains chain 1
  withWallet chains testMnemonic test
