{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | hw as a user runs it, for the tests of whole workflows: the hw binary
-- of this build, run in a home of its own, with the lines the user types
-- on its input. A fake haskoin-store serves it a small chain over HTTP, as
-- the real one would: the tests write the chain, mine its mempool, and see
-- what hw sends to the network.
module Haskoin.Wallet.HwTestUtils
  ( -- * Running hw
    Run (..),
    withHome,
    setDefaultNetwork,
    setConfigKey,
    setUnit,
    setListingView,
    hw,
    hwWithEnv,
    hwOnTerminal,
    hwOnScreen,
    whileHwWaitsAt,
    centralEurope,
    hwJson,
    hwJsonValue,
    tabAfter,
    bashCompletion,
    -- * The chain of the fake store
    Chain (..),
    mine,
    inMempool,
    withChain,
    withChainDownOn,
    withChainServing,
    withChainFailingOn,
    withChainMinedAgo,
    withLyingChain,
    withServerAnswering,
    withServerSending,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (filterM, join)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Bytes.Get (runGetL)
import Data.Bytes.Put (runPutL)
import Data.Bytes.Serial (Serial, deserialize, serialize)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Serialize as S
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word32)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Store.WebCommon (HeightParam (..), LimitParam (..), Param (..))
import Haskoin.Wallet.Cli.Response (Response)
import Haskoin.Wallet.Store (Store (..), checkHealth)
import Haskoin.Wallet.TestUtils (addressScript, blockMinedAt, otherAddr)
import Network.HTTP.Types (hContentType, status200, status404, status502)
import Network.Wai (Application, pathInfo, queryString, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import qualified System.IO as IO
import System.Posix.IO (fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Types (ProcessID)
import qualified System.Process as Process
import System.Process.Typed (byteStringInput, createPipe, getStderr, nullStream, proc, readProcess, readProcessStderr, readProcessStdout, setEnv, setStderr, setStdin, setStdout, setWorkingDir, unsafeProcessHandle, useHandleClose, withProcessTerm)
import System.Timeout (timeout)
import UnliftIO.Async (wait, withAsync)
import UnliftIO.Temporary (withSystemTempDirectory)

{- Running hw -}

-- | What a run of hw left: its exit code, and what it wrote on its output
-- and on its error output
data Run = Run
  { runExit :: ExitCode,
    runOut :: Text,
    runErr :: Text
  }
  deriving (Show)

-- | Run a test with a home of its own for hw, removed afterwards. Its
-- config.json points at a store that refuses every connection: a command
-- that needs the network fails at once, and no test reaches a real store.
-- The real wallet in ~/.hw is out of reach, since hw only sees this home.
withHome :: (FilePath -> IO a) -> IO a
withHome test =
  withSystemTempDirectory "hw-home" $ \home -> do
    useStore home "http://127.0.0.1:1/"
    test home

-- | Point the hw of a home at a store, on Bitcoin by default, as the
-- configuration file hw writes for a new home is
useStore :: FilePath -> String -> IO ()
useStore home url = do
  createDirectoryIfMissing True (home </> ".hw")
  BL.writeFile (configOf home) (Json.encode (Json.object ["host" Json..= url, "default-network" Json..= ("btc" :: Text)]))

-- | The configuration file of a home
configOf :: FilePath -> FilePath
configOf home = home </> ".hw" </> "config.json"

-- | Set the default network of the configuration file of a home, or take
-- it away
setDefaultNetwork :: FilePath -> Maybe Text -> IO ()
setDefaultNetwork home netM =
  editConfig home $ maybe (KeyMap.delete "default-network") (KeyMap.insert "default-network" . Json.String) netM

-- | Set a key of the configuration file of a home, such as default-type
-- or listing-view, to the value given
setConfigKey :: FilePath -> Text -> Json.Value -> IO ()
setConfigKey home key value = editConfig home (KeyMap.insert (Key.fromText key) value)

-- | Set the unit of the amounts without --unit, by its name, in the
-- configuration file of a home
setUnit :: FilePath -> Text -> IO ()
setUnit home name = editConfig home (KeyMap.insert "unit" (Json.String name))

-- | Set the view of the listings without --compact, --verbose or
-- --tabular, by its name, in the configuration file of a home
setListingView :: FilePath -> Text -> IO ()
setListingView home name = editConfig home (KeyMap.insert "listing-view" (Json.String name))

-- | Change the keys of the configuration file of a home
editConfig :: FilePath -> (Json.Object -> Json.Object) -> IO ()
editConfig home edit = do
  config <- Json.decodeFileStrict (configOf home)
  case config of
    Just (Json.Object o) -> Json.encodeFile (configOf home) (Json.Object (edit o))
    _ -> fail "The configuration file of the home is not a JSON object"

-- | The hw binary of this build. The one on the PATH may be another
-- version, so it is never looked up there: it is next to the binary of the
-- tests, as stack and cabal build them (build-tool-depends).
hwBinary :: IO FilePath
hwBinary = do
  tests <- getExecutablePath
  let builds = takeDirectory (takeDirectory tests)
      -- stack: build/spec/spec, build/hw/hw; cabal: t/spec/build/spec/spec,
      -- x/hw/build/hw/hw
      candidates = [builds </> "hw" </> "hw", takeDirectory (takeDirectory builds) </> "x" </> "hw" </> "build" </> "hw" </> "hw"]
  found <- filterM doesFileExist candidates
  case found of
    binary : _ -> return binary
    [] -> fail $ "The hw binary of this build is not next to the tests: " <> show candidates

-- | Run hw in a home, with the given arguments and the given lines typed
-- on its input, which then ends. Nothing but HOME is in its environment:
-- no locale, as cron jobs and containers run it. A run longer than a
-- minute fails the test.
hw :: FilePath -> [String] -> [Text] -> IO Run
hw = hwWithEnv []

-- | Run hw as 'hw' does, with these variables added to its environment:
-- TZ, for instance, to show the times on a clock the test knows
hwWithEnv :: [(String, String)] -> FilePath -> [String] -> [Text] -> IO Run
hwWithEnv extra home args typed = do
  binary <- hwBinary
  let process =
        setStdin (byteStringInput (BL.fromStrict (encodeUtf8 (Text.unlines typed))))
          . setEnv (("HOME", home) : extra)
          . setWorkingDir home
          $ proc binary args
  done <- timeout 60000000 (readProcess process)
  case done of
    Nothing -> fail $ "hw " <> unwords args <> " did not finish within a minute"
    Just (code, out, err) -> return $ Run code (decodeUtf8 (BL.toStrict out)) (decodeUtf8 (BL.toStrict err))

-- | Run hw as 'hw' does, with its error output on a terminal, as a user
-- has it. 'runErr' is all the terminal received, its control codes
-- included, and a new line comes as a carriage return and a line feed.
hwOnTerminal :: FilePath -> [String] -> [Text] -> IO Run
hwOnTerminal home args typed = do
  binary <- hwBinary
  (screenFd, terminalFd) <- openPseudoTerminal
  screen <- fdToHandle screenFd
  terminal <- fdToHandle terminalFd
  let process =
        setStdin (byteStringInput (BL.fromStrict (encodeUtf8 (Text.unlines typed))))
          . setStderr (useHandleClose terminal)
          . setEnv [("HOME", home), ("TERM", "xterm")]
          . setWorkingDir home
          $ proc binary args
  -- Read as it comes: hw would stop on a terminal nobody reads
  done <- timeout 60000000 $ withAsync (shownOn screen) $ \shown -> do
    (code, out) <- readProcessStdout process
    (,,) code out <$> wait shown
  case done of
    Nothing -> fail $ "hw " <> unwords args <> " did not finish within a minute"
    Just (code, out, shown) -> return $ Run code (decodeUtf8 (BL.toStrict out)) (decodeUtf8 shown)

-- | Run hw as 'hw' does, with its answer on a terminal, as a user reads it,
-- and these variables added to its environment. 'runOut' is all the
-- terminal received, its control codes included, and a new line comes as a
-- carriage return and a line feed.
hwOnScreen :: [(String, String)] -> FilePath -> [String] -> IO Run
hwOnScreen extra home args = do
  binary <- hwBinary
  (screenFd, terminalFd) <- openPseudoTerminal
  screen <- fdToHandle screenFd
  terminal <- fdToHandle terminalFd
  let process =
        setStdin (byteStringInput BL.empty)
          . setStdout (useHandleClose terminal)
          . setEnv (("HOME", home) : ("TERM", "xterm") : extra)
          . setWorkingDir home
          $ proc binary args
  -- Read as it comes: hw would stop on a terminal nobody reads
  done <- timeout 60000000 $ withAsync (shownOn screen) $ \shown -> do
    (code, err) <- readProcessStderr process
    (,,) code err <$> wait shown
  case done of
    Nothing -> fail $ "hw " <> unwords args <> " did not finish within a minute"
    Just (code, err, out) -> return $ Run code (decodeUtf8 out) (decodeUtf8 (BL.toStrict err))

-- | All a terminal shows until the last program on it ends. Linux then
-- fails the read rather than giving an end of file.
shownOn :: IO.Handle -> IO BS.ByteString
shownOn screen = go BS.empty
  where
    go seen = do
      more <- try (BS.hGetSome screen 4096) :: IO (Either IOException BS.ByteString)
      case more of
        Right bytes | not (BS.null bytes) -> go (seen <> bytes)
        _ -> return seen

-- | Run hw in a home and look at its process while it waits for a line to
-- be typed. The test gets the id of the process once hw has shown the
-- prompt given on its error output. Nothing is ever typed, and hw is
-- stopped when the test is done. A prompt that does not show within a
-- minute fails the test.
whileHwWaitsAt :: FilePath -> [String] -> Text -> (ProcessID -> IO a) -> IO a
whileHwWaitsAt home args prompt look = do
  binary <- hwBinary
  let process =
        setStdin createPipe
          . setStdout nullStream
          . setStderr createPipe
          . setEnv [("HOME", home)]
          . setWorkingDir home
          $ proc binary args
  withProcessTerm process $ \running -> do
    shown <- timeout 60000000 (shownUpToPrompt (getStderr running) BS.empty)
    pid <- Process.getPid (unsafeProcessHandle running)
    case (shown, pid) of
      (Just seen, Just waiting) | asked seen -> look waiting
      (Just seen, _) -> fail $ "hw " <> unwords args <> " ended without asking, after: " <> show seen
      (Nothing, _) -> fail $ "hw " <> unwords args <> " did not ask within a minute"
  where
    asked seen = encodeUtf8 prompt `BS.isInfixOf` seen
    -- A prompt ends with no new line, so the output is read as it comes
    shownUpToPrompt :: IO.Handle -> BS.ByteString -> IO BS.ByteString
    shownUpToPrompt output seen
      | asked seen = return seen
      | otherwise = do
          more <- BS.hGetSome output 4096
          if BS.null more then return seen else shownUpToPrompt output (seen <> more)

-- | The arguments the completion script of bash passes hw when Tab is
-- pressed after the words typed after hw. They are the index of the last
-- word, the one to complete, then every word, hw first.
tabAfter :: [String] -> [String]
tabAfter typed = ["--bash-completion-index", show (length typed)] <> concatMap (\w -> ["--bash-completion-word", w]) ("hw" : typed)

-- | What the completion script of bash offers for the words typed after
-- hw, with the cursor at the end of the last one: bash sources the script
-- from the hw binary of this build, in the home, and lists what the
-- script put in COMPREPLY, one a line
bashCompletion :: FilePath -> [String] -> IO [Text]
bashCompletion home typed = do
  binary <- hwBinary
  let script =
        unlines
          [ "source <(\"$1\" --bash-completion-script \"$1\")",
            "shift",
            "COMP_WORDS=(hw \"$@\")",
            "COMP_CWORD=$#",
            "_hw",
            "printf '%s\\n' \"${COMPREPLY[@]}\""
          ]
      process = setEnv [("HOME", home)] . setWorkingDir home $ proc "bash" ("-c" : script : "bash" : binary : typed)
  (code, out, err) <- readProcess process
  case code of
    ExitSuccess -> return $ filter (not . Text.null) $ Text.lines (decodeUtf8 (BL.toStrict out))
    _ -> fail $ "The completion script of bash failed: " <> Text.unpack (decodeUtf8 (BL.toStrict err))

-- | The time zone of Central Europe, for TZ, written as POSIX rules that
-- need no zone files: CET, an hour ahead of UTC, and CEST, two hours
-- ahead, from the last Sunday of March to the last Sunday of October
centralEurope :: String
centralEurope = "CET-1CEST,M3.5.0,M10.5.0/3"

-- | Run hw with --json, and its answer, which must read as one
hwJson :: FilePath -> [String] -> [Text] -> IO Response
hwJson = hwJsonAs

-- | Run hw with --json, and its answer as the JSON hw wrote, for the keys
-- that a 'Response' does not keep, such as the descriptor of an account
hwJsonValue :: FilePath -> [String] -> [Text] -> IO Json.Value
hwJsonValue = hwJsonAs

-- | Run hw with --json, and its answer, which must read as the type asked
hwJsonAs :: (Json.FromJSON a) => FilePath -> [String] -> [Text] -> IO a
hwJsonAs home args typed = do
  run <- hw home (args <> ["--json"]) typed
  either (\e -> fail $ "hw " <> unwords args <> " did not answer in JSON (" <> e <> "): " <> show run) return $
    Json.eitherDecodeStrict (encodeUtf8 run.runOut)

{- The chain of the fake store -}

-- | The chain the fake store serves: the height of its best block, and its
-- transactions in the order they came, each with the height of its block,
-- or none while in the mempool
data Chain = Chain
  { chainHeight :: BlockHeight,
    chainTxs :: [(Tx, Maybe BlockHeight)]
  }

-- | The chain with a new block, which holds the transactions of its mempool
mine :: Chain -> Chain
mine (Chain height txs) = Chain (height + 1) [(tx, Just (fromMaybe (height + 1) block)) | (tx, block) <- txs]

-- | The chain without the transactions of the mempool that spend a coin the
-- transaction spends, and those that spend theirs: a node of Bitcoin Core
-- takes a replacement in their place, as long as it pays more
withoutConflicts :: Tx -> Chain -> Chain
withoutConflicts tx chain = chain {chainTxs = [entry | entry@(t, _) <- chain.chainTxs, txHash t `notElem` evicted]}
  where
    mempool = [t | (t, Nothing) <- chain.chainTxs]
    spends :: [OutPoint] -> Tx -> Bool
    spends spent t = any ((`elem` spent) . (.outpoint)) t.inputs
    direct = [txHash t | t <- mempool, spends ((.outpoint) <$> tx.inputs) t]
    evicted = grow direct
    grow :: [TxHash] -> [TxHash]
    grow ids =
      let more = [txHash t | t <- mempool, txHash t `notElem` ids, any ((`elem` ids) . (.outpoint.hash)) t.inputs]
       in if null more then ids else grow (ids <> more)

-- | The transactions of the mempool: what hw sent to the network, until the
-- chain is mined
inMempool :: Chain -> [Tx]
inMempool chain = [tx | (tx, Nothing) <- chain.chainTxs]

-- | The transactions with their places: a block and a position in it, or
-- the mempool
placed :: Chain -> [(Tx, Store.BlockRef)]
placed chain = [(tx, place i block) | (i, (tx, block)) <- zip [0 :: Int ..] chain.chainTxs]
  where
    place _ Nothing = Store.MemRef 1700000000
    place i (Just height) = Store.BlockRef height (fromIntegral (length [() | (_, b) <- take i chain.chainTxs, b == Just height]))

-- | The outputs of the chain, by outpoint, with their places
outputsOf :: Chain -> [(OutPoint, TxOut, Store.BlockRef)]
outputsOf chain = [(OutPoint (txHash tx) i, out, ref) | (tx, ref) <- placed chain, (i, out) <- zip [0 ..] tx.outputs]

-- | The input of the chain that spends each outpoint it spends
spenders :: Chain -> Map.Map OutPoint Store.Spender
spenders chain = Map.fromList [(input.outpoint, Store.Spender (txHash tx) i) | (tx, _) <- chain.chainTxs, (i, input) <- zip [0 ..] tx.inputs]

-- | The address an output pays, if a standard one. The fake store has one
-- chain for every network, so the address is read as on Bitcoin, which
-- has every kind of address the tests pay.
addressOf :: Ctx -> TxOut -> Maybe Address
addressOf ctx out = either (const Nothing) Just (scriptToAddressBS btc ctx out.script)

-- | The outputs of the chain that no transaction of it spends
unspentsOf :: Ctx -> Chain -> [Store.Unspent]
unspentsOf ctx chain =
  [Store.Unspent ref outpoint out.value out.script (addressOf ctx out) | (outpoint, out, ref) <- outputsOf chain, outpoint `Map.notMember` spenders chain]

-- | The transactions of an address, newest first: those paying it, and
-- those spending its outputs
refsOf :: Ctx -> Chain -> Address -> [Store.TxRef]
refsOf ctx chain addr = reverse [Store.TxRef ref (txHash tx) | (tx, ref) <- placed chain, touches tx]
  where
    paid = Map.fromList [(outpoint, addressOf ctx out) | (outpoint, out, _) <- outputsOf chain]
    touches :: Tx -> Bool
    touches tx =
      any ((== Just addr) . addressOf ctx) tx.outputs
        || any (\input -> Map.lookup input.outpoint paid == Just (Just addr)) tx.inputs

-- | The balance of an address, from its coins and its transactions
balanceOf :: Ctx -> Chain -> Address -> Store.Balance
balanceOf ctx chain addr =
  Store.Balance
    addr
    (sum [coin.value | coin <- coins, Store.confirmed coin.block])
    (sum [coin.value | coin <- coins, not (Store.confirmed coin.block)])
    (fromIntegral (length coins))
    (fromIntegral (length (refsOf ctx chain addr)))
    (sum [out.value | (_, out, _) <- outputsOf chain, addressOf ctx out == Just addr])
  where
    coins = [coin | coin <- unspentsOf ctx chain, coin.address == Just addr]

-- | A transaction of the chain as the store describes it. Its inputs from
-- outside the chain come from another wallet, worth what makes its fee
-- 1000 satoshis, as the funding transactions of the tests are.
storeTxOf :: Ctx -> Chain -> (Tx, Store.BlockRef) -> Store.Transaction
storeTxOf ctx chain (tx, ref) =
  Store.Transaction
    { Store.block = ref,
      Store.version = tx.version,
      Store.locktime = tx.locktime,
      Store.inputs = inputs,
      Store.outputs = [Store.StoreOutput out.value out.script (Map.lookup (OutPoint (txHash tx) i) (spenders chain)) (addressOf ctx out) | (i, out) <- zip [0 ..] tx.outputs],
      Store.deleted = False,
      Store.rbf = False,
      Store.timestamp = 1700000000,
      Store.txid = txHash tx,
      Store.size = size,
      Store.weight = 4 * size,
      Store.fee = fromChain + outside - paid
    }
  where
    known = Map.fromList [(outpoint, out) | (outpoint, out, _) <- outputsOf chain]
    paid = sum ((.value) <$> tx.outputs)
    fromChain = sum [out.value | i <- tx.inputs, Just out <- [Map.lookup i.outpoint known]]
    outside = if fromChain >= paid + 1000 then 0 else paid + 1000 - fromChain
    firstOutside = length (takeWhile (\i -> i.outpoint `Map.member` known) tx.inputs)
    witnessOf :: Int -> WitnessStack
    witnessOf n = if n < length tx.witness then tx.witness !! n else []
    inputs = zipWith input [0 ..] tx.inputs
    input :: Int -> TxIn -> Store.StoreInput
    input n i = case Map.lookup i.outpoint known of
      Just out -> Store.StoreInput i.outpoint i.sequence i.script out.script out.value (witnessOf n) (addressOf ctx out)
      Nothing ->
        Store.StoreInput i.outpoint i.sequence i.script (addressScript ctx (otherAddr 0)) (if n == firstOutside then outside else 0) (witnessOf n) (Just (otherAddr 0))
    size = fromIntegral (BS.length (S.encode tx))

-- | The store of a chain held in a reference: each answer reads the chain
-- as it is then, and a transaction handed to the network joins its mempool.
-- The latest blocks it lists are timed from the given Unix time of its
-- best block, ten minutes apart, and hold the fees of their transactions:
-- what the fee rates are read from. The other block questions answer with
-- untimed blocks, whose hashes the sync keeps, so that time does not move
-- the chain under it.
chainStore :: Ctx -> IORef Chain -> Word32 -> Store
chainStore ctx ref bestTime =
  Store
    { storeHealth = return (),
      storeBestBlock = (\chain -> blockOf chain.chainHeight) <$> now,
      storeBlocksAtHeight = \height -> (\chain -> [blockOf height | height <= chain.chainHeight]) <$> now,
      storeLatestBlocks = (\chain -> timedBlock chain <$> latestHeights chain) <$> now,
      storeBlockTransactions = \hash ->
        (\chain -> [storeTxOf ctx chain p | height <- latestHeights chain, headerHash (timedBlock chain height).header == hash, p@(_, Store.BlockRef h _) <- placed chain, h == height]) <$> now,
      storeBalances = \addrs -> (\chain -> balanceOf ctx chain <$> addrs) <$> now,
      storeAddressTxs = \addr count -> (\chain -> (if count == 0 then id else take (fromIntegral count)) (refsOf ctx chain addr)) <$> now,
      storeTransactions = \tids -> (\chain -> [storeTxOf ctx chain p | tid <- tids, p@(tx, _) <- placed chain, txHash tx == tid]) <$> now,
      storeUnspents = \addrs -> (\chain -> [coin | coin <- unspentsOf ctx chain, maybe False (`elem` addrs) coin.address]) <$> now,
      storeRawTransactions = \tids -> (\chain -> [tx | tid <- tids, (tx, _) <- chain.chainTxs, txHash tx == tid]) <$> now,
      storePublish = \tx -> liftIO $ modifyIORef ref $ \chain ->
        if any ((== txHash tx) . txHash . fst) chain.chainTxs
          then chain
          else
            let kept = withoutConflicts tx chain
             in kept {chainTxs = kept.chainTxs <> [(tx, Nothing)]},
      storeFailed = \_ -> return ()
    }
  where
    now = liftIO (readIORef ref)
    -- A block of the main chain, its hash its own at each height. Every
    -- block has the time of the best one, which does not move with the
    -- chain: the sync keeps the hash of the best block, and a time of its
    -- own at each height would change it as the chain grows.
    blockOf height = blockMinedAt height (fromIntegral height) bestTime 0
    -- The hundred latest heights, newest first, as the store lists them
    latestHeights :: Chain -> [BlockHeight]
    latestHeights chain = [chain.chainHeight, chain.chainHeight - 1 .. max 1 (chain.chainHeight - 99)]
    timedBlock chain height =
      let fees = sum [(storeTxOf ctx chain p).fee | p@(_, Store.BlockRef h _) <- placed chain, h == height]
       in blockMinedAt height (fromIntegral height) (bestTime - 600 * fromIntegral (chain.chainHeight - height)) fees

{- The fake store -}

-- | Run a test with a home whose hw talks to a fake store, which serves the
-- chain held in the reference it gives the test, on every network
withChain :: Ctx -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChain ctx = withChainDownOn ctx []

-- | 'withChain', with the store down on the given networks. The best block
-- was mined three minutes before the test.
withChainDownOn :: Ctx -> [Network] -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChainDownOn ctx downs = withChainMinedAgo ctx downs 180

-- | 'withChainDownOn', with the best block of the chain mined the given
-- number of seconds before the test: more than eight hours, and a wallet
-- synced with it is behind the chain
withChainMinedAgo :: Ctx -> [Network] -> Word32 -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChainMinedAgo ctx = withChainStore ctx id

-- | 'withChain', with a host that serves the given networks only, and is
-- down on those of the second list: a question about any other network
-- gets a 404, as a host answers for a network it has no store of. The
-- public server serves four of the eight networks of a portfolio.
withChainServing :: Ctx -> [Network] -> [Network] -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChainServing ctx served downs chain test = do
  ref <- newIORef chain
  bestTime <- subtract 180 . floor <$> getPOSIXTime
  testWithApplication (return (servingOnly (served <> downs) (downOn downs (storeApp ctx (chainStore ctx ref bestTime))))) $ \port ->
    withHome $ \home -> do
      useStore home ("http://127.0.0.1:" <> show port <> "/")
      test home ref

-- | The server, with a store for the given networks only: a question
-- about another network gets a 404
servingOnly :: [Network] -> Application -> Application
servingOnly served app req respond
  | any (\net -> [Text.pack net.name] == take 1 (pathInfo req)) served = app req respond
  | otherwise = respond $ responseLBS status404 [] "Not found"

-- | 'withChain', with the store of the given networks failing once hw has
-- checked its health: the health check is answered as a store that is up
-- answers it, and every later question about those networks gets the
-- error page of a proxy in front of a store that stopped answering. hw
-- finds the network up when a command starts, and its store failing in
-- the middle of the work, which 'withChainDownOn' cannot show.
withChainFailingOn :: Ctx -> [Network] -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChainFailingOn ctx failing chain test = do
  ref <- newIORef chain
  bestTime <- subtract 180 . floor <$> getPOSIXTime
  testWithApplication (return (failingAfterHealth failing (storeApp ctx (chainStore ctx ref bestTime)))) $ \port ->
    withHome $ \home -> do
      useStore home ("http://127.0.0.1:" <> show port <> "/")
      test home ref

-- | The server, failing on the given networks after their health check:
-- any question about one of them but the health check gets the error page
-- of 'downOn'
failingAfterHealth :: [Network] -> Application -> Application
failingAfterHealth failing app req respond
  | any (\net -> [Text.pack net.name] == take 1 (pathInfo req)) failing && drop 1 (pathInfo req) /= ["health"] =
      respond $ responseLBS status502 [] "<html><body><h1>502 Bad Gateway</h1></body></html>"
  | otherwise = app req respond

-- | 'withChain', with a store that reports every address used, as a store
-- that lies does
withLyingChain :: Ctx -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withLyingChain ctx = withChainStore ctx everyAddressUsed [] 180

-- | The store, reporting every address used: one transaction, and 1000
-- satoshis received and held
everyAddressUsed :: Store -> Store
everyAddressUsed store = store {storeBalances = \addrs -> return [Store.Balance addr 1000 0 1 1 1000 | addr <- addrs]}

-- | Run a test with a home whose hw talks to a fake store serving the
-- chain, altered by the function, down on the given networks, with the
-- best block mined the given number of seconds before the test
withChainStore :: Ctx -> (Store -> Store) -> [Network] -> Word32 -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChainStore ctx alter downs ago chain test = do
  ref <- newIORef chain
  bestTime <- subtract ago . floor <$> getPOSIXTime
  testWithApplication (return (downOn downs (storeApp ctx (alter (chainStore ctx ref bestTime))))) $ \port ->
    withHome $ \home -> do
      useStore home ("http://127.0.0.1:" <> show port <> "/")
      test home ref

-- | The server, down on the given networks: every question about one of
-- them gets the error page of a proxy in front of a store that does not
-- answer
downOn :: [Network] -> Application -> Application
downOn downs app req respond
  | any (\net -> [Text.pack net.name] == take 1 (pathInfo req)) downs =
      respond $ responseLBS status502 [] "<html><body><h1>502 Bad Gateway</h1></body></html>"
  | otherwise = app req respond

-- | Run a test with a home whose hw talks to a server that answers every
-- question with the same text, as a server that is not a haskoin-store
-- would. The test gets the address of the server, as config.json has it.
withServerAnswering :: Text -> (FilePath -> String -> IO a) -> IO a
withServerAnswering text = withServerSending (BL.fromStrict (encodeUtf8 text))

-- | 'withServerAnswering' with the bytes of the answer, which the server
-- sends as it makes them: a long answer is never whole in memory
withServerSending :: BL.ByteString -> (FilePath -> String -> IO a) -> IO a
withServerSending body test =
  testWithApplication (return answering) $ \port ->
    withHome $ \home -> do
      let url = "http://127.0.0.1:" <> show port <> "/"
      useStore home url
      test home url
  where
    answering _ respond = respond $ responseLBS status200 [] body

-- | The web API of haskoin-store, as its web client asks it, answering
-- from a store: the paths, the parameters and the binary encoding of
-- haskoin-store-data. A question it does not know is a 404. The store is
-- the same on every network, as the coins of a key from before the fork
-- of Bitcoin Cash are on both chains.
storeApp :: Ctx -> Store -> Application
storeApp ctx store req respond = do
  answer <- runExceptT route
  respond $ case answer of
    Right body -> responseLBS status200 [(hContentType, "application/octet-stream")] body
    Left err -> responseLBS status404 [] (BL.fromStrict (encodeUtf8 (Text.pack err)))
  where
    route :: ExceptT String IO BL.ByteString
    route = case (requestMethod req, drop 1 (pathInfo req)) of
      ("GET", ["health"]) -> encoded healthy <$ checkHealth store
      ("GET", ["block", "best"]) -> encoded <$> storeBestBlock store
      ("GET", ["block", "latest"]) -> encoded . Store.SerialList <$> storeLatestBlocks store
      ("GET", ["transactions", "block", h]) -> do
        hash <- fromPath h
        encoded . Store.SerialList <$> storeBlockTransactions store hash
      ("GET", ["block", "height", h]) -> do
        HeightParam height <- fromPath h
        encoded . Store.SerialList <$> storeBlocksAtHeight store (fromIntegral height)
      ("GET", ["address", "balances"]) -> encoded . Store.SerialList <$> (storeBalances store =<< query "addresses")
      ("GET", ["address", a, "transactions"]) -> do
        addr <- fromPath a
        limit <- maybe 0 (\(LimitParam l) -> l) <$> optionalQuery "limit"
        encoded . Store.SerialList <$> storeAddressTxs store addr limit
      ("GET", ["address", "unspent"]) -> encoded . Store.SerialList <$> (storeUnspents store =<< query "addresses")
      ("GET", ["transactions"]) -> encoded . Store.SerialList <$> (storeTransactions store =<< query "txids")
      ("GET", ["transactions", "raw"]) -> encoded . Store.RawResultList <$> (storeRawTransactions store =<< query "txids")
      ("POST", ["transactions"]) -> do
        tx <- runGetL deserialize <$> liftIO (strictRequestBody req)
        storePublish store tx
        return $ encoded (Store.TxId (txHash tx))
      (method, path) -> throwError $ "The fake store does not answer " <> show method <> " " <> show path
    -- The network the web client names first in the path, which reads
    -- the addresses of the question
    net = fromMaybe btc (netByName . Text.unpack =<< listToMaybe (pathInfo req))
    encoded :: (Serial a) => a -> BL.ByteString
    encoded = runPutL . serialize
    fromPath :: (Param p) => Text -> ExceptT String IO p
    fromPath text = maybe (throwError $ "Unreadable path: " <> show (pathInfo req)) return (parseParam net ctx [text])
    optionalQuery :: (Param p) => BS.ByteString -> ExceptT String IO (Maybe p)
    optionalQuery name = case join (lookup name (queryString req)) of
      Nothing -> return Nothing
      Just value -> maybe (throwError $ "Unreadable parameter " <> show name) (return . Just) (parseParam net ctx (Text.splitOn "," (decodeUtf8 value)))
    query :: (Param p) => BS.ByteString -> ExceptT String IO p
    query name = do
      found <- optionalQuery name
      maybe (throwError $ "Missing parameter " <> show name) return found
    -- A store whose blocks, transactions and peers are all as they should be
    healthy = Store.HealthCheck (Store.BlockHealth 0 0 7) (Store.TimeHealth 0 3600) (Store.TimeHealth 0 3600) (Store.MaxHealth 0 10000) (Store.CountHealth 8 1) "btc" "fake" 1700000000
