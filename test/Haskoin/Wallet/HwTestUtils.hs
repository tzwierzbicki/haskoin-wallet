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
    setChangeBack,
    hw,
    hwJson,
    -- * The chain of the fake store
    Chain (..),
    mine,
    inMempool,
    withChain,
    withChainDownOn,
    withServerAnswering,
  )
where

import Control.Monad (filterM, join)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Json
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
import Haskoin.Wallet.Response (Response)
import Haskoin.Wallet.Store (Store (..))
import Haskoin.Wallet.TestUtils (addressScript, blockAt, blockMinedAt, otherAddr)
import Network.HTTP.Types (hContentType, status200, status404, status502)
import Network.Wai (Application, pathInfo, queryString, requestMethod, responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode)
import System.FilePath (takeDirectory, (</>))
import System.Process.Typed (byteStringInput, proc, readProcess, setEnv, setStdin, setWorkingDir)
import System.Timeout (timeout)
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

-- | Set whether preparetx sends the change back to an address it spends
-- from without --change-back, in the configuration file of a home
setChangeBack :: FilePath -> Bool -> IO ()
setChangeBack home on = editConfig home (KeyMap.insert "change-back" (Json.Bool on))

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
hw home args typed = do
  binary <- hwBinary
  let process =
        setStdin (byteStringInput (BL.fromStrict (encodeUtf8 (Text.unlines typed))))
          . setEnv [("HOME", home)]
          . setWorkingDir home
          $ proc binary args
  done <- timeout 60000000 (readProcess process)
  case done of
    Nothing -> fail $ "hw " <> unwords args <> " did not finish within a minute"
    Just (code, out, err) -> return $ Run code (decodeUtf8 (BL.toStrict out)) (decodeUtf8 (BL.toStrict err))

-- | Run hw with --json, and its answer, which must read as one
hwJson :: FilePath -> [String] -> [Text] -> IO Response
hwJson home args typed = do
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

-- | The address an output pays, if a standard one
addressOf :: Ctx -> TxOut -> Maybe Address
addressOf ctx out = either (const Nothing) Just (scriptToAddressBS ctx out.script)

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
             in kept {chainTxs = kept.chainTxs <> [(tx, Nothing)]}
    }
  where
    now = liftIO (readIORef ref)
    -- A block of the main chain, its hash its own at each height
    blockOf height = blockAt height (fromIntegral height)
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

-- | 'withChain', with the store down on the given networks
withChainDownOn :: Ctx -> [Network] -> Chain -> (FilePath -> IORef Chain -> IO a) -> IO a
withChainDownOn ctx downs chain test = do
  ref <- newIORef chain
  -- The best block was mined three minutes before the test
  bestTime <- subtract 180 . floor <$> getPOSIXTime
  testWithApplication (return (downOn downs (storeApp ctx (chainStore ctx ref bestTime)))) $ \port ->
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
withServerAnswering text test =
  testWithApplication (return answering) $ \port ->
    withHome $ \home -> do
      let url = "http://127.0.0.1:" <> show port <> "/"
      useStore home url
      test home url
  where
    answering _ respond = respond $ responseLBS status200 [] (BL.fromStrict (encodeUtf8 text))

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
      ("GET", ["health"]) -> encoded healthy <$ storeHealth store
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
