{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.TxSweepSpec where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Address
import Haskoin.Wallet.Coin
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.Store
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (TxInfo (..), TxSummary (..), TxType (..))
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (parseTxSignData)
import Haskoin.Wallet.TxSweep
import Haskoin.Wallet.TxVectors
import Haskoin.Wallet.Util (Page (Page))
import System.Random (mkStdGen)
import Test.Hspec

-- The signing vectors live in JSON files in test/data/tx-sweep (see the
-- README.md file there). Every .json file is loaded, so new vectors can be
-- added without changing this module.
spec :: Spec
spec = do
  txVectorSpec "test/data/tx-sweep" "Sweep signing test vectors" sweepSigner
  prepareContext $ \ctx -> do
    keyAddressesSpec ctx
    sweepSizeSpec ctx
    sweepBuilderSpec ctx
    givenCoinsSpec ctx
    fetchSpec ctx
    sweepIntoWalletSpec ctx

-- | Sign with the sweep keys and the account public key, like signsweep does
sweepSigner :: VectorSigner
sweepSigner ctx v = do
  specs <- maybe (Left "not a sweep vector: it has no sweep keys") Right v.vecSweepKeys
  accountKey <- prefixError "account" $ accountSigningKey v.vecNetwork ctx v.vecAccount
  keys <- prefixError "sweep key" $ mapM (vectorKey ctx v.vecNetwork) specs
  let accountPub = deriveXPubKey ctx accountKey
  return $ \c -> signTxWithKeys v.vecNetwork c v.vecSignData accountPub keys

{- Key addresses -}

-- | The swept key of the tests below (secret 0x11), and its addresses on
-- Bitcoin and on Bitcoin testnet, computed independently with the Python
-- primitives of test/data/message-signing/tools/btc.py
sweptKey :: SecKey
sweptKey = (.key) . fromJust $ fromWif btc "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgd9M7rFU7Bqcefezr"

sweptKeyAddrsText, sweptKeyTestnetAddrsText :: [Text]
sweptKeyAddrsText =
  [ "18FtYoEJipP5bCJnjgWdeeEbxSvozSjEUA", -- P2PKH, compressed key
    "1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1", -- P2PKH, uncompressed key
    "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2", -- P2WPKH
    "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p" -- P2SH-P2WPKH
  ]
sweptKeyTestnetAddrsText =
  [ "mnmqqrKHXqpLNJnQTFV1UZSvpSXWsS5QQm",
    "n1TgqBVN1XVMWeFisDcjuMst5s95Bo2bMA",
    "tb1qf7vmha6hqljyhs405efn0hkwj98gz74vdud2ye",
    "2N9eX4fbRW1MPUiAang9UMrTDKuYpTN1p4G"
  ]

sweptKeyAddrs :: [Address]
sweptKeyAddrs = fromJust . textToAddr btc <$> sweptKeyAddrsText

-- | The P2PKH address of the uncompressed encoding of the swept key
sweptKeyUncompressedAddr :: Address
sweptKeyUncompressedAddr = fromJust $ textToAddr btc "1LwjY8QPCW46jXn79eeN5SfZDsYNB9qDj1"

keyAddressesSpec :: Ctx -> Spec
keyAddressesSpec ctx =
  describe "Addresses of a swept key" $ do
    it "are its P2PKH, P2WPKH and P2SH-P2WPKH addresses on Bitcoin" $
      addressesOn btc `shouldBe` sweptKeyAddrsText
    it "use the testnet encodings on Bitcoin testnet" $
      addressesOn btcTest `shouldBe` sweptKeyTestnetAddrsText
    it "are only the two P2PKH addresses on Bitcoin Cash" $
      keyAddresses bch ctx sweptKey `shouldBe` take 2 sweptKeyAddrs
  where
    addressesOn :: Network -> [Text]
    addressesOn net = fromJust . addrToText net <$> keyAddresses net ctx sweptKey

{- Sweep size -}

-- The coins: one at each of the four Bitcoin addresses of the swept key,
-- and a P2PK coin of its compressed public key. The sizes are written out
-- from the byte counts in the documentation of the estimator: a
-- transaction is 10 bytes of overhead around its inputs and outputs, a
-- P2PKH output is 34 bytes.
sweepSizeSpec :: Ctx -> Spec
sweepSizeSpec ctx =
  describe "Sweep size" $ do
    it "counts a P2PKH input with a compressed key as 148 bytes" $
      sizeOf [compressedCoin] [otherAddr 0] `shouldBe` Right 192
    it "counts a P2PKH input with an uncompressed key as 180 bytes" $
      sizeOf [uncompressedCoin] [otherAddr 0] `shouldBe` Right 224
    it "counts a P2PK input as 114 bytes" $
      sizeOf [p2pkCoin] [otherAddr 0] `shouldBe` Right 158
    it "counts a P2WPKH input as 41 bytes, with a 108-byte witness after the 2 marker bytes" $
      sizeOf [witnessCoin] [otherAddr 0] `shouldBe` Right 195
    it "counts a P2SH-P2WPKH input as 64 bytes, with the same witness" $
      sizeOf [nestedCoin] [otherAddr 0] `shouldBe` Right 218
    it "counts an empty witness for a legacy input next to a SegWit one" $
      -- 10 + 148 + 41 + 34, then 2 + 1 + 108 of witness data
      sizeOf [compressedCoin, witnessCoin] [otherAddr 0] `shouldBe` Right 344
    it "sizes the outputs by their scripts" $
      -- 10 + 148, then P2PKH 34, P2SH 32, P2WPKH 31, P2WSH 43 and P2TR 43
      sizeOf [compressedCoin] [otherAddr 0, scriptAddr, witnessAddr, witnessScriptAddr, taprootAddr] `shouldBe` Right 341
    it "refuses a coin the sweep cannot sign" $
      sizeOf [storeCoin ctx (fakeTxHash 3, 0) Nothing witnessScriptAddr 100000] [otherAddr 0]
        `shouldBe` Left "Unsupported output script for the coin at bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
  where
    sizeOf swept to =
      sweepTxSize btc ctx (SweepCoins swept [] Map.empty [sweptKeyUncompressedAddr]) (fromJust . addressToOutput <$> to)
    compressedCoin = storeCoin ctx (fakeTxHash 1, 0) Nothing (fromJust $ textToAddr btc "18FtYoEJipP5bCJnjgWdeeEbxSvozSjEUA") 100000
    uncompressedCoin = storeCoin ctx (fakeTxHash 1, 1) Nothing sweptKeyUncompressedAddr 200000
    witnessCoin = storeCoin ctx (fakeTxHash 1, 2) Nothing (fromJust $ textToAddr btc "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2") 300000
    nestedCoin = storeCoin ctx (fakeTxHash 1, 3) Nothing (fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p") 400000
    -- A coin paid to the public key itself, which the store lists under the
    -- P2PKH address of the key
    p2pkCoin =
      Store.Unspent
        { Store.block = Store.MemRef 0,
          Store.outpoint = OutPoint (fakeTxHash 2) 0,
          Store.value = 100000,
          Store.script = marshal ctx (PayPK sweptPub),
          Store.address = Just (pubKeyAddr ctx sweptPub)
        }
    sweptPub = derivePublicKey ctx (wrapSecKey True sweptKey)
    -- Destinations of every kind: the P2SH and P2WPKH addresses of the swept
    -- key, and the P2WSH and P2TR examples of BIP173 and BIP350
    scriptAddr = fromJust $ textToAddr btc "3J6JzvfPtYr3GvY37YXbjuTx7ZLedG3C9p"
    witnessAddr = fromJust $ textToAddr btc "bc1qf7vmha6hqljyhs405efn0hkwj98gz74v86kel2"
    witnessScriptAddr = fromJust $ textToAddr btc "bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3"
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"

{- Sweep builder -}

-- The scenario: the swept key holds a coin at each of its four Bitcoin
-- addresses, and the test wallet holds one coin at its first external
-- address, whose key is given as well. Everything is swept to an address of
-- another wallet and to the test wallet's second external address.
sweepBuilderSpec :: Ctx -> Spec
sweepBuilderSpec ctx =
  describe "Sweep builder" $ do
    it "spends every coin once and pays every destination" $ do
      tsd <- expectRight $ build destinations
      spentBy tsd `shouldMatchList` ((.outpoint) <$> allCoins)
      length tsd.txSignDataTx.outputs `shouldBe` length destinations
    it "splits what is left after the fee evenly" $ do
      tsd <- expectRight $ build destinations
      ((.value) <$> tsd.txSignDataTx.outputs) `shouldMatchList` [745600, 745600]
    it "records the dependent transactions and the wallet's paths" $ do
      tsd <- expectRight $ build destinations
      tsd.txSignDataInputs `shouldBe` [keyFunding, walletFunding]
      tsd.txSignDataInputPaths `shouldBe` [firstExternal]
      tsd.txSignDataOutputPaths `shouldBe` [secondExternal]
      tsd.txSignDataFeeRate `shouldBe` Just 10
    it "builds a transaction the sweep signer accepts" $ do
      tsd <- expectRight $ build destinations
      (signed, info) <- expectRight $ signTxWithKeys btc ctx tsd accountPub [sweptKey, walletKey]
      isSigned signed.txSignDataTx `shouldBe` True
      -- The wallet receives one of the two outputs (745600) and gives up its coin (500000)
      info.txInfoSummary.summaryType `shouldBe` TxCredit
      info.txInfoSummary.summaryAmount `shouldBe` 245600
    it "matches the size of the signed transaction, part by part" $ do
      tsd <- expectRight $ build destinations
      (signed, _) <- expectRight $ signTxWithKeys btc ctx tsd accountPub [sweptKey, walletKey]
      let tx = signed.txSignDataTx
          legacy = tx {witness = []}
      -- Signed, the transaction is the 880 bytes estimated, every signature
      -- being 72 bytes: 659 bytes outside the witnesses and 221 of witness
      -- data. The inputs, in the order the builder shuffled them, are the
      -- P2WPKH coin (empty script), the uncompressed P2PKH one (139-byte
      -- script), the P2SH-P2WPKH one (23) and the two compressed P2PKH ones
      -- (107); the witness of a SegWit input holds the signature and the
      -- public key with a length byte each (72 + 33 + 2).
      ( BS.length (S.encode tx),
        BS.length (S.encode legacy),
        BS.length . (.script) <$> tx.inputs,
        (\stack -> length stack + sum (BS.length <$> stack)) <$> tx.witness
        )
        `shouldBe` (880, 659, [0, 139, 23, 107, 107], [107, 0, 107, 0, 0])
    it "refuses to sweep without coins" $
      buildSweepSignData btc ctx seed (SweepCoins [] [] Map.empty []) destinations 10 546
        `shouldBe` Left "There are no coins to sweep in those addresses"
    it "refuses to sweep without destinations" $
      build [] `shouldBe` Left "No addresses to sweep to"
    it "builds a sweep to addresses outside the wallet, P2PKH and Taproot, which the signer accepts" $ do
      -- 10 bytes of overhead, the inputs (581), a P2PKH output (34), a
      -- Taproot output (43) and the witness data (221): 889 bytes, a fee of
      -- 8890, and (1500000 - 8890) / 2 = 745555 for each destination
      tsd <- expectRight $ build [otherAddr 0, taprootAddr]
      ((.value) <$> tsd.txSignDataTx.outputs) `shouldBe` [745555, 745555]
      tsd.txSignDataOutputPaths `shouldBe` []
      (_, info) <- expectRight $ signTxWithKeys btc ctx tsd accountPub [sweptKey, walletKey]
      info.txInfoSummary.summaryFee `shouldBe` 8890
      Map.toList info.txInfoSummary.summaryOtherOutputs `shouldBe` [(otherAddr 0, 745555), (taprootAddr, 745555)]
      -- A sweep has no change: nothing of the fee is change
      info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
    it "is reported before signing with the size estimated without the keys" $ do
      -- The report cannot know that one coin belongs to an uncompressed key
      -- and counts its input as 148 bytes rather than 180: 848 bytes for the
      -- 8800 of fee the builder set on 880
      tsd <- expectRight $ build destinations
      info <- expectRight $ parseTxSignData ctx accountPub tsd
      (info.txInfoSummary.summarySize, info.txInfoSummary.summaryFee, info.txInfoSummary.summaryFeeByte, info.txInfoSummary.summaryChangeInFee) `shouldBe` (848, 8800, 10, Nothing)
    it "refuses a fee larger than the coins" $
      buildWith 10000 546 destinations `shouldBe` Left "Could not find a sweep solution: fee is too large"
    -- Each destination gets 745600: the dust limit right below, at and
    -- right above that amount
    it "accepts outputs above the dust value" $
      buildWith 10 745599 destinations `shouldBe` build destinations
    it "accepts outputs exactly at the dust value" $
      buildWith 10 745600 destinations `shouldBe` build destinations
    it "refuses outputs below the dust value" $
      buildWith 10 745601 destinations `shouldBe` Left "Outputs are smaller than the dust value"
    it "refuses a fee rate of zero" $
      buildWith 0 546 destinations `shouldBe` Left "The fee rate must be at least 1 satoshi per byte"
    it "refuses to sign a sweep without fee" $ do
      -- The wallet's coin of 500000 is swept in full
      let tx = spendingTx ctx [(txHash walletFunding, 0)] [(otherAddr 0, 500000)]
          unsigned = TxSignData tx [walletFunding] [firstExternal] [] Nothing
      signTxWithKeys btc ctx unsigned accountPub [walletKey] `shouldBe` Left "The transaction pays no fee"
  where
    seed = mkStdGen 0
    -- The wallet: the test wallet of TestUtils, with its first two external addresses
    accountPub = testAccountPub ctx
    firstExternal = Deriv :/ 0 :/ 0 :: SoftPath
    secondExternal = Deriv :/ 0 :/ 1 :: SoftPath
    walletKey = (derivePath ctx firstExternal testAccountKey).key
    -- The coins: 100000 to 400000 at the four addresses of the swept key, and 500000 of the wallet's
    keyFunding = fundingTx ctx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
    walletFunding = fundingTx ctx 2 [(externalAddr 0, 500000)]
    allCoins :: [Store.Unspent]
    allCoins = coinsOf ctx keyFunding <> coinsOf ctx walletFunding
    found =
      SweepCoins
        { coins = allCoins,
          transactions = [keyFunding, walletFunding],
          walletAddrs = Map.fromList [(externalAddr 0, firstExternal), (externalAddr 1, secondExternal)],
          uncompressedAddrs = [sweptKeyUncompressedAddr]
        }
    -- The sweep: 10 satoshis per byte, dust at 546. The estimated size is
    -- 10 bytes of overhead, the inputs (148 + 180 + 41 + 64 + 148 = 581),
    -- two P2PKH outputs (68) and the witness data (2 + 1 + 1 + 108 + 108 + 1
    -- = 221): 880 bytes, so the fee is 8800 and each destination gets
    -- (1500000 - 8800) / 2 = 745600.
    destinations = [otherAddr 0, externalAddr 1]
    -- The Taproot example address of BIP350
    taprootAddr = fromJust $ textToAddr btc "bc1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqzk5jj0"
    build = buildWith 10 546
    buildWith feeByte dust to = buildSweepSignData btc ctx seed found to feeByte dust
    spentBy :: TxSignData -> [OutPoint]
    spentBy tsd = (.outpoint) <$> tsd.txSignDataTx.inputs

{- Coins given by outpoint -}

-- The store's view of the transaction funding the swept key's four
-- addresses, first in block 800000, with its third output spent
givenCoinsSpec :: Ctx -> Spec
givenCoinsSpec ctx =
  describe "The coins of a sweep given by outpoint" $ do
    it "reads them from their transactions, in the order given" $
      coinsOfTransactions [(txHash keyFunding, 1), (txHash keyFunding, 0)] [storeFunding]
        `shouldBe` Right
          [ storeCoin ctx (txHash keyFunding, 1) (Just 800000) (sweptKeyAddrs !! 1) 200000,
            storeCoin ctx (txHash keyFunding, 0) (Just 800000) (sweptKeyAddrs !! 0) 100000
          ]
    it "refuses a coin of a transaction the store does not know" $
      coinsOfTransactions [(fakeTxHash 7, 1)] [storeFunding]
        `shouldBe` Left "Coin 0000000000000000000000000000000000000000000000000000000000000007 1 is unknown to the store"
    it "refuses a coin of a transaction the store reports as deleted" $
      coinsOfTransactions [(txHash keyFunding, 0)] [replaced storeFunding]
        `shouldBe` Left ("Coin " <> fundingHex <> " 0 is unknown to the store")
    it "refuses an output index past the last output" $
      coinsOfTransactions [(txHash keyFunding, 4)] [storeFunding]
        `shouldBe` Left ("Coin " <> fundingHex <> " 4 does not exist: the transaction has 4 outputs")
    it "refuses a spent output" $
      coinsOfTransactions [(txHash keyFunding, 2)] [storeFunding]
        `shouldBe` Left ("Coin " <> fundingHex <> " 2 is already spent")
  where
    keyFunding = fundingTx ctx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
    fundingHex = cs $ txHashToHex $ txHash keyFunding
    storeFunding =
      Store.Transaction
        { Store.block = Store.BlockRef 800000 0,
          Store.version = 1,
          Store.locktime = 0,
          Store.inputs = [],
          Store.outputs =
            [ Store.StoreOutput 100000 (addressScript ctx (sweptKeyAddrs !! 0)) Nothing (Just (sweptKeyAddrs !! 0)),
              Store.StoreOutput 200000 (addressScript ctx (sweptKeyAddrs !! 1)) Nothing (Just (sweptKeyAddrs !! 1)),
              Store.StoreOutput 300000 (addressScript ctx (sweptKeyAddrs !! 2)) (Just (Store.Spender (fakeTxHash 9) 0)) (Just (sweptKeyAddrs !! 2)),
              Store.StoreOutput 400000 (addressScript ctx (sweptKeyAddrs !! 3)) Nothing (Just (sweptKeyAddrs !! 3))
            ],
          Store.deleted = False,
          Store.rbf = False,
          Store.timestamp = 1700000000,
          Store.txid = txHash keyFunding,
          Store.size = 226,
          Store.weight = 904,
          Store.fee = 1000
        }

{- Fetching the coins -}

-- The store: the swept key holds a coin at each of its four Bitcoin
-- addresses, 100000 to 400000, and the test wallet one of 500000 at its
-- first external address. The store knows both funding transactions, raw
-- and as it reports them.
fetchSpec :: Ctx -> Spec
fetchSpec ctx =
  describe "Fetching the coins of a sweep" $ do
    it "fetches the coins at the given addresses and the transactions they come from" $
      inDatabaseE $ do
        accId <- account
        found <- fetchSweepCoins btc ctx (toStore store) accId [] (take 2 sweptKeyAddrs) [] [otherAddr 0]
        liftIO $ summary found `shouldBe` ([(0, 100000), (1, 200000)], [keyFunding], Map.empty, [])
    it "asks the store for every address of a given key and notes its uncompressed address" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        found <- fetchSweepCoins btc ctx (notingUnspentRequests asked (toStore store)) accId [sweptKey] [] [] [otherAddr 0]
        liftIO $ do
          readIORef asked `shouldReturn` [sweptKeyAddrs]
          summary found `shouldBe` ([(0, 100000), (1, 200000), (2, 300000), (3, 400000)], [keyFunding], Map.empty, [sweptKeyUncompressedAddr])
    it "takes the coins given by outpoint from their transactions, each coin once" $
      inDatabaseE $ do
        accId <- account
        found <- fetchSweepCoins btc ctx (toStore store) accId [] [sweptKeyAddrs !! 0] [(txHash keyFunding, 0), (txHash keyFunding, 3)] [otherAddr 0]
        liftIO $ summary found `shouldBe` ([(0, 100000), (3, 400000)], [keyFunding], Map.empty, [])
    it "records the paths of the wallet's addresses among the coins and the destinations" $
      inDatabaseE $ do
        accId <- account
        found <- fetchSweepCoins btc ctx (toStore store) accId [] [externalAddr 0, sweptKeyAddrs !! 0] [] [externalAddr 1, otherAddr 0]
        liftIO $ do
          ((.outpoint.hash) <$> found.coins) `shouldBe` [txHash keyFunding, txHash walletFunding]
          found.transactions `shouldBe` [keyFunding, walletFunding]
          found.walletAddrs `shouldBe` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0), (externalAddr 1, Deriv :/ 0 :/ 1)]
    it "finds nothing at an address without coins and asks for no raw transaction" $
      inDatabaseE $ do
        accId <- account
        asked <- liftIO $ newIORef []
        found <- fetchSweepCoins btc ctx (notingRawRequests asked (toStore store)) accId [] [otherAddr 5] [] [otherAddr 0]
        liftIO $ do
          summary found `shouldBe` ([], [], Map.empty, [])
          readIORef asked `shouldReturn` []
    it "refuses to fetch without keys, addresses or coins" $
      inDatabaseE $ do
        accId <- account
        lift $
          dbShouldFailWith "No private keys, addresses or coins to sweep from" $
            fetchSweepCoins btc ctx (toStore store) accId [] [] [] [otherAddr 0]
    it "refuses a store that does not return the transaction a coin comes from" $
      inDatabaseE $ do
        accId <- account
        lift $
          dbShouldFailWith "The store was asked for 1 raw transactions but returned 0" $
            fetchSweepCoins btc ctx (toStore store {fakeRawTransactions = []}) accId [] [sweptKeyAddrs !! 0] [] [otherAddr 0]
    it "refuses a store that is not healthy" $
      inDatabaseE $ do
        accId <- account
        let unhealthy = (toStore store) {storeHealth = throwError "The indexer health check has failed"}
        lift $
          dbShouldFailWith "The indexer health check has failed" $
            fetchSweepCoins btc ctx unhealthy accId [] [sweptKeyAddrs !! 0] [] [otherAddr 0]
  where
    -- The test wallet with its first two external addresses
    account = do
      (accId, _) <- testNewAcc ctx "main"
      _ <- genExtAddress ctx def accId ""
      _ <- genExtAddress ctx def accId ""
      return accId
    keyFunding = fundingTx ctx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
    walletFunding = fundingTx ctx 2 [(externalAddr 0, 500000)]
    store =
      emptyStore
        { fakeUnspents = coinsOf ctx keyFunding <> coinsOf ctx walletFunding,
          fakeTransactions = [payoutTx ctx (txHash keyFunding) (Store.BlockRef 800000 0) (zip sweptKeyAddrs [100000, 200000, 300000, 400000])],
          fakeRawTransactions = [(txHash keyFunding, keyFunding), (txHash walletFunding, walletFunding)]
        }
    -- What was fetched: the coins as (output index, value), the transactions,
    -- the wallet's paths and the uncompressed addresses
    summary :: SweepCoins -> ([(Word32, Word64)], [Tx], Map.Map Address SoftPath, [Address])
    summary found = ([(c.outpoint.index, c.value) | c <- found.coins], found.transactions, found.walletAddrs, found.uncompressedAddrs)

{- Sweeping into the wallet -}

-- The original use of a sweep, as the commands run it: coins of an outside
-- key are swept to an address of the wallet, the transaction waits among
-- the pending transactions, is signed there by its nosigHash with the
-- outside key, and the signed one takes its place.
sweepIntoWalletSpec :: Ctx -> Spec
sweepIntoWalletSpec ctx =
  describe "Sweeping into the wallet" $ do
    it "keeps the sweep pending, signs it in place and reports the credit" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx def accId ""
        _ <- genExtAddress ctx def accId ""
        -- preparesweep: the coins of the swept key, 100000 to 400000, to the
        -- wallet's second external address and to an outside address. The
        -- wallet holds no coin, so none is locked.
        found <- SweepCoins (coinsOf ctx keyFunding) [keyFunding] <$> walletPaths btc accId [externalAddr 1, otherAddr 0] <*> pure [sweptKeyUncompressedAddr]
        tsd <- liftEither $ buildSweepSignData btc ctx (mkStdGen 0) found [externalAddr 1, otherAddr 0] 10 546
        let pendingHash = nosigHash tsd.txSignDataTx
        importPendingTx btc ctx accId tsd `dbShouldBeE` pendingHash
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, False, False, False))]
        coinPage btc accId (Page 5 0) `dbShouldBeE` []
        -- signsweep by nosigHash, with the outside key only
        held <- requirePendingTx pendingHash
        liftIO $ held.pendingTxOnline `shouldBe` False
        (signed, info) <- liftEither $ signTxWithKeys btc ctx held.pendingTxData accountPub [sweptKey]
        importPendingTx btc ctx accId signed `dbShouldBeE` pendingHash
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, False, False))]
        -- The wallet receives one of the two outputs: 10 bytes of overhead,
        -- inputs of 148 + 180 + 41 + 64, two P2PKH outputs and 2 + 1 + 1 +
        -- 108 + 108 of witness data make 731 bytes, a fee of 7310, so
        -- (1000000 - 7310) / 2 = 496345 each
        liftIO $ (info.txInfoSummary.summaryType, info.txInfoSummary.summaryAmount, info.txInfoSummary.summaryFee) `shouldBe` (TxCredit, 496345, 7310)
        liftIO $ Map.keys info.txInfoSummary.summaryMyOutputs `shouldBe` [externalAddr 1]
        -- Paid into the wallet or not, a sweep has no change
        liftIO $ info.txInfoSummary.summaryChangeInFee `shouldBe` Nothing
        -- sendtx marks it online, the sync retires it once the store lists it
        lift (setPendingTxOnline pendingHash) `dbShouldBeE` 1
        ((pendingFlags <$>) <$> pendingTxPage ctx accId (Page 5 0))
          `dbShouldBeE` [Just ((pendingHash, True, True, False))]
    it "refuses to keep a transaction that does not involve the wallet" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx "main"
        _ <- genExtAddress ctx def accId ""
        let tx = spendingTx ctx [(txHash keyFunding, 0)] [(otherAddr 0, 90000)]
            outside = TxSignData tx [keyFunding] [] [] (Just 10)
        lift $
          dbShouldFailWith "The transaction does not involve this wallet" $
            importPendingTx btc ctx accId outside
  where
    accountPub = testAccountPub ctx
    keyFunding = fundingTx ctx 1 (zip sweptKeyAddrs [100000, 200000, 300000, 400000])
