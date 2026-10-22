{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.TxSweepSpec where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Serialize as S
import Data.Text (Text)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.FileIO (TxSignData (..))
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.TxInfo (TxInfo (..), TxType (..))
import Haskoin.Wallet.TxSweep
import Haskoin.Wallet.TxVectors
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
      signed.txSignDataSigned `shouldBe` True
      -- The wallet receives one of the two outputs (745600) and gives up its coin (500000)
      info.txInfoType `shouldBe` TxCredit
      info.txInfoAmount `shouldBe` 245600
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
          unsigned = TxSignData tx [walletFunding] [firstExternal] [] Nothing False
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
    build = buildWith 10 546
    buildWith feeByte dust to = buildSweepSignData btc ctx seed found to feeByte dust
    spentBy :: TxSignData -> [OutPoint]
    spentBy tsd = (.outpoint) <$> tsd.txSignDataTx.inputs
