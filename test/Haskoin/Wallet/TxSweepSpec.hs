{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskoin.Wallet.TxSweepSpec where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
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
      ((.value) <$> tsd.txSignDataTx.outputs) `shouldMatchList` [745910, 745910]
    it "records the dependent transactions and the wallet's paths" $ do
      tsd <- expectRight $ build destinations
      tsd.txSignDataInputs `shouldBe` [keyFunding, walletFunding]
      tsd.txSignDataInputPaths `shouldBe` [firstExternal]
      tsd.txSignDataOutputPaths `shouldBe` [secondExternal]
    it "builds a transaction the sweep signer accepts" $ do
      tsd <- expectRight $ build destinations
      (signed, info) <- expectRight $ signTxWithKeys btc ctx tsd accountPub [sweptKey, walletKey]
      signed.txSignDataSigned `shouldBe` True
      -- The wallet receives one of the two outputs (745910) and gives up its coin (500000)
      info.txInfoType `shouldBe` TxCredit
      info.txInfoAmount `shouldBe` 245910
    it "refuses to sweep without coins" $
      buildSweepSignData btc ctx seed (SweepCoins [] [] Map.empty) destinations 10 546
        `shouldBe` Left "There are no coins to sweep in those addresses"
    it "refuses to sweep without destinations" $
      build [] `shouldBe` Left "No addresses to sweep to"
    it "refuses a fee larger than the coins" $
      buildWith 10000 546 destinations `shouldBe` Left "Could not find a sweep solution: fee is too large"
    -- Each destination gets 745910: the dust limit right below, at and
    -- right above that amount
    it "accepts outputs above the dust value" $
      buildWith 10 745909 destinations `shouldBe` build destinations
    it "accepts outputs exactly at the dust value" $
      buildWith 10 745910 destinations `shouldBe` build destinations
    it "refuses outputs below the dust value" $
      buildWith 10 745911 destinations `shouldBe` Left "Outputs are smaller than the dust value"
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
          walletAddrs = Map.fromList [(externalAddr 0, firstExternal), (externalAddr 1, secondExternal)]
        }
    -- The sweep: 10 satoshis per byte, dust at 546. The estimated size of a
    -- transaction with 5 inputs and 2 outputs is 10 + 5 * 148 + 2 * 34 = 818
    -- bytes, so the fee is 8180 and each destination gets
    -- (1500000 - 8180) / 2 = 745910.
    destinations = [otherAddr 0, externalAddr 1]
    build = buildWith 10 546
    buildWith feeByte dust to = buildSweepSignData btc ctx seed found to feeByte dust
    spentBy :: TxSignData -> [OutPoint]
    spentBy tsd = (.outpoint) <$> tsd.txSignDataTx.inputs
