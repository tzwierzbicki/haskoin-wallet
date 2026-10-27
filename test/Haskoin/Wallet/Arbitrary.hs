{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | QuickCheck generators for the wallet types, used by the property tests.
-- Generators for haskoin-core types come from "Haskoin.Util.Arbitrary".
module Haskoin.Wallet.Arbitrary where

import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Word
import Database.Persist.Sql (toSqlKey)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Util.Arbitrary
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Coin
import Haskoin.Wallet.Response
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Numeric.Natural
import Test.QuickCheck

genNatural :: Test.QuickCheck.Gen Natural
genNatural = arbitrarySizedNatural

arbitraryText :: Gen Text
arbitraryText = cs <$> (arbitrary :: Gen String)

arbitraryPositive :: (Arbitrary a, Integral a) => Gen a
arbitraryPositive = abs <$> arbitrary

arbitraryNatural :: Gen Natural
arbitraryNatural = fromIntegral <$> (arbitraryPositive :: Gen Word64)

arbitraryBlockRef :: Gen Store.BlockRef
arbitraryBlockRef =
  oneof [a, b]
  where
    a = Store.BlockRef <$> arbitraryPositive <*> arbitraryPositive
    b = Store.MemRef <$> arbitrary

arbitraryDBAccount :: Network -> Ctx -> Gen DBAccount
arbitraryDBAccount net ctx =
  DBAccount
    <$> arbitraryText
    <*> arbitraryPositive
    <*> return (cs net.name)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> (xPubExport net ctx <$> arbitraryXPubKey ctx)
    <*> arbitraryMaybe (fingerprintToText <$> arbitraryFingerprint)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryUTCTime

arbitraryDBAddress :: Network -> Gen DBAddress
arbitraryDBAddress net =
  DBAddress
    <$> (toSqlKey <$> arbitraryPositive)
    <*> arbitrary
    <*> arbitraryPositive
    <*> (fromJust . addrToText net <$> arbitraryAddress net)
    <*> arbitraryText
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryUTCTime

arbitraryAddressBalance :: Gen AddressBalance
arbitraryAddressBalance =
  AddressBalance
    <$> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive

arbitraryJsonCoin :: Network -> Gen JsonCoin
arbitraryJsonCoin net =
  JsonCoin
    <$> arbitraryOutPoint
    <*> arbitraryAddress net
    <*> arbitraryPositive
    <*> arbitraryBlockRef
    <*> arbitraryNatural
    <*> arbitraryMaybe arbitraryCoinLock
    <*> arbitrary

arbitraryCoinLock :: Gen CoinLock
arbitraryCoinLock = elements [LockedByTransaction, LockedByUser]

arbitraryPubKeyDoc :: Ctx -> Gen PubKeyDoc
arbitraryPubKeyDoc ctx =
  PubKeyDoc
    <$> arbitraryXPubKey ctx
    <*> arbitraryNetwork
    <*> arbitraryText
    <*> arbitraryMaybe arbitraryFingerprint

-- | A transaction file of Bitcoin signing data, on any network or none
arbitraryTxFile :: Ctx -> Gen TxFile
arbitraryTxFile ctx = TxFile <$> arbitraryMaybe arbitraryNetwork <*> arbitraryTxSignData btc ctx

arbitraryTxSignData :: Network -> Ctx -> Gen TxSignData
arbitraryTxSignData net ctx =
  TxSignData
    <$> arbitraryTx net ctx
    <*> resize 10 (listOf (arbitraryTx net ctx))
    <*> resize 10 (listOf arbitrarySoftPath)
    <*> resize 10 (listOf arbitrarySoftPath)
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryMaybe arbitraryFingerprint

arbitraryMyOutputs :: Gen MyOutputs
arbitraryMyOutputs =
  MyOutputs
    <$> arbitraryNatural
    <*> arbitrarySoftPath
    <*> arbitraryText

arbitraryMyInputs :: Gen MyInputs
arbitraryMyInputs =
  MyInputs
    <$> arbitraryNatural
    <*> arbitrarySoftPath
    <*> arbitraryText
    <*> resize 10 (listOf arbitraryOutPoint)

arbitraryTxType :: Gen TxType
arbitraryTxType = oneof [pure TxDebit, pure TxInternal, pure TxCredit]

arbitraryStoreOutput :: Network -> Gen Store.StoreOutput
arbitraryStoreOutput net =
  Store.StoreOutput
    <$> arbitrary
    <*> arbitraryBS1
    <*> arbitraryMaybe arbitrarySpender
    <*> arbitraryMaybe (arbitraryAddress net)

arbitrarySpender :: Gen Store.Spender
arbitrarySpender = Store.Spender <$> arbitraryTxHash <*> arbitrary

arbitraryStoreInput :: Network -> Gen Store.StoreInput
arbitraryStoreInput net =
  oneof
    [ Store.StoreCoinbase
        <$> arbitraryOutPoint
        <*> arbitrary
        <*> arbitraryBS1
        <*> listOf arbitraryBS1,
      Store.StoreInput
        <$> arbitraryOutPoint
        <*> arbitrary
        <*> arbitraryBS1
        <*> arbitraryBS1
        <*> arbitrary
        <*> listOf arbitraryBS1
        <*> arbitraryMaybe (arbitraryAddress net)
    ]

arbitraryTxSummary :: Network -> Gen TxSummary
arbitraryTxSummary net =
  TxSummary
    <$> arbitraryTxType
    <*> arbitrary
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryNatural
    <*> addressMap arbitraryMyOutputs
    <*> addressMap arbitraryNatural
    <*> resize 10 (listOf $ arbitraryStoreOutput net)
    <*> addressMap arbitraryMyInputs
    <*> addressMap arbitraryNatural
    <*> resize 10 (listOf $ arbitraryStoreInput net)
  where
    addressMap gen = Map.fromList <$> resize 10 (listOf $ (,) <$> arbitraryAddress net <*> gen)

arbitraryTxStatus :: Gen TxStatus
arbitraryTxStatus =
  oneof
    [ OnChain <$> (ChainStatus <$> arbitraryTxHash <*> arbitraryBlockRef <*> arbitraryNatural <*> arbitrary),
      Pending <$> (PendingStatus <$> arbitraryTxHash <*> arbitraryMaybe arbitraryTxHash <*> arbitrary <*> arbitrary)
    ]

arbitraryTxInfo :: Network -> Gen TxInfo
arbitraryTxInfo net = TxInfo <$> arbitraryTxSummary net <*> arbitraryTxStatus

arbitrarySyncRes :: Network -> Ctx -> Gen SyncRes
arbitrarySyncRes net ctx =
  SyncRes
    <$> arbitraryDBAccount net ctx
    <*> arbitraryPositive
    <*> arbitraryBlockHash
    <*> arbitrary
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural

arbitraryResponse :: Network -> Ctx -> Gen Response
arbitraryResponse net ctx =
  oneof
    [ ResponseError <$> arbitraryText,
      ResponseMnemonic
        <$> arbitraryText
        <*> resize 12 (listOf arbitraryText)
        <*> resize 12 (listOf $ resize 12 $ listOf arbitraryText),
      ResponseAccount <$> arbitraryDBAccount net ctx <*> arbitraryPositive,
      ResponseAccResult
        <$> arbitraryDBAccount net ctx
        <*> arbitrary
        <*> arbitraryText,
      ResponseFile <$> (cs <$> arbitraryText),
      ResponseAccounts <$> resize 20 (listOf $ (,) <$> arbitraryDBAccount net ctx <*> arbitraryPositive),
      ResponseAddress <$> arbitraryDBAccount net ctx <*> arbitraryDBAddress net <*> arbitraryPositive,
      ResponseAddresses
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ (,) <$> arbitraryDBAddress net <*> arbitraryPositive),
      ResponseTxs
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ arbitraryTxInfo net),
      ResponseTx <$> arbitraryDBAccount net ctx <*> arbitraryTxInfo net,
      ResponseOutsideTx net <$> arbitraryTxInfo net,
      ResponseSentTx <$> arbitraryNetwork <*> arbitraryTxHash,
      ResponseDeleteTx
        <$> arbitraryTxHash
        <*> arbitraryNatural
        <*> arbitraryNatural,
      ResponseCoins
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ arbitraryJsonCoin net),
      ResponseSync
        <$> (resize 10 $ listOf $ arbitrarySyncRes net ctx),
      ResponseRecover
        <$> arbitraryNetwork
        <*> arbitraryNatural
        <*> (resize 10 $ listOf $ arbitrarySyncRes net ctx),
      ResponseVersion
        <$> arbitraryText
        <*> arbitraryText
    ]

arbitraryKeyBytes :: Gen KeyBytes
arbitraryKeyBytes = KeyBytes <$> arbitraryBS

arbitraryConfig :: Gen Config
arbitraryConfig =
  Config
    <$> arbitraryText
    <*> count
    <*> count
    <*> count
    <*> count
    <*> count
    <*> arbitrary
  where
    -- Every count of the configuration is at least one
    count = (+ 1) <$> arbitraryNatural
