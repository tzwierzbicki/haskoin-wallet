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
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Labels (AccountImport (..), ImportReport (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..))
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tx.SignData
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
    <*> elements [44, 84]
    <*> return (cs net.name)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> (xPubExport net ctx <$> arbitraryXPubKey ctx)
    <*> arbitraryMaybe (fingerprintToText <$> arbitraryFingerprint)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryUTCTime

-- | The path of an address on one of the two chains of an account: /0/i
-- or /1/i, with i below 2^31
arbitraryChainPath :: Gen SoftPath
arbitraryChainPath = chainPath <$> arbitrary <*> choose (0, 0x7fffffff)

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

-- | The balance of the coins at an address: what can be spent now is part
-- of the total
arbitraryCoinBalance :: Gen CoinBalance
arbitraryCoinBalance = do
  spendable <- choose (0, 2100000000000000)
  unspendable <- choose (0, 2100000000000000)
  return $ CoinBalance (spendable + unspendable) spendable

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
    <*> arbitraryText
    <*> arbitraryText
    <*> arbitrary

arbitraryCoinLock :: Gen CoinLock
arbitraryCoinLock = elements [LockedByTransaction, LockedByUser]

arbitraryLeftCoin :: Gen LeftCoin
arbitraryLeftCoin =
  LeftCoin
    <$> arbitraryOutPoint
    <*> arbitraryPositive
    <*> elements [WorthLessThanFee, Locked LockedByUser, Locked LockedByTransaction, NotSpendableYet, NotGiven]
    <*> arbitraryText

-- | The labels of a few transactions, by id
arbitraryTxidLabels :: Gen (Map.Map TxHash Text)
arbitraryTxidLabels = Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryTxHash <*> arbitraryText))

arbitraryPubKeyDoc :: Ctx -> Gen PubKeyDoc
arbitraryPubKeyDoc ctx =
  PubKeyDoc
    <$> arbitraryXPubKey ctx
    <*> arbitraryNetwork
    <*> arbitraryText
    <*> arbitraryMaybe arbitraryFingerprint

arbitraryTxSignData :: Network -> Ctx -> Gen TxSignData
arbitraryTxSignData net ctx =
  TxSignData
    <$> arbitraryTx net ctx
    <*> resize 10 (listOf (arbitraryTx net ctx))
    <*> resize 10 (listOf arbitrarySoftPath)
    <*> resize 10 (listOf arbitrarySoftPath)
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryMaybe arbitraryFingerprint
    <*> resize 3 (listOf arbitraryTxHash)
    <*> arbitraryText

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
    <*> arbitrary
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
    [ OnChain <$> (ChainStatus <$> arbitraryTxHash <*> arbitraryBlockRef <*> arbitraryMaybe arbitraryUTCTime <*> arbitraryNatural <*> arbitrary),
      Pending <$> (PendingStatus <$> arbitraryTxHash <*> arbitraryMaybe arbitraryTxHash <*> arbitrary <*> arbitrary <*> arbitraryMaybe arbitraryUTCTime <*> resize 3 (listOf arbitraryTxHash))
    ]

arbitraryTxInfo :: Network -> Gen TxInfo
arbitraryTxInfo net = TxInfo <$> arbitraryTxSummary net <*> arbitraryTxStatus <*> arbitraryText <*> arbitraryTxidLabels

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
      ResponseImport <$> arbitraryImportReport net ctx,
      ResponseAccounts <$> resize 20 (listOf $ (,) <$> arbitraryDBAccount net ctx <*> arbitraryPositive),
      ResponseAddress <$> arbitraryDBAccount net ctx <*> arbitraryDBAddress net <*> arbitraryPositive,
      ResponseAddresses
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ (,) <$> arbitraryDBAddress net <*> arbitraryPositive),
      ResponseBalances
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ (,) <$> arbitraryDBAddress net <*> arbitraryCoinBalance),
      ResponseCheckAddr
        <$> arbitraryDBAccount net ctx
        <*> arbitraryChainPath
        <*> (fromJust . addrToText net <$> arbitraryAddress net)
        <*> arbitraryText,
      ResponsePendingTxs
        <$> arbitraryMaybe (arbitraryDBAccount net ctx)
        <*> resize 20 (listOf $ (,) <$> arbitraryDBAccount net ctx <*> arbitraryTxInfo net),
      ResponseTxs
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ arbitraryTxInfo net),
      ResponseTx <$> arbitraryDBAccount net ctx <*> arbitraryTxInfo net,
      ResponseLabelledTxs <$> resize 3 (listOf $ (,) <$> arbitraryDBAccount net ctx <*> arbitraryTxInfo net),
      ResponsePrepared
        <$> arbitraryDBAccount net ctx
        <*> arbitraryTxInfo net
        <*> resize 20 (listOf arbitraryLeftCoin),
      ResponseOutsideTx net <$> arbitraryTxInfo net,
      ResponseSentTx <$> arbitraryNetwork <*> arbitraryTxHash,
      ResponseDeleteTx
        <$> arbitraryTxHash
        <*> arbitraryMaybe arbitraryTxHash
        <*> arbitraryNatural
        <*> arbitraryNatural,
      ResponseCoins
        <$> arbitraryDBAccount net ctx
        <*> resize 20 (listOf $ arbitraryJsonCoin net),
      ResponseSync
        <$> (resize 10 $ listOf $ arbitrarySyncRes net ctx)
        <*> (resize 10 $ listOf $ (,) <$> arbitraryDBAccount net ctx <*> arbitraryText),
      ResponseRecover
        <$> arbitraryNetwork
        <*> arbitraryNatural
        <*> (resize 10 $ listOf $ arbitrarySyncRes net ctx),
      ResponseFees net <$> arbitraryFeeRates
    ]

-- | What importacc did with a file
arbitraryImportReport :: Network -> Ctx -> Gen ImportReport
arbitraryImportReport net ctx =
  ImportReport
    <$> resize 5 (listOf (arbitraryAccountImport net ctx))
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> resize 5 (listOf arbitraryNatural)

-- | What became of an account a file names
arbitraryAccountImport :: Network -> Ctx -> Gen AccountImport
arbitraryAccountImport net ctx =
  oneof
    [ AccountCreated <$> arbitraryDBAccount net ctx,
      AccountKept <$> arbitraryDBAccount net ctx <*> arbitraryMaybe arbitraryText,
      AccountRenamed <$> arbitraryText <*> arbitraryDBAccount net ctx,
      AccountRefused <$> arbitraryText <*> arbitraryText
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
    <*> arbitraryMaybe arbitraryNetwork
    <*> arbitrary
    <*> arbitraryMaybe arbitraryText
  where
    -- Every count of the configuration is at least one
    count = (+ 1) <$> arbitraryNatural

-- | The fee rates recent blocks paid, as fees answers them
arbitraryFeeRates :: Gen FeeRates
arbitraryFeeRates = FeeRates <$> arbitraryNatural <*> arbitraryNatural <*> arbitraryNatural <*> arbitrary <*> arbitrary <*> arbitrary

