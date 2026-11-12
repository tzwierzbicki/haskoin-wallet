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
import Haskoin.Wallet.Amounts (AmountUnit (..))
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Coin
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Cli.View (View (..))
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Bip329 (Record (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), ActiveAfter (..), Place (..), PortfolioRef (..))
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), accountDeriv, networkKinds)
import Haskoin.Wallet.Labels (AccountImport (..), FileShown (..), ImportReport (..), PortfolioImport (..), WalletImport (..))
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
    <$> (toSqlKey <$> arbitraryPositive)
    <*> return (cs net.name)
    <*> elements [44, 84]
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> (xPubExport net ctx <$> arbitraryXPubKey ctx)
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryUTCTime

-- | A type of account
arbitraryKind :: Gen AccountKind
arbitraryKind = elements [Bip44, Bip84]

-- | How an answer names a portfolio: the indexes of its wallet and of the
-- portfolio
arbitraryPortfolioRef :: Gen PortfolioRef
arbitraryPortfolioRef = PortfolioRef <$> index <*> index
  where
    -- The index of a portfolio is a hardened step of a path: below 2^31
    index = fromIntegral <$> choose (0, 0x7fffffff :: Integer)

-- | A portfolio on the network
arbitraryPlace :: Network -> Gen Place
arbitraryPlace net = (`Place` net) <$> arbitraryPortfolioRef

-- | An account of a portfolio on the network, of a type the network has
arbitraryAccountRef :: Network -> Gen AccountRef
arbitraryAccountRef net = AccountRef <$> arbitraryPlace net <*> elements (networkKinds net)

-- | The path of an address in a pool: the type of its account, and its
-- path there
arbitraryPoolPath :: Gen PoolPath
arbitraryPoolPath = PoolPath <$> arbitraryKind <*> arbitrarySoftPath

-- | A portfolio as portfolios shows it, with a few networks and accounts
arbitraryPortfolioView :: Ctx -> Gen PortfolioView
arbitraryPortfolioView ctx =
  PortfolioView
    <$> arbitraryPortfolioRef
    <*> arbitrary
    <*> arbitraryText
    <*> arbitraryText
    <*> arbitraryFingerprint
    <*> arbitraryUTCTime
    <*> resize 3 (listOf (arbitraryNetworkView ctx))

arbitraryNetworkView :: Ctx -> Gen NetworkView
arbitraryNetworkView ctx = do
  net <- arbitraryNetwork
  NetworkView net
    <$> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryNatural
    <*> resize 2 (listOf (arbitraryAccountKeys net ctx))

arbitraryAccountKeys :: Network -> Ctx -> Gen AccountKeys
arbitraryAccountKeys net ctx = do
  kind <- arbitraryKind
  index <- choose (0, 1000 :: Integer)
  AccountKeys kind (accountDeriv kind net (fromIntegral index))
    <$> (xPubExport net ctx <$> arbitraryXPubKey ctx)
    <*> arbitraryText
    <*> arbitraryMaybe arbitraryText

-- | A group of a listing on the network, with its accounts and a few rows
arbitraryGroup :: Network -> Gen row -> Gen (Group row)
arbitraryGroup net row =
  Group
    <$> arbitraryPlace net
    <*> resize 2 (listOf (AccountCounts <$> arbitraryKind <*> arbitraryNatural <*> arbitraryNatural))
    <*> resize 10 (listOf row)

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

-- | The balance of the coins at an address: what can be spent now, what
-- pending transactions and the user lock, and what is not spendable yet
-- are parts of the total
arbitraryCoinBalance :: Gen CoinBalance
arbitraryCoinBalance = do
  spendable <- choose (0, 2100000000000000)
  lockedByTx <- choose (0, 2100000000000000)
  lockedByUser <- choose (0, 2100000000000000)
  notYet <- choose (0, 2100000000000000)
  return $ CoinBalance (spendable + lockedByTx + lockedByUser + notYet) spendable lockedByTx lockedByUser

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
    <*> arbitraryMaybe (PoolPath <$> arbitraryKind <*> arbitraryChainPath)
    <*> arbitraryMaybe arbitraryUTCTime

arbitraryCoinLock :: Gen CoinLock
arbitraryCoinLock = elements [LockedByTransaction, LockedByUser]

arbitraryLeftCoin :: Network -> Gen LeftCoin
arbitraryLeftCoin net = LeftCoin <$> arbitraryJsonCoin net <*> elements leftReasons

-- | The labels of a few transactions, by id
arbitraryTxidLabels :: Gen (Map.Map TxHash Text)
arbitraryTxidLabels = Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryTxHash <*> arbitraryText))

arbitraryTxSignData :: Network -> Ctx -> Gen TxSignData
arbitraryTxSignData net ctx =
  TxSignData
    <$> arbitraryTx net ctx
    <*> resize 10 (listOf (arbitraryTx net ctx))
    <*> resize 10 (listOf arbitraryPoolPath)
    <*> resize 10 (listOf arbitraryPoolPath)
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryMaybe arbitraryFingerprint
    <*> resize 3 (listOf arbitraryTxHash)
    <*> arbitraryText

arbitraryMyOutputs :: Gen MyOutputs
arbitraryMyOutputs =
  MyOutputs
    <$> arbitraryNatural
    <*> arbitraryPoolPath
    <*> arbitraryText

arbitraryMyInputs :: Gen MyInputs
arbitraryMyInputs =
  MyInputs
    <$> arbitraryNatural
    <*> arbitraryPoolPath
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
arbitraryTxInfo net = TxInfo <$> arbitraryTxSummary net <*> arbitraryTxStatus <*> arbitraryText <*> arbitraryTxidLabels <*> arbitraryElsewhere net

-- | Addresses of the network that another portfolio holds, each with its
-- portfolio, its path there and its label there
arbitraryElsewhere :: Network -> Gen (Map.Map Address HeldElsewhere)
arbitraryElsewhere net =
  Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryAddress net <*> (HeldElsewhere <$> arbitraryPortfolioRef <*> arbitraryPoolPath <*> arbitraryText)))

arbitrarySyncRes :: Network -> Gen SyncRes
arbitrarySyncRes net =
  SyncRes
    <$> arbitraryPlace net
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryBlockHash
    <*> arbitrary
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural

-- | The secret newportfolio shows: a mnemonic, or the shares of a backup
arbitraryNewSecret :: Gen NewSecret
arbitraryNewSecret =
  oneof
    [ NewMnemonic <$> arbitraryText <*> resize 12 (listOf arbitraryText),
      NewShares <$> arbitraryText <*> arbitraryNatural <*> resize 5 (listOf $ resize 12 $ listOf arbitraryText)
    ]

-- | A transaction with the place of the portfolio that holds it
arbitraryHeldTx :: Network -> Gen (Place, TxInfo)
arbitraryHeldTx net = (,) <$> arbitraryPlace net <*> arbitraryTxInfo net

-- | A network left out, with the failure of its store
arbitraryFailedNetwork :: Gen (Network, Text)
arbitraryFailedNetwork = (,) <$> arbitraryNetwork <*> arbitraryText

arbitraryResponse :: Network -> Ctx -> Gen Response
arbitraryResponse net ctx =
  oneof
    [ ResponseError <$> arbitraryText,
      ResponseNewPortfolio
        <$> arbitraryMaybe arbitraryNewSecret
        <*> arbitraryPortfolioView ctx
        <*> arbitraryActiveAfter,
      ResponsePortfolio <$> arbitraryPortfolioView ctx,
      ResponseSwitched <$> arbitraryPortfolioView ctx,
      ResponseChecked <$> arbitraryPortfolioRef <*> arbitrary <*> arbitraryText,
      ResponseDeleted <$> arbitraryText,
      ResponseDefaults <$> resize 4 (listOf ((,) <$> arbitraryText <*> arbitraryText)) <*> (cs <$> arbitraryText),
      ResponseFile <$> (cs <$> arbitraryText),
      ResponseImport <$> arbitraryImportReport net <*> arbitraryMaybe arbitraryActiveAfter,
      ResponseWalletFile <$> arbitraryFileShown net,
      ResponsePortfolios <$> resize 5 (listOf (arbitraryPortfolioView ctx)),
      -- The JSON of an answer about one thing leaves out the active
      -- portfolio, and reads back as naming none
      ResponseAddress <$> arbitraryAccountRef net <*> arbitraryDBAddress net <*> arbitraryPositive <*> pure Nothing,
      ResponseAddresses
        <$> resize 3 (listOf $ arbitraryGroup net ((,) <$> arbitraryDBAddress net <*> arbitraryPositive)),
      ResponseBalances
        <$> resize 3 (listOf $ (,) <$> arbitraryGroup net ((,) <$> arbitraryDBAddress net <*> arbitraryCoinBalance) <*> arbitraryCoinBalance),
      ResponseDerivedAddr
        <$> arbitraryAccountRef net
        <*> arbitraryChainPath
        <*> (fromJust . addrToText net <$> arbitraryAddress net),
      -- The JSON leaves out how the screen names the wallets, and reads
      -- back as naming none
      ResponsePendingTxs WithoutWallets Nothing <$> resize 10 (listOf (arbitraryHeldTx net)),
      ResponseTxs
        <$> resize 3 (listOf $ arbitraryGroup net (ListedTx <$> arbitraryTxInfo net <*> arbitraryMaybe arbitrary)),
      ResponseTx <$> arbitraryPlace net <*> arbitraryTxInfo net <*> pure Nothing,
      ResponseLabelledTxs <$> resize 3 (listOf (arbitraryHeldTx net)) <*> pure Nothing,
      ResponseHeldTxs <$> resize 3 (listOf (arbitraryHeldTx net)) <*> pure Nothing,
      ResponsePrepared
        <$> arbitraryPlace net
        <*> arbitraryTxInfo net
        <*> resize 20 (listOf (arbitraryLeftCoin net))
        <*> pure Nothing,
      ResponseOutsideTx net <$> arbitraryTxInfo net,
      ResponseSentTx <$> arbitraryNetwork <*> arbitraryTxHash,
      ResponseDeleteTx
        <$> arbitraryTxHash
        <*> arbitraryMaybe arbitraryTxHash
        <*> arbitraryNatural
        <*> arbitraryNatural,
      ResponseCoins
        <$> resize 3 (listOf $ arbitraryGroup net (arbitraryJsonCoin net)),
      ResponseCoin <$> arbitraryAccountRef net <*> arbitraryJsonCoin net <*> pure Nothing,
      ResponseSync
        <$> (resize 10 $ listOf $ arbitrarySyncRes net)
        <*> (resize 3 $ listOf arbitraryNetwork)
        <*> (resize 3 $ listOf arbitraryFailedNetwork)
        <*> arbitraryMaybe arbitraryPortfolioRef,
      ResponseRecover
        <$> (resize 2 $ listOf arbitraryNetwork)
        <*> arbitraryNatural
        <*> arbitraryMaybe arbitraryNatural
        <*> resize 3 (listOf arbitraryNatural)
        <*> (resize 10 $ listOf $ arbitrarySyncRes net)
        <*> (resize 10 $ listOf $ (,) <$> arbitraryNetwork <*> arbitraryForeignFunds)
        <*> (resize 3 $ listOf arbitraryNetwork)
        <*> (resize 3 $ listOf arbitraryFailedNetwork)
        <*> arbitraryMaybe arbitraryActiveAfter,
      ResponseFees net <$> arbitraryFeeRates
    ]

-- | The active portfolio after a command that may create portfolios, and
-- whether the command activated it
arbitraryActiveAfter :: Gen ActiveAfter
arbitraryActiveAfter = ActiveAfter <$> arbitraryPortfolioRef <*> arbitrary

-- | The funds of a seed at an account hw does not hold, of any kind
arbitraryForeignFunds :: Gen ForeignFunds
arbitraryForeignFunds =
  ForeignFunds
    <$> elements [minBound .. maxBound]
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryPositive

-- | What importwallet did with a file
arbitraryImportReport :: Network -> Gen ImportReport
arbitraryImportReport net =
  ImportReport
    <$> resize 3 (listOf arbitraryWalletImport)
    <*> resize 5 (listOf (arbitraryAccountImport net))
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
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> resize 5 (listOf arbitraryNatural)

-- | What became of a wallet a file names, and of its portfolios
arbitraryWalletImport :: Gen WalletImport
arbitraryWalletImport =
  WalletImport
    <$> arbitraryNatural
    <*> arbitraryFingerprint
    <*> arbitrary
    <*> resize 3 (listOf (PortfolioImport <$> arbitraryNatural <*> arbitrary))

-- | What became of an account a file names
arbitraryAccountImport :: Network -> Gen AccountImport
arbitraryAccountImport net =
  oneof
    [ AccountAdded <$> arbitraryAccountRef net,
      AccountHeld <$> arbitraryAccountRef net,
      AccountLeftAside <$> arbitraryText <*> arbitraryText
    ]

-- | A file of a wallet as show shows it
arbitraryFileShown :: Network -> Gen FileShown
arbitraryFileShown net =
  FileShown
    <$> resize 3 (listOf arbitraryWalletImport)
    <*> resize 5 (listOf ((,) <$> arbitraryAccountImport net <*> resize 5 (listOf arbitraryRecord)))
    <*> resize 5 (listOf arbitraryRecord)
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> arbitraryNatural
    <*> resize 5 (listOf arbitraryNatural)

-- | A record of a file of BIP 329, of any type the BIP defines
arbitraryRecord :: Gen Record
arbitraryRecord =
  Record
    <$> elements [minBound .. maxBound]
    <*> arbitraryText
    <*> arbitraryMaybe arbitraryText
    <*> arbitraryMaybe arbitraryText
    <*> arbitraryMaybe arbitrary
    <*> arbitraryMaybe arbitraryText
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryMaybe arbitrary
    <*> arbitraryMaybe arbitraryText
    <*> arbitraryMaybe arbitraryNatural
    <*> arbitraryMaybe arbitraryText

arbitraryKeyBytes :: Gen KeyBytes
arbitraryKeyBytes = KeyBytes <$> arbitraryBS

arbitraryConfig :: Gen Config
arbitraryConfig =
  Config
    <$> arbitraryText
    <*> count
    <*> count
    <*> arbitrary
    <*> arbitraryNetwork
    <*> arbitraryKind
    <*> elements [UnitBitcoin, UnitBit, UnitSatoshi]
    <*> elements [Compact, Verbose, Table]
    <*> arbitraryMaybe arbitraryText
  where
    -- Every count of the configuration is at least one
    count = (+ 1) <$> arbitraryNatural

-- | The fee rates recent blocks paid, as fees answers them
arbitraryFeeRates :: Gen FeeRates
arbitraryFeeRates = FeeRates <$> arbitraryNatural <*> arbitraryNatural <*> arbitraryNatural <*> arbitrary <*> arbitrary <*> arbitrary

