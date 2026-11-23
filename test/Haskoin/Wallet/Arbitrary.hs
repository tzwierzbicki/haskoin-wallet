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
import Haskoin.Wallet.Database.PendingTx (Retag (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Bip329 (Record (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), ActiveAfter (..), Place (..), PortfolioRef (..))
import Haskoin.Wallet.Backup (BackupsShown (..), CopiesShown (..))
import Haskoin.Wallet.DatabaseFile (DatabaseShown (..), PortfolioShown (..), WalletShown (..))
import Haskoin.Wallet.Keys (AccountKind (..), PoolPath (..), accountDeriv, networkKinds)
import Haskoin.Wallet.Labels (AccountImport (..), FileRecord (..), FileShown (..), FileTag (..), ImportReport (..), LabelImport (..), PortfolioImport (..), RecordFate (..), WalletImport (..))
import Haskoin.Wallet.Tx.FeeRates (FeeRates (..))
import Haskoin.Wallet.Sync
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Tags (ShownTag (..), Tag, readTag, tagColors)
import Haskoin.Wallet.Tx.SignData
import Haskoin.Wallet.Tx.Sweep (UnsweptCoin (..), UnsweptReason (..))
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

arbitraryDBAccount :: Network -> Gen DBAccount
arbitraryDBAccount net =
  DBAccount
    <$> (toSqlKey <$> arbitraryPositive)
    <*> return (cs net.name)
    <*> elements [44, 84]
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> (xPubExport net <$> arbitraryXPubKey)
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
arbitraryPortfolioView :: Gen PortfolioView
arbitraryPortfolioView =
  PortfolioView
    <$> arbitraryPortfolioRef
    <*> arbitrary
    <*> arbitraryText
    <*> arbitraryText
    <*> arbitraryFingerprint
    <*> arbitraryUTCTime
    <*> resize 3 (listOf arbitraryNetworkView)

arbitraryNetworkView :: Gen NetworkView
arbitraryNetworkView = do
  net <- arbitraryNetwork
  NetworkView net
    <$> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryPositive
    <*> arbitraryNatural
    <*> resize 2 (listOf (arbitraryAccountKeys net))

arbitraryAccountKeys :: Network -> Gen AccountKeys
arbitraryAccountKeys net = do
  kind <- arbitraryKind
  index <- choose (0, 1000 :: Integer)
  AccountKeys kind (accountDeriv kind net (fromIntegral index))
    <$> (xPubExport net <$> arbitraryXPubKey)
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
  lockedByTag <- choose (0, 2100000000000000)
  lockedByTokens <- choose (0, 2100000000000000)
  notYet <- choose (0, 2100000000000000)
  return $ CoinBalance (spendable + lockedByTx + lockedByUser + lockedByTag + lockedByTokens + notYet) spendable lockedByTx lockedByUser lockedByTag lockedByTokens

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
    <*> arbitraryShownTags
    <*> arbitrary
    <*> arbitraryMaybe (PoolPath <$> arbitraryKind <*> arbitraryChainPath)
    <*> arbitraryMaybe arbitraryUTCTime

-- | A tag: a word of the letters, digits and signs a tag is made of
arbitraryTag :: Gen Tag
arbitraryTag = do
  first <- elements (['a' .. 'z'] <> ['0' .. '9'])
  others <- resize 8 (listOf (elements (['a' .. 'z'] <> ['0' .. '9'] <> "-_")))
  either error return (readTag (cs (first : others)))

-- | A tag as an answer shows it, with a colour or without
arbitraryShownTag :: Gen ShownTag
arbitraryShownTag = ShownTag <$> arbitraryTag <*> arbitraryMaybe (elements tagColors)

-- | The tags of an address or of a transaction in an answer: a few
arbitraryShownTags :: Gen [ShownTag]
arbitraryShownTags = resize 3 (listOf arbitraryShownTag)

-- | The change addresses of the network that followed an address tag or
-- untag was given with --with-change, which may be none
arbitraryChangeFollowed :: Network -> Gen ChangeFollowed
arbitraryChangeFollowed net = do
  retag <- elements [AddTags, RemoveTags]
  rows <- resize 3 (listOf shown)
  -- Only tags that are put on stop somewhere
  stops <- if retag == AddTags then resize 2 (listOf stop) else return []
  return (ChangeFollowed retag rows stops)
  where
    shown = (,,) <$> arbitraryDBAddress net <*> arbitraryPositive <*> arbitraryShownTags
    stop = ChangeStop <$> shown <*> arbitraryShownTags <*> arbitraryTxHash <*> arbitraryMaybe shown

-- | A tag with its uses, as tags lists it
arbitraryTagUse :: Gen TagUse
arbitraryTagUse = TagUse <$> arbitraryShownTag <*> arbitraryNatural <*> arbitraryNatural <*> arbitraryNatural <*> arbitrary

-- | The tags of a transaction: its own, those of some addresses of the
-- network, and those a listing was asked to keep
arbitraryTxTags :: Network -> Gen TxTags
arbitraryTxTags net =
  TxTags
    <$> arbitraryShownTags
    <*> (Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryAddress net <*> arbitraryShownTags)))
    <*> resize 3 (listOf arbitraryTag)

arbitraryCoinLock :: Gen CoinLock
arbitraryCoinLock = elements [LockedByTokens, LockedByTransaction, LockedByReplacement, LockedByUser, LockedByTag]

arbitraryLeftCoin :: Network -> Gen LeftCoin
arbitraryLeftCoin net = LeftCoin <$> arbitraryJsonCoin net <*> elements leftReasons <*> resize 2 (listOf arbitraryTag)

-- | The labels of a few transactions, by id
arbitraryTxidLabels :: Gen (Map.Map TxHash Text)
arbitraryTxidLabels = Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryTxHash <*> arbitraryText))

arbitraryTxSignData :: Network -> Gen TxSignData
arbitraryTxSignData net =
  TxSignData
    <$> arbitraryTx net
    <*> resize 10 (listOf (arbitraryTx net))
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
arbitraryTxType = oneof [pure TxDebit, pure TxInternal, pure TxCredit, pure TxExternal]

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
    [ OnChain <$> (ChainStatus <$> arbitraryTxHash <*> arbitraryBlockRef <*> arbitraryMaybe arbitraryUTCTime <*> arbitraryNatural <*> arbitrary <*> resize 3 (listOf arbitraryTxHash)),
      Pending <$> (PendingStatus <$> arbitraryTxHash <*> arbitraryMaybe arbitraryTxHash <*> arbitrary <*> arbitrary <*> arbitraryMaybe arbitraryUTCTime <*> resize 3 (listOf arbitraryTxHash))
    ]

arbitraryTxInfo :: Network -> Gen TxInfo
arbitraryTxInfo net = TxInfo <$> arbitraryTxSummary net <*> arbitraryTxStatus <*> arbitraryText <*> arbitraryTxidLabels <*> arbitraryElsewhere net <*> arbitraryTxTags net <*> arbitraryContacts net

-- | A contact of the address book on the network, as an answer shows it
arbitraryContact :: Network -> Gen Contact
arbitraryContact net =
  Contact
    <$> (fromJust . addrToText net <$> arbitraryAddress net)
    <*> arbitraryText
    <*> arbitraryShownTags
    <*> arbitraryUTCTime

-- | Addresses of the network that are contacts of the address book
arbitraryContacts :: Network -> Gen (Map.Map Address Contact)
arbitraryContacts net = Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryAddress net <*> arbitraryContact net))

-- | Addresses of the network that another portfolio holds, each with its
-- portfolio, its path there and its label there
arbitraryElsewhere :: Network -> Gen (Map.Map Address HeldElsewhere)
arbitraryElsewhere net =
  Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryAddress net <*> (HeldElsewhere <$> arbitraryPortfolioRef <*> arbitraryPoolPath <*> arbitraryText)))

-- | Some coins a sweep leaves for a reason. A sweep names the coins that
-- hold tokens before the dust coins, and its JSON keeps each kind in a
-- list of its own.
unswept :: Network -> UnsweptReason -> Gen [UnsweptCoin]
unswept net why = resize 3 (listOf (UnsweptCoin <$> arbitraryOutPoint <*> arbitraryAddress net <*> arbitraryNatural <*> pure why))

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
    <*> resize 2 (listOf ((,) <$> arbitraryOutPoint <*> arbitraryOutPoint))
    <*> resize 2 (listOf arbitraryPositive)
    <*> arbitraryNatural
    <*> resize 2 (listOf ((,) <$> arbitraryTxHash <*> arbitrary))
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

-- | What showdefault says of the active portfolio: nothing, the
-- portfolio or the wallet, each maybe none
arbitraryActiveShown :: Gen ActiveShown
arbitraryActiveShown =
  oneof
    [ pure ActiveNotAsked,
      ActivePortfolioShown <$> arbitraryMaybe arbitraryPortfolioRef,
      ActiveWalletShown <$> arbitraryMaybe (fromIntegral <$> chooseInt (0, 5))
    ]

arbitraryResponse :: Network -> Gen Response
arbitraryResponse net =
  oneof
    [ ResponseError <$> arbitraryText,
      ResponseNewPortfolio
        <$> arbitraryMaybe arbitraryNewSecret
        <*> arbitraryMaybe arbitraryText
        <*> arbitraryPortfolioView
        <*> arbitraryActiveAfter
        <*> arbitraryMaybe arbitraryText,
      ResponsePortfolio <$> arbitraryPortfolioView,
      ResponseSwitched <$> arbitraryPortfolioView,
      ResponseChecked <$> arbitraryPortfolioRef <*> arbitrary <*> arbitraryText,
      ResponseDeleted <$> arbitraryText <*> arbitraryMaybe arbitraryBackupFiles,
      ResponseDefaults <$> resize 4 (listOf ((,) <$> arbitraryText <*> arbitraryText)) <*> (cs <$> arbitraryText) <*> oneof [pure Nothing, Just <$> (JournalSwitch <$> arbitrary <*> (cs <$> arbitraryText))],
      ResponseShownDefaults <$> resize 4 (listOf ((,) <$> arbitraryText <*> arbitraryText)) <*> (cs <$> arbitraryText) <*> arbitraryActiveShown,
      ResponseFile <$> (cs <$> arbitraryText),
      ResponseBackup <$> (cs <$> arbitraryText) <*> (cs <$> arbitraryText),
      ResponseRestored <$> (cs <$> arbitraryText) <*> arbitraryDatabaseShown <*> arbitraryMaybe arbitraryBackupFiles <*> arbitraryMaybe (cs <$> arbitraryText),
      ResponseBackups <$> arbitraryBackupsShown,
      ResponseImport <$> arbitraryImportReport net <*> arbitraryMaybe arbitraryActiveAfter,
      ResponseWalletFile <$> (cs <$> arbitraryText) <*> arbitraryFileShown net,
      ResponseDatabaseFile <$> (cs <$> arbitraryText) <*> arbitraryDatabaseShown,
      ResponsePortfolios <$> resize 5 (listOf arbitraryPortfolioView),
      -- The JSON of an answer about one thing leaves out the active
      -- portfolio, and reads back as naming none
      ResponseAddress <$> arbitraryAccountRef net <*> arbitraryDBAddress net <*> arbitraryPositive <*> arbitraryShownTags <*> arbitraryMaybe (arbitraryChangeFollowed net) <*> pure Nothing,
      ResponseAddresses
        <$> resize 3 (listOf $ arbitraryGroup net ((,,) <$> arbitraryDBAddress net <*> arbitraryPositive <*> arbitraryShownTags)),
      ResponseBalances
        <$> resize 3 (listOf $ (,) <$> arbitraryGroup net ((,,) <$> arbitraryDBAddress net <*> arbitraryCoinBalance <*> arbitraryShownTags) <*> arbitraryCoinBalance),
      ResponseTags <$> resize 5 (listOf arbitraryTagUse),
      ResponseTagRenamed <$> arbitraryTag <*> arbitraryTagUse,
      ResponseTagMerged <$> arbitraryTag <*> arbitraryTagUse,
      ResponseTagDeleted <$> arbitraryTag <*> (TagDeleted <$> arbitraryNatural <*> arbitraryNatural <*> arbitraryNatural),
      ResponseContact net <$> arbitraryContact net,
      ResponseContacts <$> resize 3 (listOf ((,) net <$> resize 5 (listOf (arbitraryContact net)))),
      ResponseContactDeleted net <$> arbitraryContact net,
      ResponseDerivedAddr
        <$> arbitraryAccountRef net
        <*> arbitraryChainPath
        <*> (fromJust . addrToText net <$> arbitraryAddress net),
      -- The JSON leaves out how the screen names the wallets, and reads
      -- back as naming none
      ResponsePendingTxs Nothing <$> resize 10 (listOf (arbitraryHeldTx net)),
      ResponseTxs
        <$> resize 3 (listOf $ arbitraryGroup net (ListedTx <$> arbitraryTxInfo net <*> arbitraryMaybe arbitrary)),
      ResponseTx <$> arbitraryPlace net <*> arbitraryTxInfo net <*> pure Nothing,
      ResponseSentPending <$> arbitraryPlace net <*> arbitraryTxInfo net <*> arbitrarySyncAfter net <*> pure Nothing,
      ResponseLabelledTxs <$> resize 3 (listOf (arbitraryHeldTx net)) <*> pure Nothing,
      ResponseHeldTxs <$> resize 3 (listOf (arbitraryHeldTx net)) <*> pure Nothing,
      ResponsePrepared
        <$> arbitraryPlace net
        <*> arbitraryTxInfo net
        <*> resize 20 (listOf (arbitraryLeftCoin net))
        <*> pure Nothing,
      ResponseSweep
        <$> arbitraryMaybe (arbitraryPlace net)
        <*> pure net
        <*> arbitraryTxInfo net
        <*> resize 5 (listOf (arbitraryAddress net))
        <*> ((<>) <$> unswept net HoldsTokens <*> unswept net WorthLessThanItsFee)
        <*> pure Nothing,
      ResponseOutsideTx net <$> arbitraryTxInfo net,
      ResponseSentTx <$> arbitraryNetwork <*> arbitraryTxHash <*> arbitrarySyncAfter net,
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
        <*> arbitraryMaybe arbitraryActiveAfter
        <*> arbitraryMaybe arbitraryText,
      ResponseFees net <$> arbitraryFeeRates
    ]

-- | The sync that follows a sending: the syncs of a few portfolios, and
-- a failure or none
arbitrarySyncAfter :: Network -> Gen SyncAfter
arbitrarySyncAfter net = SyncAfter <$> resize 3 (listOf (arbitrarySyncRes net)) <*> arbitraryMaybe arbitraryText

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
    <*> arbitraryLabelImport
    <*> resize 3 (listOf (PortfolioImport <$> arbitraryNatural <*> arbitrary <*> arbitraryLabelImport))

-- | The label a file gives a wallet or a portfolio, if any, taken or left
-- aside for a description kept
arbitraryLabelImport :: Gen (Maybe LabelImport)
arbitraryLabelImport = oneof [pure Nothing, Just <$> (LabelImport <$> arbitraryText <*> oneof [pure Nothing, Just <$> arbitraryText])]

-- | A record of a file as show shows it, with the labels it would replace
arbitraryFileRecord :: Gen FileRecord
arbitraryFileRecord = FileRecord <$> arbitraryRecord <*> resize 2 (listOf arbitraryText) <*> elements [FateSet, FateNotInWallet, FateOtherAccount, FateOwnAddress]

-- | What became of an account a file names
arbitraryAccountImport :: Network -> Gen AccountImport
arbitraryAccountImport net =
  oneof
    [ AccountAdded <$> arbitraryAccountRef net,
      AccountHeld <$> arbitraryAccountRef net,
      AccountLeftAside <$> arbitraryText <*> arbitraryText
    ]

-- | A file of a wallet as show shows it. The accounts of its report are
-- those its records are under, as its JSON gives them once.
arbitraryFileShown :: Network -> Gen FileShown
arbitraryFileShown net = do
  report <- arbitraryImportReport net
  records <- vectorOf (length (reportAccounts report)) (resize 5 (listOf arbitraryFileRecord))
  FileShown report (zip (reportAccounts report) records)
    <$> resize 5 (listOf arbitraryFileRecord)
    <*> resize 3 (listOf (FileTag <$> arbitraryShownTag <*> arbitrary))

-- | A database file as show shows it
-- | The two files of a backup hw made before a command lost something
arbitraryBackupFiles :: Gen BackupFiles
arbitraryBackupFiles = BackupFiles <$> (cs <$> arbitraryText) <*> (cs <$> arbitraryText)

-- | The copies hw keeps, as showbackups shows them: each kind on or off,
-- with or without a newest copy
arbitraryBackupsShown :: Gen BackupsShown
arbitraryBackupsShown =
  BackupsShown
    <$> (cs <$> arbitraryText)
    <*> arbitrary
    <*> copies
    <*> arbitrary
    <*> copies
    <*> (cs <$> arbitraryText)
    <*> copies
  where
    copies = CopiesShown <$> chooseInt (0, 60) <*> arbitraryMaybe ((,) <$> (cs <$> arbitraryText) <*> arbitraryUTCTime)

arbitraryDatabaseShown :: Gen DatabaseShown
arbitraryDatabaseShown =
  DatabaseShown <$> chooseInt (1, 9) <*> resize 3 (listOf wallet)
  where
    wallet = WalletShown <$> arbitraryNatural <*> arbitraryFingerprint <*> arbitraryText <*> resize 3 (listOf portfolio)
    portfolio = PortfolioShown <$> arbitraryNatural <*> arbitrary <*> arbitraryText <*> arbitraryUTCTime

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
    <*> arbitraryMaybe (resize 3 (listOf arbitraryText))
    <*> arbitraryMaybe (Map.fromList <$> resize 3 (listOf ((,) <$> arbitraryText <*> arbitraryText)))
    <*> arbitraryMaybe (resize 3 (listOf arbitraryText))
    <*> arbitraryMaybe arbitrary
    <*> arbitraryMaybe arbitraryUTCTime

arbitraryKeyBytes :: Gen KeyBytes
arbitraryKeyBytes = KeyBytes <$> arbitraryBS

-- | The fee rates recent blocks paid, as fees answers them
arbitraryFeeRates :: Gen FeeRates
arbitraryFeeRates = FeeRates <$> arbitraryNatural <*> arbitraryNatural <*> arbitraryNatural <*> arbitrary <*> arbitrary <*> arbitrary

