{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Wallets and their labels as a BIP 329 file ("Haskoin.Wallet.Bip329"),
-- which exportwallet writes and importwallet reads.
--
-- An account goes in the file as an @xpub@ record: its key, its origin
-- @pkh([fingerprint/44'/coin'/index'])@, or
-- @wpkh([fingerprint/84'/coin'/index'])@ for a BIP84 account, which names
-- the fingerprint of the seed, and under hw's own keys its network,
-- its purpose, 44 or 84, and the description of its wallet. The label of
-- the record is the description of the portfolio. Then come the labelled
-- receiving addresses of the account, each with its key path, the
-- labelled transactions of its portfolio on its network, with their
-- height, fee and value when the chain has them, and the coins the user
-- locked, as outputs that are not spendable. Every record carries the
-- origin of its account and the name of its network: the test networks
-- share their keys, so the origin alone does not tell one from another.
--
-- A file holds every account of the portfolios it was asked for.
-- exportwallet asks for every portfolio of every wallet, unless the user
-- names one wallet or one portfolio. So the whole database goes from the
-- computer that has the mnemonics to the one that watches the chain in
-- one file, and one file keeps every label.
--
-- The wallets of a file come one after the other, in the order of their
-- indexes. Only the origins of their records tell them apart. BIP 329
-- gives the origin for that. It says the origin "should be used to
-- disambiguate transaction labels from different wallets contained in
-- the same export".
--
-- The index of a wallet is not written. It is a name the database
-- gives, the lowest free one when the wallet is created. Another
-- database may give the same wallet another index. For example, recover
-- run with two mnemonics in the other order makes wallet1 of what was
-- wallet0. The fingerprint of the mnemonic is the same everywhere, so a
-- file names a wallet by it.
--
-- Reading a file, importwallet takes the @xpub@ records first. They are
-- sorted by the fingerprint of their origin, which names their wallet, and
-- by the index of their path, which names their portfolio. A wallet is
-- the one with that fingerprint, whatever its index in the database, and
-- each portfolio is taken whole or not at all. A wallet the database
-- does not have takes the lowest free index. Such wallets are created in
-- the order of their first keys in the file. So an empty database gets
-- the wallets in the order the file has them.
--
-- A portfolio the database has must hold every key the file gives it,
-- and nothing is added to it: a key it does not hold means the file and
-- the database are not of one mnemonic, and the import is refused. A
-- portfolio the database does not have is created with its twelve
-- accounts, so the file must give all of them. A portfolio that lacks
-- accounts would need the mnemonic to be completed, since a public key of
-- an account derives none of the others, and the online computer never
-- sees the mnemonic. A wallet the database does not have is created with
-- its portfolio0, which every wallet has, so the file must bring that one
-- whole. The import is refused whole when any of this fails, and so is a
-- key that another account of the network holds, whatever child number
-- the file writes it with, and two keys for accounts of one path, as the
-- test networks share theirs
-- ("Haskoin.Wallet.Database.Account".'createPortfolioWith').
--
-- importwallet reads a part of a file when the user names it
-- ('scopeWallet', 'scopePortfolio'). --wallet names one wallet by its
-- fingerprint, the name the file itself gives it, and --portfolio one
-- portfolio by its index.
-- The keys and the records of the rest of the file are then left out.
-- The records left out are counted, so that the answer accounts for
-- them. An index alone would name a portfolio of each wallet of a file
-- that holds several. Such a request is refused, and the refusal asks
-- for the wallet ('checkScopeHeld').
--
-- A wallet and a portfolio take the labels of their key records as their
-- descriptions when they have none, and keep their own otherwise
-- ('takeLabel'). The report counts each wallet and each portfolio once,
-- though each of its key records carries its label. A label taken counts
-- among the labels set, beside those of addresses and transactions. A
-- label that meets a description of the user's own counts apart, among
-- what is left aside. So the answer accounts for every label of the file.
--
-- Nothing in a file proves that its keys are those of the mnemonic of the
-- wallet it names: only the mnemonic could. importwallet trusts the file,
-- as any import of public keys must, and the help says to import only a
-- file the user wrote on their own signer, or got over a channel they
-- trust.
--
-- Every key record hw writes names its origin. A key record without one
-- is of the wallet of the account that holds its key in the database, if
-- any, and is left aside otherwise.
--
-- An @xpub@ record that is not one of hw's, with no network or no type of
-- account, is the key of one account. It does not say which portfolio of
-- which wallet it belongs to, so it is left aside, with the reason: hw
-- imports whole wallets, not single keys.
--
-- Then each other record goes where it belongs. An address is found among
-- the addresses of the wallet, however it is written, or derived from its
-- key path in the account whose key gives it, which then generates its
-- chain up to it, no further than a thousand addresses past the last used
-- one ('scanLimit'). A transaction goes to the portfolios its origin
-- names, whether they have the transaction yet or not, since the labels
-- of transactions are kept apart from the transactions; without an
-- origin, to the portfolios that have it, on the network the record names
-- ('labelTxRecord'). A coin that is not spendable is
-- locked, as lockcoin locks it. What hw cannot place, the wallet may not
-- have yet: the answer says so, and a sync followed by another import
-- places it. What hw keeps no label for, such as inputs, coins and change
-- addresses, is left aside and counted.
--
-- show shows such a file before the user imports it ('fileShown'). It
-- reads the file as importwallet does ('readWalletFile'), and shows what
-- importwallet would make of each account, with the labels and locks of
-- its records under it. The database keeps nothing of it. show sorts the
-- records by their origins only, where importwallet looks for each in
-- the wallet, since show tells what the file says.
module Haskoin.Wallet.Labels
  ( -- * The file exportwallet writes
    walletRecords,

    -- * The file importwallet reads
    WalletFile (..),
    readWalletFile,
    ImportScope (..),
    wholeFile,
    importFile,
    importRecords,
    ImportReport (..),
    WalletImport (..),
    PortfolioImport (..),
    AccountImport (..),
    reportPairs,
    parseReport,
    reportNeedsSync,
    importCreated,

    -- * The file show shows
    FileShown (..),
    fileShown,
    holdsAnything,
    fileShownPairs,
    parseFileShown,
  )
where

import Conduit (MonadUnliftIO)
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadTrans (lift))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import Data.Bits (clearBit, testBit)
import Data.List (elemIndex, nub, nubBy, partition, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (bestHeight, getConfirmations)
import Haskoin.Wallet.Database.Coin (coinOutpoint, coinRows, readBlock, setLockedByUser, userLockedCoins)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), findPendingTx, intercalateAnd, pendingTxs, setPendingTxLabel, txInfoOf)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..), allPools, liveSummary, txInfoKey)
import Haskoin.Wallet.Database.TxLabel (poolTxLabels, setTxLabel)
import Haskoin.Wallet.Descriptor
import Haskoin.Wallet.Keys (AccountKind (..), accountDeriv, checkKindOnNetwork, kindName, kindOfPurpose, kindPurpose, portfolioAccounts, sameKey, xPubIndex)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), isSigned)
import Haskoin.Wallet.Util (visibleText)
import Numeric.Natural (Natural)

{- The file exportwallet writes -}

-- | The records of portfolios, in their order: those of each account of
-- each ('accountRecords'). The portfolios may be of several wallets. The
-- origin of each record names its wallet, by the fingerprint of the
-- portfolio the record is of.
walletRecords :: (MonadUnliftIO m) => Ctx -> [Portfolio] -> ExceptT String (DB m) [Record]
walletRecords ctx portfolios =
  fmap concat . forM portfolios $ \portfolio -> do
    accounts <- lift $ portfolioAccountRows portfolio.portfolioId
    -- The transactions are of the portfolio on a network: they are
    -- written once, with the first account of the network
    let firstOfPool = [accId | net <- nub (accountNetwork . snd <$> accounts), (accId, _) <- take 1 [a | a@(_, acc) <- accounts, accountNetwork acc == net]]
    concat <$> forM accounts (\held@(accId, _) -> accountRecords ctx portfolio (accId `elem` firstOfPool) held)

-- | The records of an account of a portfolio: its key, then its labelled
-- receiving addresses, the labelled transactions of its portfolio on its
-- network when they are asked for, on the chain or signed and pending,
-- and the coins the user locked
accountRecords :: (MonadUnliftIO m) => Ctx -> Portfolio -> Bool -> (DBAccountId, DBAccount) -> ExceptT String (DB m) [Record]
accountRecords ctx portfolio withTransactions (accId, acc) = do
  heightM <- bestHeight net
  addrs <- lift $ labelledAddresses accId
  labels <- if withTransactions then lift (poolTxLabels pool) else return []
  chainRecords <- forM labels $ \(tid, label) -> do
    liveM <- liveSummary pool tid
    return
      (ofAccount (record TxRecord (txHashToHex tid)))
        { recordLabel = Just label,
          recordHeight = settledHeight heightM . fst =<< liveM,
          recordFee = summaryFee . snd <$> liveM,
          recordValue = summaryAmount . snd <$> liveM
        }
  pending <- if withTransactions then pendingTxs pool else return []
  pendingRecords <- fmap concat . forM pending $ \p -> do
    let tx = txSignDataTx p.pendingTxData
        label = p.pendingTxData.txSignDataLabel
    if isSigned tx && not (Text.null label)
      then do
        info <- txInfoOf ctx pool p.pendingTxData
        return
          [ (ofAccount (record TxRecord (txHashToHex (txHash tx))))
              { recordLabel = Just label,
                recordFee = Just (summaryFee (txInfoSummary info)),
                recordValue = Just (summaryAmount (txInfoSummary info))
              }
          ]
      else return []
  coins <- lift $ userLockedCoins accId
  coinRecords <- forM coins $ \coin -> do
    outpoint <- liftEither (coinOutpoint coin)
    block <- liftEither (readBlock outpoint (dBCoinBlockRef coin))
    rowM <- lift $ P.getBy (UniqueAddress accId (dBCoinAddress coin))
    return
      (ofAccount (record OutputRecord (outpointText outpoint)))
        { recordSpendable = Just False,
          recordKeypath = keypathText . addrPath . P.entityVal <$> rowM,
          recordHeight = settledHeight heightM block,
          recordValue = Just (fromIntegral (dBCoinValue coin))
        }
  return $ keyRecord : addressRecords addrs <> chainRecords <> pendingRecords <> coinRecords
  where
    net = accountNetwork acc
    pool = accountPool acc
    kind = accountKind acc
    origin = originText (accountOrigin kind (portfolioFingerprint portfolio) (accountPath portfolio acc))
    -- Every record names the account it is of: its origin, and its
    -- network, which the origin does not tell
    ofAccount r = r {recordOrigin = Just origin, recordNetwork = Just (dBAccountNetwork acc)}
    keyRecord =
      (ofAccount (record XpubRecord (dBAccountXPubKey acc)))
        { recordLabel = given (dBPortfolioDescription portfolio.portfolioRow),
          recordPurpose = Just (kindPurpose kind),
          recordWallet = given (dBWalletDescription portfolio.portfolioWallet)
        }
    given text = if Text.null text then Nothing else Just text
    addressRecords addrs =
      [ (ofAccount (record AddrRecord (dBAddressAddress addr)))
          { recordLabel = Just (dBAddressLabel addr),
            recordKeypath = Just (keypathText (addrPath addr))
          }
        | addr <- addrs
      ]

-- | The height of a block a transaction or a coin is in, once six blocks
-- confirm it, as the BIP asks: before that, the height may still change
settledHeight :: Maybe BlockHeight -> Store.BlockRef -> Maybe Natural
settledHeight heightM block = case block of
  Store.BlockRef height _ | getConfirmations heightM block >= 6 -> Just (fromIntegral height)
  _ -> Nothing

{- The file importwallet reads -}

-- | What importwallet did with a file
data ImportReport = ImportReport
  { -- | What became of each wallet the file names, in the order of their
    -- first keys
    reportWallets :: ![WalletImport],
    -- | What became of each account the file names, in the order of its
    -- xpub records
    reportAccounts :: ![AccountImport],
    -- | The labels set on wallets, as their descriptions. A wallet counts
    -- when it bears the label of the file after the import: it had no
    -- description and took it, or had that one already. It counts once,
    -- however many key records carry its label.
    reportWalletLabels :: !Natural,
    -- | The labels set on portfolios, as their descriptions, counted as
    -- those of wallets
    reportPortfolioLabels :: !Natural,
    -- | The labels set on addresses
    reportAddressLabels :: !Natural,
    -- | The labels set on transactions, each portfolio counting once
    reportTxLabels :: !Natural,
    reportCoinsLocked :: !Natural,
    reportCoinsUnlocked :: !Natural,
    -- | The addresses the wallet does not have, and could not derive
    reportAddressesMissing :: !Natural,
    -- | Of those, the addresses hw did not derive from their key path,
    -- which lies beyond 'scanLimit' past the last used address of the
    -- receiving chain of their account
    reportAddressesTooFar :: !Natural,
    -- | The transactions no portfolio has, when no origin says which
    -- portfolio they are of
    reportTxsMissing :: !Natural,
    -- | The coins the wallet does not have
    reportCoinsMissing :: !Natural,
    -- | The records whose origin names an account the wallet does not have
    reportOtherAccounts :: !Natural,
    -- | The records of the portfolios --wallet or --portfolio leaves
    -- out, which are not read: those of the other wallets, and those of
    -- the other portfolios
    reportOtherPortfolios :: !Natural,
    -- | The labels the file gives wallets that keep a description of
    -- their own, which differs from it
    reportWalletLabelsKept :: !Natural,
    -- | The labels the file gives portfolios that keep a description of
    -- their own, which differs from it
    reportPortfolioLabelsKept :: !Natural,
    -- | The labels of change addresses, which hw does not keep
    reportChangeAddresses :: !Natural,
    -- | The records hw keeps no label for: inputs, the labels of coins,
    -- public keys it cannot read, and the kinds the BIP does not define
    reportNotKept :: !Natural,
    -- | The lines that are not records, as a text editor numbers them
    reportUnreadable :: ![Natural]
  }
  deriving (Eq, Show)

-- | What became of the label a file gives a wallet or a portfolio, which
-- hw keeps as its description. Each key record of a portfolio carries the
-- label of the portfolio and that of its wallet. So a wallet or a
-- portfolio has one outcome, however many of its key records the file
-- holds.
data LabelOutcome
  = -- | The file gives it no label
    NoLabelGiven
  | -- | It bears the label of the file: it had no description and took
    -- it, or had that one already
    LabelTaken
  | -- | It keeps a description of its own, which differs from the label
    -- of the file
    OwnLabelKept
  deriving (Eq)

-- | What became of the labels a file gives its wallets and its
-- portfolios: an outcome for each wallet, and one for each portfolio
data LabelsTaken = LabelsTaken
  { walletLabelOutcomes :: ![LabelOutcome],
    portfolioLabelOutcomes :: ![LabelOutcome]
  }

instance Semigroup LabelsTaken where
  LabelsTaken wallets portfolios <> LabelsTaken moreWallets morePortfolios =
    LabelsTaken (wallets <> moreWallets) (portfolios <> morePortfolios)

instance Monoid LabelsTaken where
  mempty = LabelsTaken [] []

-- | Count in a report what became of the labels of wallets and
-- portfolios
withLabelsTaken :: LabelsTaken -> ImportReport -> ImportReport
withLabelsTaken labels report =
  report
    { reportWalletLabels = countOf LabelTaken (walletLabelOutcomes labels),
      reportPortfolioLabels = countOf LabelTaken (portfolioLabelOutcomes labels),
      reportWalletLabelsKept = countOf OwnLabelKept (walletLabelOutcomes labels),
      reportPortfolioLabelsKept = countOf OwnLabelKept (portfolioLabelOutcomes labels)
    }
  where
    countOf outcome = fromIntegral . length . filter (== outcome)

-- | What became of a wallet the file names
data WalletImport = WalletImport
  { -- | The index of the wallet in the database
    walletImportIndex :: !Natural,
    -- | The fingerprint of its seed
    walletImportFingerprint :: !Fingerprint,
    -- | Whether the import created it
    walletImportCreated :: !Bool,
    -- | What became of each of its portfolios the file names, in the
    -- order of their indexes
    walletImportPortfolios :: ![PortfolioImport]
  }
  deriving (Eq, Show)

-- | What became of a portfolio the file names: created whole, with its
-- twelve accounts, or found in the database, which had it whole already
data PortfolioImport = PortfolioImport
  { portfolioImportIndex :: !Natural,
    -- | Whether the import created it
    portfolioImportCreated :: !Bool
  }
  deriving (Eq, Show)

-- | What became of an account the file names
data AccountImport
  = -- | Added with its portfolio, which the import created
    AccountAdded !AccountRef
  | -- | Its portfolio had it already, with the key of the file
    AccountHeld !AccountRef
  | -- | Not imported: the start of its key, and why. Its record is not one
    -- of an account of a wallet of hw.
    AccountLeftAside !Text !Text
  deriving (Eq, Show)

-- | The portfolios an import created, in the order of the report: the
-- wallets in the order of their first keys, and the portfolios of each in
-- the order of their indexes. The first one became the active portfolio
-- when the database had none.
importCreated :: ImportReport -> [PortfolioRef]
importCreated report =
  [ PortfolioRef (walletImportIndex wallet) (portfolioImportIndex portfolio)
    | wallet <- reportWallets report,
      portfolio <- walletImportPortfolios wallet,
      portfolioImportCreated portfolio
  ]

-- | The account an xpub record left in the wallet, added or found, if any
importedAccount :: AccountImport -> Maybe AccountRef
importedAccount = \case
  AccountAdded ref -> Just ref
  AccountHeld ref -> Just ref
  AccountLeftAside _ _ -> Nothing

-- | Whether the file labels things the wallet does not have yet, which a
-- sync may bring
reportNeedsSync :: ImportReport -> Bool
reportNeedsSync report = reportAddressesMissing report + reportTxsMissing report + reportCoinsMissing report > 0

-- | A report with nothing done yet
emptyReport :: [WalletImport] -> [AccountImport] -> [Natural] -> ImportReport
emptyReport wallets accounts unreadable =
  ImportReport
    { reportWallets = wallets,
      reportAccounts = accounts,
      reportWalletLabels = 0,
      reportPortfolioLabels = 0,
      reportAddressLabels = 0,
      reportTxLabels = 0,
      reportCoinsLocked = 0,
      reportCoinsUnlocked = 0,
      reportAddressesMissing = 0,
      reportAddressesTooFar = 0,
      reportTxsMissing = 0,
      reportCoinsMissing = 0,
      reportOtherAccounts = 0,
      reportOtherPortfolios = 0,
      reportWalletLabelsKept = 0,
      reportPortfolioLabelsKept = 0,
      reportChangeAddresses = 0,
      reportNotKept = 0,
      reportUnreadable = unreadable
    }

-- | What of a file importwallet takes. With no wallet and no portfolio
-- named, it takes everything.
--
-- A file names each of its wallets by the fingerprint of the seed, in the
-- origins of its records. So --wallet names a wallet of a file by its
-- fingerprint too. The index of a wallet is no name here. The file does
-- not hold it, and the database that wrote the file may number its
-- wallets in another order than this one. The active portfolio plays no
-- part either, since the file may be of other wallets.
--
-- --portfolio names a portfolio by its index. That index is the last
-- step of the path of its accounts, so it is the same in every database.
-- An index alone names one portfolio only in a file of one wallet. In a
-- file of several wallets it would name a portfolio of each of them, and
-- nobody wants those together. Such a scope is refused, and the user
-- names the wallet too ('checkScopeHeld').
data ImportScope = ImportScope
  { -- | The one wallet to import, by the fingerprint of its seed, or all
    -- of them
    scopeWallet :: !(Maybe Fingerprint),
    -- | The one portfolio to import, by its index, or all of them
    scopePortfolio :: !(Maybe Natural)
  }
  deriving (Eq, Show)

-- | Everything a file holds
wholeFile :: ImportScope
wholeFile = ImportScope Nothing Nothing

-- | Import the content of a file of a wallet, as 'readWalletFile' reads
-- it, within the scope given. A file that cannot be imported is refused
-- whole: the portfolios the import created before it found that out are
-- undone ('undoneIfRefused'), so that a refused file changes nothing.
importFile :: (MonadUnliftIO m) => Ctx -> ImportScope -> Text -> ExceptT String (DB m) ImportReport
importFile ctx scope content = undoneIfRefused $ do
  let file = readWalletFile content
  report <- importRecords ctx scope (fileKeys file) (fileRecords file) (fileUnreadable file)
  return report {reportNotKept = reportNotKept report + fileOtherTypes file}

-- | What a file of a wallet holds, as importwallet reads it, before
-- anything is done with it. importwallet imports it, and show shows it
-- ('fileShown').
data WalletFile = WalletFile
  { -- | The key of each account, in the order of the file, each key once
    -- on its network
    fileKeys :: ![Record],
    -- | The records of anything but a key, each with the number of its line
    fileRecords :: ![(Natural, Record)],
    -- | The records of a type the BIP does not define
    fileOtherTypes :: !Natural,
    -- | The lines that are not records, by number
    fileUnreadable :: ![Natural]
  }
  deriving (Eq, Show)

-- | Read the content of a file of a wallet: a BIP 329 file, a record on
-- each line. Any content reads, if only as lines that are not records.
readWalletFile :: Text -> WalletFile
readWalletFile content =
  WalletFile
    { fileKeys = nub [r | KeyLine r <- classified],
      fileRecords = [(number, r) | RecordLine number r <- classified],
      fileOtherTypes = fromIntegral (length [() | OtherTypeLine <- classified]),
      fileUnreadable = [number | UnreadableLine number <- classified]
    }
  where
    classified = classifyLine <$> readLines content

-- | What a line of a file holds, for the import
data FileLine
  = -- | An xpub record
    KeyLine !Record
  | -- | A record of another type, with the number of its line
    RecordLine !Natural !Record
  | -- | A record of a type the BIP does not define
    OtherTypeLine
  | -- | A line that is not a record, by its number
    UnreadableLine !Natural

-- | A line as a record. A line that is none is not readable.
classifyLine :: (Natural, Text) -> FileLine
classifyLine (number, line) = case parseLine line of
  Right (LineRecord r) | recordType r == XpubRecord -> KeyLine r
  Right (LineRecord r) -> RecordLine number r
  Right (LineOfOtherType _) -> OtherTypeLine
  Left _ -> UnreadableLine number

-- | Import the wallets of the key records, then the other records, each
-- with the number of its line, and count the lines given as not readable.
-- A scope the records cannot answer is refused ('checkScopeHeld').
importRecords :: (MonadUnliftIO m) => Ctx -> ImportScope -> [Record] -> [(Natural, Record)] -> [Natural] -> ExceptT String (DB m) ImportReport
importRecords ctx scope keys others unreadable = do
  liftEither $ checkScopeHeld ctx scope keys (snd <$> others)
  (wallets, labels, accountImports) <- importKeys ctx scope keys
  accounts <- lift getAccounts
  -- The records of the wallets and portfolios left out are counted, and
  -- not read. Their labels would be looked for in what is read.
  let (inScope, leftOut) = partition (recordInScope scope . snd) others
      keysImported = withLabelsTaken labels (emptyReport wallets accountImports unreadable)
  report <- foldM (importRecord ctx accounts) keysImported inScope
  return report {reportOtherPortfolios = fromIntegral (length leftOut)}

-- | Whether a record is of a wallet and a portfolio the scope reads. The
-- origin of the record names both. With --wallet, a record of another
-- fingerprint is left out. With --portfolio, a record of an account of
-- another portfolio is left out. A record without an origin hw can read
-- names neither, and is read. The address or the transaction it labels
-- says where it goes.
recordInScope :: ImportScope -> Record -> Bool
recordInScope scope r = ofWallet && ofPortfolio
  where
    originM = parseOrigin =<< recordOrigin r
    ofWallet = case (scope.scopeWallet, originFingerprint <$> originM) of
      (Just wanted, Just fingerprint) -> fingerprint == wanted
      _ -> True
    ofPortfolio = case (scope.scopePortfolio, originAccountIndex =<< originM) of
      (Just wanted, Just index) -> index == wanted
      _ -> True

-- | Refuse a scope the file cannot answer. There are three of them.
--
-- The first is a wallet the file holds nothing of. The refusal names the
-- wallets the file has, by their fingerprints.
--
-- The second is a portfolio alone, when the file holds several wallets.
-- The index then names a portfolio of each wallet. Reading them all would
-- import portfolios that have nothing in common but their index. So the
-- user is asked for the wallet, and the refusal gives the fingerprints to
-- choose from. A file of one wallet needs no --wallet.
--
-- The third is a portfolio the file holds nothing of, in the wallet
-- named when one is. importwallet --portfolio 2 of such a file would do
-- nothing, and exit as if it had. The index is more likely a typo, or
-- the file the wrong one. The refusal names the portfolios whose keys
-- the file holds, so that the user sees which index was meant.
--
-- A wallet or a portfolio is in the file when a key record names it, or
-- the origin of another record does. A file of labels alone, from
-- another wallet, holds no key, and still names them by its origins.
checkScopeHeld :: Ctx -> ImportScope -> [Record] -> [Record] -> Either String ()
checkScopeHeld ctx scope keys others = do
  forM_ scope.scopeWallet $ \wanted ->
    unless (wanted `elem` wallets) $
      Left $
        "The file has no wallet "
          <> shown wanted
          <> ": "
          <> case wallets of
            [] -> "it names no wallet"
            [only] -> "it has the wallet " <> shown only
            held -> "it has the wallets " <> intercalateAnd (shown <$> held)
  case (scope.scopeWallet, scope.scopePortfolio, wallets) of
    (Nothing, Just wanted, first : _ : _) ->
      Left $
        "The file holds the wallets "
          <> intercalateAnd (shown <$> wallets)
          <> ", and --portfolio alone does not say which of them "
          <> cs (portfolioName wanted)
          <> " is of. Name the wallet by its fingerprint too, as in --wallet "
          <> shown first
          <> " --portfolio "
          <> show wanted
    _ -> Right ()
  forM_ scope.scopePortfolio $ \wanted ->
    unless (wanted `elem` keyIndexes || wanted `elem` recordIndexes) $
      Left $
        "The file has no "
          <> cs (portfolioName wanted)
          <> ofWalletNamed
          <> ": "
          <> case keyIndexes of
            [] -> "it holds the key of no portfolio" <> ofThatWallet
            held -> "it has " <> intercalateAnd (cs . portfolioName <$> held) <> ofThatWallet
  where
    shown :: Fingerprint -> String
    shown = cs . fingerprintToText
    -- The wallet and the portfolio each key record hw can read names. A
    -- key record without an origin names no wallet here, since only the
    -- database tells whose key it is ('importKeys').
    keyPlaces = [(key.walletKeyFingerprint, key.walletKeyIndex) | Right key <- readWalletKey ctx <$> keys]
    -- The origins of the other records. One that is the path of an
    -- account names a portfolio, and any names its wallet.
    recordOrigins = mapMaybe (parseOrigin <=< recordOrigin) others
    -- The wallets the file names, each once, in the order of the file
    wallets = nub ([fingerprint | (Just fingerprint, _) <- keyPlaces] <> (originFingerprint <$> recordOrigins))
    -- Whether a wallet is the one --wallet names, any wallet without the
    -- option. A key that names no wallet may be of the one named.
    ofWanted fingerprintM = case (scope.scopeWallet, fingerprintM) of
      (Just wanted, Just fingerprint) -> fingerprint == wanted
      _ -> True
    -- The portfolios of those wallets that the key records name, each
    -- once, in the order of their indexes
    keyIndexes = sort (nub [index | (fingerprintM, index) <- keyPlaces, ofWanted fingerprintM])
    recordIndexes = [index | origin <- recordOrigins, ofWanted (Just (originFingerprint origin)), Just index <- [originAccountIndex origin]]
    ofWalletNamed = maybe "" (\wanted -> " of the wallet " <> shown wanted) scope.scopeWallet
    ofThatWallet = maybe "" (const " of that wallet") scope.scopeWallet

-- | The index of the account an origin names, when its path is that of
-- an account: three hardened steps, the last of them the index
originAccountIndex :: Origin -> Maybe Natural
originAccountIndex origin = case pathToList (originPath origin) of
  [_, _, index] | testBit index 31 -> Just (fromIntegral (clearBit index 31))
  _ -> Nothing

{- Wallets, portfolios and accounts -}

-- | The key of an account of a wallet of hw, as an xpub record gives it:
-- what places the account in a portfolio of a wallet
data WalletKey = WalletKey
  { walletKeyRecord :: !Record,
    walletKeyNetwork :: !Network,
    walletKeyKind :: !AccountKind,
    -- | The index of the portfolio: the last step of the path of the key
    walletKeyIndex :: !Natural,
    walletKeyXPub :: !XPubKey,
    -- | The fingerprint of the seed, which names the wallet, when the
    -- record has an origin
    walletKeyFingerprint :: !(Maybe Fingerprint)
  }

-- | How a key record that is left aside is named: by the start of its key
shownKey :: Record -> Text
shownKey r = Text.take 12 (recordRef r) <> "..."

-- | Read the key of an account of a wallet from an xpub record, or say
-- why the record is not one. The key is read first, as BIP32 writes it: a
-- zpub or a vpub names the same key as its xpub, and says its account is
-- a BIP84 one ('readAccountKey'). The record must say its network, under
-- hw's own key, and the type of its account, by the script function of
-- its origin, by the purpose hw wrote or by the prefix of its key, which
-- must agree. An origin must be the path of that account on that network.
readWalletKey :: Ctx -> Record -> Either Text WalletKey
readWalletKey ctx r = do
  (prefixKindM, keyText) <- failing (readAccountKey (recordRef r))
  when (isPrivateKey keyText) $
    Left "The key is a private key. hw takes the public key of an account, and no file of a wallet should hold a private one"
  netName <- maybe (Left noNetworkKey) Right (recordNetwork r)
  net <- maybe (Left ("Unknown network " <> visibleText netName)) Right (netByName (cs netName))
  xpub <- maybe (Left ("The key does not read as a key of " <> cs net.name)) Right (xPubImport net ctx keyText)
  unless (xpub.depth == 3 && testBit xpub.index 31) $
    Left "The key is not that of an account, three hardened steps below the master key"
  purposeKindM <- forM (recordPurpose r) $ \purpose ->
    maybe (Left ("The purpose " <> cs (show purpose) <> " is of no type of account hw holds: " <> Text.intercalate " or " (kindName <$> [minBound .. maxBound]))) Right (kindOfPurpose purpose)
  kind <- case nub (catMaybes [kindOfScripts . originScripts =<< originM, purposeKindM, prefixKindM]) of
    [] -> Left noTypeKey
    [kind] -> Right kind
    kinds -> Left ("The record says its key is of accounts of two types, " <> Text.intercalate " and " (kindName <$> kinds) <> ": its origin, its purpose and the prefix of its key do not agree")
  failing (checkKindOnNetwork net kind)
  let index = xPubIndex xpub
      path = accountDeriv kind net index
  forM_ originM $ \origin ->
    unless (originScripts origin == kindScripts kind && originPath origin == toGeneric path) $
      Left ("The key is not that of the " <> kindName kind <> " account " <> cs (pathToStr path) <> ": its origin is " <> originText origin)
  return (WalletKey r net kind index xpub (originFingerprint <$> originM))
  where
    originM = parseOrigin =<< recordOrigin r
    failing :: Either String a -> Either Text a
    failing = either (Left . cs) Right
    noNetworkKey = "The record names no network. hw imports the wallets exportwallet writes, not a key alone: a key is one account, and does not say which portfolio it belongs to"
    noTypeKey = "The record does not say the type of its account. hw imports the wallets exportwallet writes, not a key alone"

-- | Import the wallets the key records name, within the scope, and say
-- what became of each wallet, of the labels of the wallets and of their
-- portfolios, and of each account, in the order of the records.
--
-- The keys are sorted by wallet, by the fingerprint of their origin. A
-- key without an origin names the wallet of the account that holds it in
-- the database, and is left aside when none does. Each wallet is then
-- imported ('importWallet').
--
-- The scope leaves out the keys of the other portfolios, and the wallets
-- of the other fingerprints. The answer says nothing of a key left out
-- this way, since the user asked for it.
importKeys :: (MonadUnliftIO m) => Ctx -> ImportScope -> [Record] -> ExceptT String (DB m) ([WalletImport], LabelsTaken, [AccountImport])
importKeys ctx scope records = do
  let readKeys = [(r, readWalletKey ctx r) | r <- records]
      ofPortfolio :: WalletKey -> Bool
      ofPortfolio key = maybe True (== key.walletKeyIndex) scope.scopePortfolio
      ofWallet :: Fingerprint -> Bool
      ofWallet fingerprint = maybe True (== fingerprint) scope.scopeWallet
  -- Each key of a portfolio in scope, with the fingerprint of the wallet
  -- it names
  named <- forM [key | (_, Right key) <- readKeys, ofPortfolio key] $ \key ->
    case key.walletKeyFingerprint of
      Just fingerprint -> return (key, Just fingerprint)
      Nothing -> do
        heldM <- lift $ accountWithKey key.walletKeyNetwork ctx key.walletKeyXPub
        ownerM <- forM heldM $ \(_, acc) -> portfolioOfAccount acc
        return (key, portfolioFingerprint <$> ownerM)
  let fingerprints = nub [fingerprint | (_, Just fingerprint) <- named, ofWallet fingerprint]
  imported <- forM fingerprints $ \fingerprint ->
    importWallet ctx scope fingerprint [key | (key, Just other) <- named, other == fingerprint]
  let outcomes = concat [keyOutcomes | (_, _, keyOutcomes) <- imported]
      -- The keys of the wallets --wallet leaves out
      ofOtherWallet = [key.walletKeyRecord | (key, Just fingerprint) <- named, not (ofWallet fingerprint)]
      outcomeOf r = case lookup r [(key.walletKeyRecord, outcome) | (key, outcome) <- outcomes] of
        Just outcome -> Just outcome
        Nothing -> case lookup r readKeys of
          Just (Left why) -> Just (AccountLeftAside (shownKey r) why)
          Just (Right key)
            | not (ofPortfolio key) || r `elem` ofOtherWallet -> Nothing
            | otherwise -> Just (AccountLeftAside (shownKey r) "The record has no origin, and no account of the database holds its key: nothing says which wallet it is of")
          Nothing -> Nothing
  return
    ( [wallet | (wallet, _, _) <- imported],
      mconcat [labels | (_, labels, _) <- imported],
      mapMaybe outcomeOf records
    )

-- | Import the keys of one wallet of a file, the wallet of the
-- fingerprint, and say what became of the wallet, of the labels of the
-- wallet and of its portfolios, and of each key.
--
-- Each portfolio of the file is taken whole ('importPortfolio'). A wallet
-- the database does not have is created, with its portfolio0, which every
-- wallet has: a file that does not bring that portfolio is refused, and
-- so is importwallet --portfolio N of such a wallet, with the advice to
-- import the whole file, or the whole wallet when --wallet names it. The
-- wallet takes the label the file gives it as its description when it
-- has none ('takeLabel').
importWallet ::
  (MonadUnliftIO m) =>
  Ctx ->
  ImportScope ->
  Fingerprint ->
  [WalletKey] ->
  ExceptT String (DB m) (WalletImport, LabelsTaken, [(WalletKey, AccountImport)])
importWallet ctx scope fingerprint keys = do
  existing <- lift $ walletWithFingerprint fingerprint
  let indexes = sort (nub ((.walletKeyIndex) <$> keys))
      shownFingerprint = cs (fingerprintToText fingerprint) :: String
  when (isNothing existing && 0 `notElem` indexes) $
    throwError $ case scope.scopePortfolio of
      Just index ->
        "The database has no wallet of the fingerprint "
          <> shownFingerprint
          <> ", and importwallet --portfolio "
          <> show index
          <> " does not create one: a wallet is created with its portfolio0. "
          <> case scope.scopeWallet of
            -- The file may hold other wallets, which the user left out
            Just _ -> "Import the whole wallet, without --portfolio"
            Nothing -> "Import the whole file, without --portfolio"
      Nothing ->
        "The file does not have portfolio0 of the wallet "
          <> shownFingerprint
          <> ", which the database does not have: a wallet is created with its portfolio0. \
             \Import a file of the whole wallet, as exportwallet writes it without --portfolio"
  (walletId, created) <- case existing of
    Just (walletId, _) -> return (walletId, False)
    Nothing -> (\(walletId, _) -> (walletId, True)) <$> lift (insertWallet fingerprint)
  held <- getWalletById walletId
  walletLabel <-
    takeLabel
      (dBWalletDescription held)
      (firstLabel (recordWallet . (.walletKeyRecord) <$> keys))
      (setWalletDescription walletId)
  wallet <- getWalletById walletId
  imported <- forM indexes $ \index ->
    importPortfolio ctx (walletId, wallet) created index [key | key <- keys, key.walletKeyIndex == index]
  let summary =
        WalletImport
          { walletImportIndex = walletIndex wallet,
            walletImportFingerprint = fingerprint,
            walletImportCreated = created,
            walletImportPortfolios = [portfolio | (portfolio, _, _) <- imported]
          }
      labels = LabelsTaken [walletLabel] [portfolioLabel | (_, portfolioLabel, _) <- imported]
  return (summary, labels, concat [keyOutcomes | (_, _, keyOutcomes) <- imported])

-- | The first label of a wallet or a portfolio that its key records give,
-- leaving out empty ones. Each key record carries the labels, so a file
-- joined from two may give two. The first one is taken.
firstLabel :: [Maybe Text] -> Maybe Text
firstLabel = listToMaybe . filter (not . Text.null) . catMaybes

-- | Give a wallet or a portfolio the label of the file as its
-- description, with the action that sets it, and say what became of the
-- label. The description it has is given first. One it has is kept: the
-- user set it, and an import adds to what the user did, and undoes
-- nothing of it.
takeLabel :: (Monad m) => Text -> Maybe Text -> (Text -> m ()) -> m LabelOutcome
takeLabel held givenM setDescription = case givenM of
  Nothing -> return NoLabelGiven
  Just given
    | given == held -> return LabelTaken
    | Text.null held -> LabelTaken <$ setDescription given
    | otherwise -> return OwnLabelKept

-- | Import the keys of one portfolio of a wallet, at an index, and say
-- what became of the portfolio, of its label and of each key. The flag
-- says whether the import has just created the wallet, which the refusals
-- then name by its fingerprint: a refusal undoes it.
--
-- A portfolio the wallet has is whole already. Every key of the file must
-- be the one its account holds, or the import is refused: the file and
-- the database are then not of one mnemonic. Nothing is added to it, and
-- it takes the label the file gives it as its description when it has
-- none ('takeLabel').
--
-- A portfolio the wallet does not have is created with the twelve
-- accounts of the file, and with the label the file gives it as its
-- description: the file must give a key for each of them
-- ('wholePortfolioKeys').
importPortfolio ::
  (MonadUnliftIO m) =>
  Ctx ->
  (DBWalletId, DBWallet) ->
  Bool ->
  Natural ->
  [WalletKey] ->
  ExceptT String (DB m) (PortfolioImport, LabelOutcome, [(WalletKey, AccountImport)])
importPortfolio ctx (walletId, wallet) newWallet index keys = do
  existing <- lift $ portfolioAt walletId index
  let description = firstLabel (recordLabel . (.walletKeyRecord) <$> keys)
      place
        | newWallet = cs (portfolioName index) <> " of the wallet " <> cs (fingerprintToText (walletFingerprint wallet))
        | otherwise = cs (portfolioName index) <> " of " <> cs (walletName (walletIndex wallet))
      refOf :: Portfolio -> WalletKey -> AccountRef
      refOf portfolio key = AccountRef (portfolioPlace portfolio key.walletKeyNetwork) key.walletKeyKind
  case existing of
    Just (portfolioKey, row) -> do
      let portfolio = Portfolio portfolioKey row walletId wallet
      outcomes <- forM keys $ \key -> do
        (_, acc) <- poolAccountOf (portfolioPool portfolio key.walletKeyNetwork) key.walletKeyKind
        unless (accountXPubKey ctx acc == key.walletKeyXPub) $
          throwError $
            "The " <> cs (kindName key.walletKeyKind) <> " account of " <> portfolioText portfolio <> " on " <> key.walletKeyNetwork.name
              <> " has another key than the file gives it: the file and the database are not of one mnemonic"
        return (key, AccountHeld (refOf portfolio key))
      label <- takeLabel (dBPortfolioDescription row) description (setPortfolioDescription portfolioKey)
      return (PortfolioImport index False, label, outcomes)
    Nothing -> do
      accountKeys <- liftEither $ wholePortfolioKeys place keys
      portfolio <- createPortfolioWith ctx (walletId, wallet) index (fromMaybe "" description) accountKeys
      let label = maybe NoLabelGiven (const LabelTaken) description
      return (PortfolioImport index True, label, [(key, AccountAdded (refOf portfolio key)) | key <- keys])

-- | The key of each of the twelve accounts of a portfolio, from the keys
-- of a file for it, in the order of 'portfolioAccounts'. A record may be
-- in the file twice, as when two files were joined, but an account with
-- two keys is refused, and so is a portfolio the file does not give every
-- account of: hw creates a portfolio whole or not at all. The text names
-- the portfolio, for the refusals.
wholePortfolioKeys :: String -> [WalletKey] -> Either String [(Network, AccountKind, XPubKey)]
wholePortfolioKeys place keys = do
  found <- forM portfolioAccounts $ \(net, kind) ->
    case nubBy sameKey [key.walletKeyXPub | key <- keys, key.walletKeyNetwork == net, key.walletKeyKind == kind] of
      [] -> Right Nothing
      [xpub] -> Right (Just (net, kind, xpub))
      _ -> Left ("The file gives two keys for the " <> cs (kindName kind) <> " account of " <> place <> " on " <> net.name)
  let given = catMaybes found
  unless (length given == length portfolioAccounts) $
    Left $
      "The file gives "
        <> show (length given)
        <> " of the "
        <> show (length portfolioAccounts)
        <> " accounts of "
        <> place
        <> ", which the database does not have: importwallet creates a portfolio only whole, with every account. \
           \Write the file with exportwallet on the computer that has the whole portfolio"
  return given

{- Labels, locks and what is left aside -}

-- | The accounts of the database a record is of: those on the network it
-- names, when it names one, and of those, the accounts its origin names,
-- when it has one hw can read: of its kind, at its path, in the wallet of
-- its seed. Nothing when the record has neither, and every account is a
-- candidate.
accountsOfRecord :: [(Portfolio, DBAccountId, DBAccount)] -> Record -> Maybe [(Portfolio, DBAccountId, DBAccount)]
accountsOfRecord accounts r =
  case (recordNetwork r, recordOrigin r) of
    (Nothing, Nothing) -> Nothing
    (netM, originM) -> Just (filter (\held -> onNetwork netM held && ofOrigin originM held) accounts)
  where
    onNetwork netM (_, _, acc) = maybe True (== dBAccountNetwork acc) netM
    ofOrigin originTextM (portfolio, _, acc) = case originTextM of
      Nothing -> True
      Just text -> case parseOrigin text of
        Just origin
          | Just kind <- kindOfScripts (originScripts origin) ->
              accountKind acc == kind
                && toGeneric (accountPath portfolio acc) == originPath origin
                && originFingerprint origin == portfolioFingerprint portfolio
        _ -> False

-- | Import a record of anything but a key, and count what was done with it
importRecord :: (MonadUnliftIO m) => Ctx -> [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> (Natural, Record) -> ExceptT String (DB m) ImportReport
importRecord ctx accounts report (number, r) =
  case recordType r of
    AddrRecord -> labelAddressRecord ctx accounts report r (\acc -> textToAddr (accountNetwork acc) (recordRef r))
    PubkeyRecord
      | isJust (publicKeyOf ctx (recordRef r)) ->
          labelAddressRecord ctx accounts report r (\acc -> publicKeyAddress ctx (accountKind acc) (recordRef r))
      | otherwise -> return (leftAside report)
    TxRecord ->
      case hexToTxHash (recordRef r) of
        Just tid -> labelTxRecord accounts report r tid
        Nothing -> return (unreadable report)
    OutputRecord ->
      case readOutpoint (recordRef r) of
        Just outpoint -> lockOutputRecord accounts report r outpoint
        Nothing -> return (unreadable report)
    InputRecord -> return (leftAside report)
    SpscanRecord -> return (leftAside report)
    XpubRecord -> return (leftAside report)
  where
    leftAside rep = rep {reportNotKept = reportNotKept rep + 1}
    unreadable rep = rep {reportUnreadable = reportUnreadable rep <> [number]}

-- | A public key written in hexadecimal, compressed or not. None for a
-- key of another kind, such as an x-only key.
publicKeyOf :: Ctx -> Text -> Maybe PublicKey
publicKeyOf ctx hex = do
  bytes <- decodeHex hex
  either (const Nothing) Just (unmarshal ctx bytes)

-- | The address of a public key written in hexadecimal in an account of
-- the kind: its P2PKH address in a BIP44 account, compressed or not, its
-- P2WPKH address in a BIP84 one, which an uncompressed key does not have
publicKeyAddress :: Ctx -> AccountKind -> Text -> Maybe Address
publicKeyAddress ctx kind hex = do
  key <- publicKeyOf ctx hex
  case kind of
    Bip44 -> Just (pubKeyAddr ctx key)
    Bip84 -> pubKeyWitnessAddr ctx key

-- | Label the address of a record, read in each account as the function
-- gives it: see 'placeAddress'
labelAddressRecord :: (MonadUnliftIO m) => Ctx -> [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> (DBAccount -> Maybe Address) -> ExceptT String (DB m) ImportReport
labelAddressRecord ctx accounts report r readOn =
  case recordLabel r of
    Nothing -> return report
    Just label -> do
      placed <- placeAddress ctx candidates readOn (parseKeypath =<< recordKeypath r)
      case placed of
        PlacedAt accId addr
          | dBAddressInternal addr -> return report {reportChangeAddresses = reportChangeAddresses report + 1}
          | otherwise -> do
              _ <- setAddrLabel accId (dBAddressIndex addr) label
              return report {reportAddressLabels = reportAddressLabels report + 1}
        PlacedOnChangeChain -> return report {reportChangeAddresses = reportChangeAddresses report + 1}
        PlacedTooFar -> return report {reportAddressesMissing = reportAddressesMissing report + 1, reportAddressesTooFar = reportAddressesTooFar report + 1}
        NotPlaced -> return report {reportAddressesMissing = reportAddressesMissing report + 1}
  where
    -- The accounts the record names, or every account when it names none
    -- the wallet has. An address is of one account at most on a network,
    -- so what the record says only narrows the search.
    candidates = [(accId, acc) | (_, accId, acc) <- narrowed]
    narrowed = case accountsOfRecord accounts r of
      Just named@(_ : _) -> named
      _ -> accounts

-- | Where the address of a record is in the wallet
data Placement
  = -- | The row of an account has it
    PlacedAt !DBAccountId !DBAddress
  | -- | Its key path derives it on the change chain of an account, where
    -- it takes no label and is not generated
    PlacedOnChangeChain
  | -- | Its key path derives it on the receiving chain of an account,
    -- beyond 'scanLimit' past the last used address, further than hw
    -- generates on the word of a file ('withinScanLimit')
    PlacedTooFar
  | -- | Nowhere hw can tell
    NotPlaced

-- | Find the address of a record among the addresses of the accounts, read
-- in each, on its network and of its kind; or derive it from its key path
-- in the account whose key gives it, which then generates its receiving
-- chain up to it, as the wallet that wrote the file had it, within
-- 'scanLimit' of its last used address ('withinScanLimit'): a file names
-- any path, and hw does not generate for days on its word
placeAddress :: (MonadUnliftIO m) => Ctx -> [(DBAccountId, DBAccount)] -> (DBAccount -> Maybe Address) -> Maybe SoftPath -> ExceptT String (DB m) Placement
placeAddress ctx accounts readIn keypathM = do
  heldM <- firstJust accounts $ \held@(accId, acc) -> do
    rows <- lift $ storedAmong (maybeToList (readIn acc)) held
    return $ PlacedAt accId <$> listToMaybe rows
  case (heldM, chainIndex =<< keypathM) of
    (Just placed, _) -> return placed
    (Nothing, Nothing) -> return NotPlaced
    (Nothing, Just (addrType, index)) -> fromMaybe NotPlaced <$> firstJust accounts (derived addrType index)
  where
    derived addrType index (accId, acc) =
      case readIn acc of
        Nothing -> return Nothing
        Just addr -> do
          derivedM <- liftIO $ deriveChainAddress ctx (accountKind acc) (accountXPubKey ctx acc) (addrDeriv addrType) index
          if derivedM /= Just addr
            then return Nothing
            else case addrType of
              AddrInternal -> return (Just PlacedOnChangeChain)
              AddrExternal -> do
                within <- lift $ withinScanLimit accId AddrExternal index
                if not within
                  then return (Just PlacedTooFar)
                  else do
                    extendChainTo ctx accId AddrExternal (fromIntegral index + 1)
                    rowM <- lift $ P.get (DBAddressKey accId False (fromIntegral index))
                    return $ PlacedAt accId <$> rowM

-- | The first result of the action on the items, if any
firstJust :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstJust [] _ = return Nothing
firstJust (x : xs) f = do
  found <- f x
  maybe (firstJust xs f) (return . Just) found

-- | Label the transaction of a record. A pending transaction takes the
-- label in the portfolio that holds it. Otherwise only an origin places
-- the transaction in a portfolio before a sync has it: the record labels
-- it in the portfolios on the network whose accounts its origin names,
-- whether they have the transaction yet or not, since the labels of
-- transactions are kept apart from the transactions. A record without an
-- origin labels the transaction in the portfolios that have it, on its
-- network when it names one, and is counted among the transactions the
-- wallet does not have yet when none has it.
--
-- A network alone names no portfolio. A file of another wallet may give
-- no origin, and its records would otherwise label the transaction in
-- every portfolio on their network, which the next export would write
-- under each of them.
labelTxRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> TxHash -> ExceptT String (DB m) ImportReport
labelTxRecord accounts report r tid =
  case recordLabel r of
    Nothing -> return report
    Just label -> do
      pendingM <- findPendingTx tid
      case pendingM of
        Just held | maybe (onNetwork held.pendingTxPool) (held.pendingTxPool `elem`) named -> do
          lift $ setPendingTxLabel held label
          return (labelled 1)
        _ -> case named of
          Just [] -> return report {reportOtherAccounts = reportOtherAccounts report + 1}
          Just ofRecord -> labelIn ofRecord label
          Nothing -> do
            pools <- filter onNetwork <$> lift allPools
            holders <- filterM (\pool -> lift (isJust <$> P.get (txInfoKey pool tid))) pools
            if null holders
              then return report {reportTxsMissing = reportTxsMissing report + 1}
              else labelIn holders label
  where
    -- The pools of the accounts the origin of the record names, on its
    -- network when it names one, or Nothing when it has no origin
    named = case recordOrigin r of
      Nothing -> Nothing
      Just _ -> nub . map (\(_, _, acc) -> accountPool acc) <$> accountsOfRecord accounts r
    -- Whether a pool is on the network the record names, any pool when
    -- it names none
    onNetwork pool = maybe True (== poolNetworkName pool) (recordNetwork r)
    labelled n = report {reportTxLabels = reportTxLabels report + n}
    labelIn pools label = do
      forM_ pools $ \pool -> lift (setTxLabel pool tid label)
      return (labelled (fromIntegral (length pools)))

-- | Lock or unlock the coin of a record as its spendable flag says, when
-- the wallet has the coin, in the accounts the record names or in any. A
-- record with a label alone is left aside: hw keeps no label on a coin.
lockOutputRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> OutPoint -> ExceptT String (DB m) ImportReport
lockOutputRecord accounts report r outpoint = do
  rows <- lift $ coinRows outpoint
  let named = map (\(_, accId, _) -> accId) <$> accountsOfRecord accounts r
      held = [row | row <- rows, maybe True (dBCoinAccount row `elem`) named]
  case (held, recordSpendable r) of
    ([], _) -> return report {reportCoinsMissing = reportCoinsMissing report + 1}
    (_, Nothing) -> return report {reportNotKept = reportNotKept report + 1}
    (_, Just spendable) -> do
      forM_ held $ \row -> setLockedByUser (dBCoinAccount row) outpoint (not spendable)
      return $
        if spendable
          then report {reportCoinsUnlocked = reportCoinsUnlocked report + 1}
          else report {reportCoinsLocked = reportCoinsLocked report + 1}

{- The file show shows -}

-- | A file of a wallet as show shows it, before the user imports it. Each
-- wallet of the file comes as importwallet would leave it, and each
-- account with the labels and locks of its records. Then come the labels
-- and locks of no account of the file, the records hw keeps no label for,
-- and the lines that are not records.
--
-- A record goes under the account of the file whose origin and network it
-- names. This reads only what the file says. importwallet places a record
-- in the wallet itself, by the address or the transaction, and may place
-- one show shows apart.
data FileShown = FileShown
  { -- | Each wallet of the file, as importwallet would leave it
    shownWallets :: ![WalletImport],
    -- | Each account of the file, in its order, with the records of the
    -- labels and locks hw keeps
    shownAccounts :: ![(AccountImport, [Record])],
    -- | The records of labels and locks that name no account of the file
    shownOthers :: ![Record],
    -- | The records of the portfolios --wallet or --portfolio leaves
    -- out, which importwallet would not read
    shownOtherPortfolios :: !Natural,
    -- | The labels of change addresses, which hw does not keep
    shownChangeLabels :: !Natural,
    -- | The records hw keeps no label for: inputs, the labels of coins,
    -- public keys it cannot read, and the kinds the BIP does not define
    shownNotKept :: !Natural,
    -- | The lines that are not records, as a text editor numbers them
    shownUnreadable :: ![Natural]
  }
  deriving (Eq, Show)

-- | Whether a file holds anything a file of a wallet holds: a key or a
-- record, even one of a type the BIP does not define. A file of other
-- lines alone is no file of a wallet.
holdsAnything :: WalletFile -> Bool
holdsAnything file = not (null (fileKeys file) && null (fileRecords file) && fileOtherTypes file == 0)

-- | Show a file of a wallet. Its wallets are imported into the database as
-- importwallet imports them, then that is undone ('undoneAfter'), so that
-- the database keeps nothing. Each wallet and each account shows as
-- importwallet would leave it: created, found, added or left aside, with
-- the reasons importwallet gives. A file that importwallet would refuse is
-- refused with its refusal, and so is a scope the file cannot answer
-- ('checkScopeHeld').
--
-- Importing the wallets and undoing it gives the very answer importwallet
-- would give, without its rules written twice, once to do and once to
-- tell what it would do. The two could not drift apart.
fileShown :: (MonadUnliftIO m) => Ctx -> ImportScope -> WalletFile -> ExceptT String (DB m) FileShown
fileShown ctx scope file = do
  liftEither $ checkScopeHeld ctx scope (fileKeys file) (snd <$> fileRecords file)
  (wallets, _, outcomes) <- liftEither =<< lift (undoneAfter (runExceptT (importKeys ctx scope (fileKeys file))))
  let (inScope, leftOut) = partition (recordInScope scope . snd) (fileRecords file)
      shown = sortRecords ctx file {fileRecords = inScope} wallets outcomes
  return shown {shownOtherPortfolios = fromIntegral (length leftOut)}

-- | Sort the records of a file under the accounts of its keys, given what
-- became of each wallet and of each account
sortRecords :: Ctx -> WalletFile -> [WalletImport] -> [AccountImport] -> FileShown
sortRecords ctx file wallets outcomes =
  FileShown
    { shownWallets = wallets,
      shownAccounts = [(outcome, [r | (Just at, r) <- placed, at == i]) | (i, outcome) <- zip [0 ..] outcomes],
      shownOthers = [r | (Nothing, r) <- placed],
      shownOtherPortfolios = 0,
      shownChangeLabels = fromIntegral (length [() | ChangeLabel <- sorted]),
      shownNotKept = fileOtherTypes file + fromIntegral (length [() | NotKept <- sorted]),
      shownUnreadable = sort (fileUnreadable file <> [number | UnreadableRef number <- sorted])
    }
  where
    sorted = sortRecord ctx <$> fileRecords file
    placed = [(accountOf r, r) | ShownRecord r <- sorted]
    -- What names each account of the file: its network, and its origin,
    -- from the fingerprint of its wallet
    names = accountName <$> outcomes
    accountName :: AccountImport -> Maybe (Text, Maybe Origin)
    accountName outcome = do
      ref <- importedAccount outcome
      let place = accountRefPlace ref
          fingerprintM = listToMaybe [walletImportFingerprint wallet | wallet <- wallets, walletImportIndex wallet == refWallet (placePortfolio place)]
      return (cs (placeNetwork place).name, (\fingerprint -> accountOrigin (accountRefKind ref) fingerprint (accountRefPath ref)) <$> fingerprintM)
    -- The place of the account of a record among the accounts of the
    -- file: the one on its network with its origin
    accountOf :: Record -> Maybe Int
    accountOf r = do
      origin <- parseOrigin =<< recordOrigin r
      let matches (netName, originM) = originM == Just origin && maybe True (== netName) (recordNetwork r)
      elemIndex True (maybe False matches <$> names) <|> Nothing

-- | What show makes of a record of a file, as importwallet would take it
data SortedRecord
  = -- | A label or a lock that hw keeps
    ShownRecord !Record
  | -- | The label of a change address, by its key path, which hw does not
    -- keep
    ChangeLabel
  | -- | A record hw keeps no label for
    NotKept
  | -- | A record whose reference does not read, by the number of its line
    UnreadableRef !Natural
  | -- | A record that neither labels nor locks anything, which
    -- importwallet passes over without a word
    NothingSet

-- | Sort a record as importwallet takes it ('importRecord'). An address
-- on the change chain is told by its key path alone, since show does not
-- look for the address in the wallet.
sortRecord :: Ctx -> (Natural, Record) -> SortedRecord
sortRecord ctx (number, r) =
  case recordType r of
    AddrRecord -> addressLabel
    PubkeyRecord
      | isJust (publicKeyOf ctx (recordRef r)) -> addressLabel
      | otherwise -> NotKept
    TxRecord
      | isNothing (hexToTxHash (recordRef r)) -> UnreadableRef number
      | isNothing (recordLabel r) -> NothingSet
      | otherwise -> ShownRecord r
    OutputRecord
      | isNothing (readOutpoint (recordRef r)) -> UnreadableRef number
      | isNothing (recordSpendable r) -> NotKept
      | otherwise -> ShownRecord r
    InputRecord -> NotKept
    SpscanRecord -> NotKept
    XpubRecord -> NotKept
  where
    addressLabel
      | isNothing (recordLabel r) = NothingSet
      | Just (AddrInternal, _) <- chainIndex =<< parseKeypath =<< recordKeypath r = ChangeLabel
      | otherwise = ShownRecord r

-- | The keys of a file as the --json answer of show writes them: each
-- wallet and each account as the answer of importwallet writes them, each
-- account with its records ("records"), then the records of no account of
-- the file, and the counts of what hw keeps no label for
fileShownPairs :: FileShown -> [Pair]
fileShownPairs shown =
  [ "wallets" .= shownWallets shown,
    "accounts" .= [Json.object (accountImportPairs outcome <> ["records" .= records]) | (outcome, records) <- shownAccounts shown],
    "records" .= shownOthers shown,
    "otherportfolios" .= shownOtherPortfolios shown,
    "changeaddresses" .= shownChangeLabels shown,
    "notkept" .= shownNotKept shown,
    "unreadable" .= shownUnreadable shown
  ]

parseFileShown :: Json.Object -> Parser FileShown
parseFileShown o =
  FileShown
    <$> o .: "wallets"
    <*> (mapM shownAccount =<< o .: "accounts")
    <*> o .: "records"
    <*> o .: "otherportfolios"
    <*> o .: "changeaddresses"
    <*> o .: "notkept"
    <*> o .: "unreadable"
  where
    shownAccount value = (,) <$> parseAccountImport value <*> Json.withObject "account" (.: "records") value

{- The report in JSON -}

-- | The keys of a report, as the --json answer of importwallet writes them
reportPairs :: ImportReport -> [Pair]
reportPairs report =
  [ "wallets" .= reportWallets report,
    "accounts" .= (Json.object . accountImportPairs <$> reportAccounts report),
    "walletlabels" .= reportWalletLabels report,
    "portfoliolabels" .= reportPortfolioLabels report,
    "addresslabels" .= reportAddressLabels report,
    "txlabels" .= reportTxLabels report,
    "coinslocked" .= reportCoinsLocked report,
    "coinsunlocked" .= reportCoinsUnlocked report,
    "addressesmissing" .= reportAddressesMissing report,
    "addressestoofar" .= reportAddressesTooFar report,
    "txsmissing" .= reportTxsMissing report,
    "coinsmissing" .= reportCoinsMissing report,
    "otheraccounts" .= reportOtherAccounts report,
    "otherportfolios" .= reportOtherPortfolios report,
    "walletlabelskept" .= reportWalletLabelsKept report,
    "portfoliolabelskept" .= reportPortfolioLabelsKept report,
    "changeaddresses" .= reportChangeAddresses report,
    "notkept" .= reportNotKept report,
    "unreadable" .= reportUnreadable report
  ]

parseReport :: Json.Object -> Parser ImportReport
parseReport o =
  ImportReport
    <$> o .: "wallets"
    <*> (mapM parseAccountImport =<< o .: "accounts")
    <*> o .: "walletlabels"
    <*> o .: "portfoliolabels"
    <*> o .: "addresslabels"
    <*> o .: "txlabels"
    <*> o .: "coinslocked"
    <*> o .: "coinsunlocked"
    <*> o .: "addressesmissing"
    <*> o .: "addressestoofar"
    <*> o .: "txsmissing"
    <*> o .: "coinsmissing"
    <*> o .: "otheraccounts"
    <*> o .: "otherportfolios"
    <*> o .: "walletlabelskept"
    <*> o .: "portfoliolabelskept"
    <*> o .: "changeaddresses"
    <*> o .: "notkept"
    <*> o .: "unreadable"

instance Json.ToJSON PortfolioImport where
  toJSON p =
    Json.object
      [ "portfolio" .= portfolioImportIndex p,
        "created" .= portfolioImportCreated p
      ]

instance Json.FromJSON PortfolioImport where
  parseJSON = Json.withObject "portfolio import" $ \o ->
    PortfolioImport <$> o .: "portfolio" <*> o .: "created"

instance Json.ToJSON WalletImport where
  toJSON w =
    Json.object
      [ "wallet" .= walletImportIndex w,
        "fingerprint" .= walletImportFingerprint w,
        "created" .= walletImportCreated w,
        "portfolios" .= walletImportPortfolios w
      ]

instance Json.FromJSON WalletImport where
  parseJSON = Json.withObject "wallet import" $ \o ->
    WalletImport <$> o .: "wallet" <*> o .: "fingerprint" <*> o .: "created" <*> o .: "portfolios"

-- | The keys of what became of an account, which show adds the records of
-- the account to: its status, the account when the wallet has it, and the
-- start of the key and the reason of one left aside
accountImportPairs :: AccountImport -> [Pair]
accountImportPairs = \case
  AccountAdded ref -> ["status" .= ("added" :: Text), "account" .= ref]
  AccountHeld ref -> ["status" .= ("held" :: Text), "account" .= ref]
  AccountLeftAside name why -> ["status" .= ("leftaside" :: Text), "name" .= name, "reason" .= why]

parseAccountImport :: Json.Value -> Parser AccountImport
parseAccountImport = Json.withObject "account import" $ \o -> do
  status <- o .: "status"
  case status :: Text of
    "added" -> AccountAdded <$> o .: "account"
    "held" -> AccountHeld <$> o .: "account"
    "leftaside" -> AccountLeftAside <$> o .: "name" <*> o .: "reason"
    _ -> fail "Unknown account import status"
