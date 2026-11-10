{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | A wallet and its labels as a BIP 329 file ("Haskoin.Wallet.Bip329"),
-- which exportwallet writes and importwallet reads.
--
-- An account goes in the file as an @xpub@ record: its key, its origin
-- @pkh([fingerprint/44'/coin'/index'])@, or
-- @wpkh([fingerprint/84'/coin'/index'])@ for a BIP84 account, when hw
-- knows the fingerprint of the seed, and under hw's own keys its network,
-- its purpose, 44 or 84, and the description of its wallet. The label of
-- the record is the description of the portfolio. Then come the labelled
-- receiving addresses of the account, each with its key path, the
-- labelled transactions of its portfolio on its network, with their
-- height, fee and value when the chain has them, and the coins the user
-- locked, as outputs that are not spendable. Every record carries the
-- origin of its account and the name of its network: the test networks
-- share their keys, so the origin alone does not tell one from another.
--
-- A file holds every account of the portfolios it was asked for, so that
-- a wallet goes from the computer that has the mnemonic to the one that
-- watches the chain in one file.
--
-- Reading a file, importwallet takes the @xpub@ records first. They are
-- sorted by the fingerprint of their seed, which names their wallet, and
-- by the index of their path, which names their portfolio. A wallet or a
-- portfolio the database does not have is created. A portfolio it has is
-- merged with the one of the file: the accounts it lacks are added, and
-- the accounts it has must hold the keys the file gives them. This merge
-- is how the partial portfolios made from a database of hw 0.9 are filled
-- in, on a computer that never sees the mnemonic. When the two cannot be
-- merged, because an account of the file has another key than the same
-- account of the database, the import is refused whole. So is a key that
-- another account of the network holds, whatever child number the file
-- writes it with, and two keys for accounts of one path, as the test
-- networks share theirs ("Haskoin.Wallet.Database.Account".'insertAccount').
--
-- Nothing in a file proves that its keys are those of the mnemonic of the
-- wallet it names: only the mnemonic could. importwallet trusts the file,
-- as any import of public keys must, and the help says to import only a
-- file the user wrote on their own signer, or got over a channel they
-- trust.
--
-- A wallet made from a database of hw 0.9 has no fingerprint. It is
-- recognized by its keys: a file that gives a key of such a wallet is of
-- that wallet, which then takes the fingerprint of the file. hw 0.9 could
-- not tell that the accounts of one seed on Bitcoin and on Bitcoin Cash
-- belonged together and left them in two wallets, which the file brings
-- into one.
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
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import Data.Bits (clearBit, testBit)
import Data.List (elemIndex, nub, partition, sort, sortOn)
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
import Haskoin.Wallet.Keys (AccountKind (..), accountDeriv, checkKindOnNetwork, kindName, kindOfPurpose, kindPurpose, xPubIndex)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), isSigned)
import Haskoin.Wallet.Util (visibleText)
import Numeric.Natural (Natural)

{- The file exportwallet writes -}

-- | The records of portfolios, in their order: those of each account of
-- each ('accountRecords')
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
    originM = originText . (\fingerprint -> accountOrigin kind fingerprint (accountPath portfolio acc)) <$> portfolioFingerprint portfolio
    -- Every record names the account it is of: its origin, when the seed
    -- is known, and its network, which the origin does not tell
    ofAccount r = r {recordOrigin = originM, recordNetwork = Just (dBAccountNetwork acc)}
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
    -- | The records of the portfolios --portfolio leaves out, which are
    -- not read
    reportOtherPortfolios :: !Natural,
    -- | The labels of change addresses, which hw does not keep
    reportChangeAddresses :: !Natural,
    -- | The records hw keeps no label for: inputs, the labels of coins,
    -- public keys it cannot read, and the kinds the BIP does not define
    reportNotKept :: !Natural,
    -- | The lines that are not records, as a text editor numbers them
    reportUnreadable :: ![Natural]
  }
  deriving (Eq, Show)

-- | What became of a wallet the file names
data WalletImport = WalletImport
  { -- | The index of the wallet in the database
    walletImportIndex :: !Natural,
    -- | The fingerprint of its seed, when it is known now
    walletImportFingerprint :: !(Maybe Fingerprint),
    -- | Whether the import created it
    walletImportCreated :: !Bool,
    -- | The indexes of the wallets of the same seed merged into it, which
    -- are gone
    walletImportMerged :: ![Natural],
    -- | What became of each of its portfolios the file names, in the
    -- order of their indexes
    walletImportPortfolios :: ![PortfolioImport]
  }
  deriving (Eq, Show)

-- | What became of a portfolio the file names
data PortfolioImport = PortfolioImport
  { portfolioImportIndex :: !Natural,
    -- | Whether the import created it
    portfolioImportCreated :: !Bool,
    -- | How many accounts the import added to it
    portfolioImportAdded :: !Natural,
    -- | How many accounts it still lacks
    portfolioImportMissing :: !Natural
  }
  deriving (Eq, Show)

-- | What became of an account the file names
data AccountImport
  = -- | Added to its portfolio, which did not have it
    AccountAdded !AccountRef
  | -- | Its portfolio had it already, with the key of the file
    AccountHeld !AccountRef
  | -- | Not imported: the start of its key, and why. Its record is not one
    -- of an account of a wallet of hw.
    AccountLeftAside !Text !Text
  deriving (Eq, Show)

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
      reportChangeAddresses = 0,
      reportNotKept = 0,
      reportUnreadable = unreadable
    }

-- | What of a file importwallet takes: every portfolio of it, or the one
-- --portfolio names, into the wallet the file is of, which --wallet may
-- only confirm
data ImportScope = ImportScope
  { -- | The one portfolio to import, by its index, or all of them
    scopePortfolio :: !(Maybe Natural),
    -- | The wallet the file must be of, when the user names it
    scopeWallet :: !(Maybe (DBWalletId, DBWallet))
  }
  deriving (Eq, Show)

-- | Everything a file holds
wholeFile :: ImportScope
wholeFile = ImportScope Nothing Nothing

-- | Import the content of a file of a wallet, as 'readWalletFile' reads
-- it, within the scope given. A file that cannot be merged with the
-- database is refused whole: the accounts the import added before it
-- found that out are undone ('undoneIfRefused'), so that a refused file
-- changes nothing.
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
-- A scope that names a portfolio the records hold nothing of is refused
-- ('checkScopeHeld').
importRecords :: (MonadUnliftIO m) => Ctx -> ImportScope -> [Record] -> [(Natural, Record)] -> [Natural] -> ExceptT String (DB m) ImportReport
importRecords ctx scope keys others unreadable = do
  liftEither $ checkScopeHeld ctx scope keys (snd <$> others)
  (wallets, accountImports) <- importKeys ctx scope keys
  accounts <- lift getAccounts
  -- The records of the portfolios left out are counted, and not read:
  -- their labels would be looked for in the portfolios that are
  let (inScope, leftOut) = partition (recordInScope scope . snd) others
  report <- foldM (importRecord ctx accounts) (emptyReport wallets accountImports unreadable) inScope
  return report {reportOtherPortfolios = fromIntegral (length leftOut)}

-- | Whether a record is of a portfolio the scope reads. With --portfolio,
-- a record whose origin names an account of another portfolio is not. A
-- record without an origin hw can read names no portfolio, and is read:
-- the address or the transaction it labels says where it goes.
recordInScope :: ImportScope -> Record -> Bool
recordInScope scope r = case (scope.scopePortfolio, originAccountIndex =<< parseOrigin =<< recordOrigin r) of
  (Just wanted, Just index) -> index == wanted
  _ -> True

-- | Refuse a scope that names a portfolio the file holds nothing of: no
-- key record of that portfolio, and no other record whose origin names
-- it. importwallet --portfolio 2 of such a file would do nothing, and
-- exit as if it had: the index is more likely a typo, or the file the
-- wrong one. The refusal names the portfolios whose keys the file holds,
-- by their indexes, so that the user sees which index was meant. A file
-- of labels alone, from another wallet, holds no key, and may still name
-- the portfolio by the origins of its records.
checkScopeHeld :: Ctx -> ImportScope -> [Record] -> [Record] -> Either String ()
checkScopeHeld ctx scope keys others =
  forM_ scope.scopePortfolio $ \wanted ->
    unless (wanted `elem` keyIndexes || wanted `elem` recordIndexes) $
      Left $
        "The file has no "
          <> cs (portfolioName wanted)
          <> ": "
          <> case keyIndexes of
            [] -> "it holds the key of no portfolio"
            held -> "it has " <> intercalateAnd (cs . portfolioName <$> held)
  where
    -- The portfolios of the key records hw can read, each once, in the
    -- order of their indexes
    keyIndexes = sort (nub [key.walletKeyIndex | Right key <- readWalletKey ctx <$> keys])
    recordIndexes = mapMaybe (originAccountIndex <=< parseOrigin <=< recordOrigin) others

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
-- what became of each wallet and of each account, in the order of the
-- records.
--
-- The keys are sorted by wallet: by the fingerprint of their origin, or,
-- for a key without one, by the wallet of the account that holds it in
-- the database. A key that names no wallet either way is left aside. Each
-- wallet is then merged with the database ('importWallet').
importKeys :: (MonadUnliftIO m) => Ctx -> ImportScope -> [Record] -> ExceptT String (DB m) ([WalletImport], [AccountImport])
importKeys ctx scope records = do
  let readKeys = [(r, readWalletKey ctx r) | r <- records]
      inScope :: WalletKey -> Bool
      inScope key = maybe True (== key.walletKeyIndex) scope.scopePortfolio
  -- Each key in scope with the wallet it names
  named <- forM [key | (_, Right key) <- readKeys, inScope key] $ \key ->
    case key.walletKeyFingerprint of
      Just fingerprint -> return (key, Just (Right fingerprint))
      Nothing -> do
        heldM <- lift $ accountWithKey key.walletKeyNetwork ctx key.walletKeyXPub
        ownerM <- forM heldM $ \(_, acc) -> portfolioOfAccount acc
        return (key, Left . (.portfolioWalletId) <$> ownerM)
  let groups = nub [name | (_, Just name) <- named]
  imported <- forM groups $ \name ->
    importWallet ctx scope (either (const Nothing) Just name) [key | (key, Just other) <- named, other == name]
  let outcomes = concatMap snd imported
      outcomeOf r = case lookup r [(key.walletKeyRecord, outcome) | (key, outcome) <- outcomes] of
        Just outcome -> Just outcome
        Nothing -> case lookup r readKeys of
          Just (Left why) -> Just (AccountLeftAside (shownKey r) why)
          Just (Right key)
            | not (inScope key) -> Nothing
            | otherwise -> Just (AccountLeftAside (shownKey r) "The record has no origin, and no account of the database holds its key: nothing says which wallet it is of")
          Nothing -> Nothing
  return (fst <$> imported, mapMaybe outcomeOf records)

-- | Merge the keys of one wallet of a file into the database, and say
-- what became of the wallet and of each key.
--
-- The wallet of the database is the one with the fingerprint of the file,
-- or one without a fingerprint that holds a key of the file: a wallet
-- made from a database of hw 0.9, which the file then gives its
-- fingerprint. Several such wallets are of one seed, and are merged into
-- one ('mergeWallets'). Without any, a new wallet is created.
--
-- Each key goes to the portfolio of its index, created when the wallet
-- does not have it, with the description the file gives. An account the
-- portfolio has must hold the key of the file, or the import is refused:
-- the file and the database are then not of the same seed, and no merge
-- of them would be a wallet. A wallet always has portfolio 0, so an empty
-- one is made when the file brings none.
importWallet ::
  (MonadUnliftIO m) =>
  Ctx ->
  ImportScope ->
  Maybe Fingerprint ->
  [WalletKey] ->
  ExceptT String (DB m) (WalletImport, [(WalletKey, AccountImport)])
importWallet ctx scope fingerprintM keys = do
  -- The wallets that hold a key of the file, and the one of its fingerprint
  holders <- fmap (nub . catMaybes) . forM keys $ \key -> do
    heldM <- lift $ accountWithKey key.walletKeyNetwork ctx key.walletKeyXPub
    forM heldM $ \(_, acc) -> (.portfolioWalletId) <$> portfolioOfAccount acc
  byFingerprint <- maybe (return Nothing) (lift . walletWithFingerprint) fingerprintM
  holdersRows <- forM holders $ \walletId -> (walletId,) <$> getWalletById walletId
  let candidates = sortOn (walletIndex . snd) (nub (maybeToList byFingerprint <> holdersRows))
  forM_ ((,) <$> fingerprintM <*> pure candidates) $ \(fingerprint, found) ->
    forM_ found $ \(_, wallet) ->
      forM_ (walletFingerprint wallet) $ \known ->
        unless (known == fingerprint) $
          throwError $
            cs (walletName (walletIndex wallet)) <> " holds a key of the file, and is of the seed " <> cs (fingerprintToText known) <> ", not of " <> cs (fingerprintToText fingerprint) <> ": the two cannot be merged"
  ((targetId, _), created, merged) <- case candidates of
    [] -> (,True,[]) <$> lift (insertWallet fingerprintM)
    first@(firstId, _) : others -> do
      forM_ others (mergeWallets firstId)
      return (first, False, walletIndex . snd <$> others)
  forM_ fingerprintM $ \fingerprint ->
    lift $ P.update targetId [DBWalletFingerprint P.=. Just (fingerprintToText fingerprint)]
  wallet <- getWalletById targetId
  forM_ scope.scopeWallet $ \(wantedId, wanted) ->
    unless (wantedId == targetId) $
      throwError $
        "The file is not of " <> cs (walletName (walletIndex wanted)) <> ": it is of "
          <> (if created then "a wallet the database does not have" else cs (walletName (walletIndex wallet)))
  -- The description of the wallet, when it has none
  forM_ (listToMaybe (mapMaybe (recordWallet . (.walletKeyRecord)) keys)) $ \description ->
    when (Text.null (dBWalletDescription wallet)) $ setWalletDescription targetId description
  outcomes <- forM keys $ \key -> do
    let index = key.walletKeyIndex
        description = fromMaybe "" (recordLabel key.walletKeyRecord)
    existing <- lift $ portfolioAt targetId index
    (portfolioKey, row, isNew) <- case existing of
      Just (portfolioKey, row) -> return (portfolioKey, row, False)
      Nothing -> (\(portfolioKey, row) -> (portfolioKey, row, True)) <$> insertPortfolio targetId index description
    when (Text.null (dBPortfolioDescription row) && not (Text.null description)) $
      setPortfolioDescription portfolioKey description
    let portfolio = Portfolio portfolioKey row targetId wallet
        ref = AccountRef (portfolioPlace portfolio key.walletKeyNetwork) key.walletKeyKind
    heldM <- lift $ poolAccount (portfolioPool portfolio key.walletKeyNetwork) key.walletKeyKind
    outcome <- case heldM of
      Just (_, acc)
        | accountXPubKey ctx acc == key.walletKeyXPub -> return (AccountHeld ref)
        | otherwise ->
            throwError $
              "The " <> cs (kindName key.walletKeyKind) <> " account of " <> portfolioText portfolio <> " on " <> key.walletKeyNetwork.name
                <> " has another key than the file gives it: the two cannot be merged"
      Nothing -> AccountAdded ref <$ insertAccount portfolioKey key.walletKeyKind key.walletKeyNetwork ctx key.walletKeyXPub
    return (key, outcome, index, isNew)
  -- A wallet always has portfolio 0. The one made here is empty, and the
  -- answer shows it, so that the user is not surprised by a partial
  -- portfolio the file did not bring.
  zero <- lift $ portfolioAt targetId 0
  when (isNothing zero) $ void $ insertPortfolio targetId 0 ""
  let indexes = sort (nub ([0 | isNothing zero] <> [index | (_, _, index, _) <- outcomes]))
  portfolios <- forM indexes $ \index -> do
    held <- lift $ portfolioAt targetId index
    missing <- maybe (return 0) (fmap length . lift . missingAccounts . fst) held
    return
      PortfolioImport
        { portfolioImportIndex = index,
          portfolioImportCreated = (index == 0 && isNothing zero) || or [isNew | (_, _, i, isNew) <- outcomes, i == index],
          portfolioImportAdded = fromIntegral (length [() | (_, AccountAdded _, i, _) <- outcomes, i == index]),
          portfolioImportMissing = fromIntegral missing
        }
  let summary =
        WalletImport
          { walletImportIndex = walletIndex wallet,
            walletImportFingerprint = walletFingerprint wallet,
            walletImportCreated = created,
            walletImportMerged = merged,
            walletImportPortfolios = portfolios
          }
  return (summary, [(key, outcome) | (key, outcome, _, _) <- outcomes])

{- Labels, locks and what is left aside -}

-- | The accounts of the database a record is of: those on the network it
-- names, when it names one, and of those, the accounts its origin names,
-- when it has one hw can read: of its kind, at its path, in a wallet of
-- its seed or of a seed hw does not know yet. Nothing when the record has
-- neither, and every account is a candidate.
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
                && maybe True (== originFingerprint origin) (portfolioFingerprint portfolio)
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
-- A network alone names no portfolio. exportwallet writes no origin for a
-- wallet whose fingerprint hw does not know, and the records of such a
-- file would otherwise label the transaction in every portfolio on their
-- network, which the next export would write under each of them.
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
    -- | The records of the portfolios --portfolio leaves out, which
    -- importwallet would not read
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

-- | Show a file of a wallet. Its wallets are merged into the database as
-- importwallet merges them, then that is undone ('undoneAfter'), so that
-- the database keeps nothing. Each wallet and each account shows as
-- importwallet would leave it: created, found, added or left aside, with
-- the reasons importwallet gives. A file that cannot be merged is refused
-- with the refusal of importwallet, and so is a --portfolio the file has
-- nothing of ('checkScopeHeld').
--
-- Merging the wallets in and undoing it gives the very answer
-- importwallet would give, without its rules written twice, once to do
-- and once to tell what it would do. The two could not drift apart.
fileShown :: (MonadUnliftIO m) => Ctx -> ImportScope -> WalletFile -> ExceptT String (DB m) FileShown
fileShown ctx scope file = do
  liftEither $ checkScopeHeld ctx scope (fileKeys file) (snd <$> fileRecords file)
  (wallets, outcomes) <- liftEither =<< lift (undoneAfter (runExceptT (importKeys ctx scope (fileKeys file))))
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
    -- What names each account of the file: its network, and its origin
    -- when the fingerprint of its wallet is known
    names = accountName <$> outcomes
    accountName :: AccountImport -> Maybe (Text, Maybe Origin)
    accountName outcome = do
      ref <- importedAccount outcome
      let place = accountRefPlace ref
          fingerprintM = listToMaybe [fingerprint | wallet <- wallets, walletImportIndex wallet == refWallet (placePortfolio place), Just fingerprint <- [walletImportFingerprint wallet]]
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
    "changeaddresses" .= reportChangeAddresses report,
    "notkept" .= reportNotKept report,
    "unreadable" .= reportUnreadable report
  ]

parseReport :: Json.Object -> Parser ImportReport
parseReport o =
  ImportReport
    <$> o .: "wallets"
    <*> (mapM parseAccountImport =<< o .: "accounts")
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
    <*> o .: "changeaddresses"
    <*> o .: "notkept"
    <*> o .: "unreadable"

instance Json.ToJSON PortfolioImport where
  toJSON p =
    Json.object
      [ "portfolio" .= portfolioImportIndex p,
        "created" .= portfolioImportCreated p,
        "added" .= portfolioImportAdded p,
        "missing" .= portfolioImportMissing p
      ]

instance Json.FromJSON PortfolioImport where
  parseJSON = Json.withObject "portfolio import" $ \o ->
    PortfolioImport <$> o .: "portfolio" <*> o .: "created" <*> o .: "added" <*> o .: "missing"

instance Json.ToJSON WalletImport where
  toJSON w =
    Json.object
      [ "wallet" .= walletImportIndex w,
        "fingerprint" .= walletImportFingerprint w,
        "created" .= walletImportCreated w,
        "merged" .= walletImportMerged w,
        "portfolios" .= walletImportPortfolios w
      ]

instance Json.FromJSON WalletImport where
  parseJSON = Json.withObject "wallet import" $ \o ->
    WalletImport <$> o .: "wallet" <*> o .:? "fingerprint" <*> o .: "created" <*> o .: "merged" <*> o .: "portfolios"

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
