{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The accounts of the wallet and their labels as a BIP 329 file
-- ("Haskoin.Wallet.Bip329"), which exportacc writes and importacc reads.
--
-- An account goes in the file as an @xpub@ record: its key, its name as
-- the label, its origin @pkh([fingerprint/44'/coin'/index'])@ when hw knows
-- the fingerprint of its seed, and its network under hw's own key. Then
-- come its labelled receiving addresses, each with its key path, its
-- labelled transactions, with their height, fee and value when the chain
-- has them, and the coins the user locked, as outputs that are not
-- spendable. Every record of the account carries its origin, so that the
-- accounts of one seed stay apart in a file of several accounts.
--
-- Reading a file, importacc takes the @xpub@ records first: an account the
-- wallet does not hold is created, named after the label, and one it holds
-- learns the fingerprint of its seed, and takes the name of the label when
-- hw wrote the file. Then
-- each other record goes where it belongs. An address is found among the
-- addresses of the wallet, however it is written, or derived from its key
-- path in the account whose key gives it, which then generates its chain
-- up to it. A transaction goes to the accounts its origin names, whether
-- they have the transaction yet or not, since the labels of transactions
-- are kept apart from the transactions; without an origin, to the accounts
-- that have it, or to the only account of the wallet. A coin that is not
-- spendable is locked, as lockcoin locks it. What hw cannot place, the
-- wallet may not have yet: the answer says so, and a sync followed by
-- another import places it. What hw keeps no label for, such as inputs,
-- coins and change addresses, is left aside and counted.
--
-- The document exportacc wrote before this format, one JSON object with
-- the key, the network, the name and the fingerprint, is read as the
-- @xpub@ record it amounts to.
module Haskoin.Wallet.Labels
  ( -- * The file exportacc writes
    accountRecords,

    -- * The file importacc reads
    importFile,
    importRecords,
    ImportReport (..),
    AccountImport (..),
    importedAccount,
    reportPairs,
    parseReport,
    reportNeedsSync,
  )
where

import Conduit (MonadUnliftIO)
import Control.Applicative ((<|>))
import Control.Monad (filterM, foldM, forM, forM_, unless)
import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Types as Json
import Data.Bits (testBit)
import Data.List (partition)
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Config (noNetwork)
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (bestHeight, getConfirmations)
import Haskoin.Wallet.Database.Coin (coinOutpoint, readBlock, setLockedByUser, userLockedCoins)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), findPendingTx, pendingTxs, setPendingTxLabel, txInfoOf)
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..), liveSummary)
import Haskoin.Wallet.Database.TxLabel (accountTxLabels, setTxLabel)
import Haskoin.Wallet.Keys (bip44Deriv, xPubIndex)
import Haskoin.Wallet.Sync (recoveredName)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), isSigned)
import Numeric.Natural (Natural)

{- The file exportacc writes -}

-- | The records of an account: its key, then its labelled receiving
-- addresses, its labelled transactions, on the chain or signed and
-- pending, and the coins the user locked
accountRecords :: (MonadUnliftIO m) => Ctx -> (DBAccountId, DBAccount) -> ExceptT String (DB m) [Record]
accountRecords ctx (accId, acc) = do
  heightM <- bestHeight net
  addrs <- lift $ labelledAddresses accId
  labels <- lift $ accountTxLabels accId
  chainRecords <- forM labels $ \(tid, label) -> do
    liveM <- liveSummary net accId tid
    return
      (withOrigin (record TxRecord (txHashToHex tid)))
        { recordLabel = Just label,
          recordHeight = settledHeight heightM . fst =<< liveM,
          recordFee = summaryFee . snd <$> liveM,
          recordValue = summaryAmount . snd <$> liveM
        }
  pending <- pendingTxs accId
  pendingRecords <- fmap concat . forM pending $ \p -> do
    let tx = txSignDataTx p.pendingTxData
        label = p.pendingTxData.txSignDataLabel
    if isSigned tx && not (Text.null label)
      then do
        info <- txInfoOf ctx (accId, acc) p.pendingTxData
        return
          [ (withOrigin (record TxRecord (txHashToHex (txHash tx))))
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
    rowM <- lift $ P.getBy (UniqueAddress (dBCoinAddress coin))
    return
      (withOrigin (record OutputRecord (outpointText outpoint)))
        { recordSpendable = Just False,
          recordKeypath = keypathText . addrPath . P.entityVal <$> rowM,
          recordHeight = settledHeight heightM block,
          recordValue = Just (fromIntegral (dBCoinValue coin))
        }
  return $ keyRecord : addressRecords addrs <> chainRecords <> pendingRecords <> coinRecords
  where
    net = accountNetwork acc
    originM = originText . (`accountOrigin` accountPath acc) <$> accountFingerprint acc
    withOrigin r = r {recordOrigin = originM}
    keyRecord =
      (withOrigin (record XpubRecord (dBAccountXPubKey acc)))
        { recordLabel = Just (dBAccountName acc),
          recordNetwork = Just (dBAccountNetwork acc)
        }
    addressRecords addrs =
      [ (withOrigin (record AddrRecord (dBAddressAddress addr)))
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

{- The file importacc reads -}

-- | What importacc did with a file
data ImportReport = ImportReport
  { -- | What became of each account the file names, in the order of its
    -- xpub records
    reportAccounts :: ![AccountImport],
    -- | The labels set on addresses
    reportAddressLabels :: !Natural,
    -- | The labels set on transactions, each account counting once
    reportTxLabels :: !Natural,
    reportCoinsLocked :: !Natural,
    reportCoinsUnlocked :: !Natural,
    -- | The addresses the wallet does not have, and could not derive
    reportAddressesMissing :: !Natural,
    -- | The transactions no account of the wallet has, when no origin says
    -- which account they are of
    reportTxsMissing :: !Natural,
    -- | The coins the wallet does not have
    reportCoinsMissing :: !Natural,
    -- | The records whose origin names an account the wallet does not have
    reportOtherAccounts :: !Natural,
    -- | The labels of change addresses, which hw does not keep
    reportChangeAddresses :: !Natural,
    -- | The records hw keeps no label for: inputs, the labels of coins,
    -- public keys it cannot read, and the kinds the BIP does not define
    reportNotKept :: !Natural,
    -- | The lines that are not records, as a text editor numbers them
    reportUnreadable :: ![Natural]
  }
  deriving (Eq, Show)

-- | What became of an account the file names
data AccountImport
  = AccountCreated !DBAccount
  | -- | The wallet had it already. It keeps its name against the file's
    -- for the reason given, if any.
    AccountKept !DBAccount !(Maybe Text)
  | -- | The wallet had it under the former name
    AccountRenamed !Text !DBAccount
  | -- | Not imported: the name the file gives it, or the start of its key,
    -- and why
    AccountRefused !Text !Text
  deriving (Eq, Show)

-- | The account an xpub record left in the wallet, created or found, if any
importedAccount :: AccountImport -> Maybe DBAccount
importedAccount = \case
  AccountCreated acc -> Just acc
  AccountKept acc _ -> Just acc
  AccountRenamed _ acc -> Just acc
  AccountRefused _ _ -> Nothing

-- | Whether the file labels things the wallet does not have yet, which a
-- sync may bring
reportNeedsSync :: ImportReport -> Bool
reportNeedsSync report = reportAddressesMissing report + reportTxsMissing report + reportCoinsMissing report > 0

-- | A report with nothing done yet
emptyReport :: [AccountImport] -> [Natural] -> ImportReport
emptyReport accounts unreadable =
  ImportReport
    { reportAccounts = accounts,
      reportAddressLabels = 0,
      reportTxLabels = 0,
      reportCoinsLocked = 0,
      reportCoinsUnlocked = 0,
      reportAddressesMissing = 0,
      reportTxsMissing = 0,
      reportCoinsMissing = 0,
      reportOtherAccounts = 0,
      reportChangeAddresses = 0,
      reportNotKept = 0,
      reportUnreadable = unreadable
    }

-- | Import the content of a file: a BIP 329 file, or a document of the
-- accounts as exportacc wrote them before. The network is the one of
-- --network or of the configuration, for a key the file places on none.
importFile :: (MonadUnliftIO m) => Ctx -> Maybe Network -> Text -> ExceptT String (DB m) ImportReport
importFile ctx netM content =
  case oldDocument ctx content of
    Just docE -> do
      doc <- liftEither docE
      importRecords ctx netM [(1, documentRecord ctx doc)] []
    Nothing -> do
      let parsed = readLines content
          records = [(number, r) | (number, Right (LineRecord r)) <- parsed]
          otherTypes = [number | (number, Right (LineOfOtherType _)) <- parsed]
          unreadable = [number | (number, Left _) <- parsed]
      report <- importRecords ctx netM records unreadable
      return report {reportNotKept = reportNotKept report + fromIntegral (length otherTypes)}

-- | The document of an earlier exportacc, when the content is one: a JSON
-- object naming the key, or Nothing for anything else
oldDocument :: Ctx -> Text -> Maybe (Either String PubKeyDoc)
oldDocument ctx content = do
  value <- Json.decodeStrict' (encodeUtf8 content)
  case value of
    Json.Object o | isJust (Json.parseMaybe (.: "xpubkey") o :: Maybe Json.Value) ->
      Just (Json.parseEither (unmarshalValue ctx) value)
    _ -> Nothing

-- | The xpub record such a document amounts to
documentRecord :: Ctx -> PubKeyDoc -> Record
documentRecord ctx (PubKeyDoc xpub net name fingerprintM) =
  (record XpubRecord (xPubExport net ctx xpub))
    { recordLabel = Just name,
      recordOrigin = originText . (`accountOrigin` bip44Deriv net (xPubIndex xpub)) <$> fingerprintM,
      recordNetwork = Just (cs net.name)
    }

-- | Import records, each with the number of its line, the xpub records
-- first, and count the lines given as not readable
importRecords :: (MonadUnliftIO m) => Ctx -> Maybe Network -> [(Natural, Record)] -> [Natural] -> ExceptT String (DB m) ImportReport
importRecords ctx netM numbered unreadable = do
  let (keys, others) = partition ((== XpubRecord) . recordType . snd) numbered
  accountImports <- mapM (importKey ctx netM . snd) keys
  accounts <- lift getAccounts
  foldM (importRecord ctx accounts) (emptyReport accountImports unreadable) others

{- Accounts -}

-- | Import the account of an xpub record: create it, or bring the one the
-- wallet holds in line with the record. A record the wallet cannot take
-- says why, and the import goes on.
importKey :: (MonadUnliftIO m) => Ctx -> Maybe Network -> Record -> ExceptT String (DB m) AccountImport
importKey ctx netM r = do
  heldM <- lift $ P.getBy (UniqueXPubKey (recordRef r))
  outcome <- attempt $ case heldM of
    Just (P.Entity heldId held) -> heldAccount heldId held
    Nothing -> newAccount
  return $ either (AccountRefused shownName . cs) id outcome
  where
    originM = parseOrigin =<< recordOrigin r
    fingerprintM = originFingerprint <$> originM
    shownName = fromMaybe (Text.take 12 (recordRef r) <> "...") (recordLabel r)
    networkNamed = forM (recordNetwork r) $ \name ->
      liftMaybe ("Unknown network " <> cs name) (netByName (cs name))
    -- An account the wallet holds: the record must agree with it, and it
    -- learns the fingerprint of the record. It takes the name of the record
    -- when hw wrote it, which the network key tells: another wallet labels
    -- a key with the name of its signer, such as BIP39, not of the account.
    heldAccount heldId held = do
      namedM <- networkNamed
      forM_ namedM $ \named ->
        unless (named.name == cs (dBAccountNetwork held)) $
          throwError $ "The account " <> cs (dBAccountName held) <> " on " <> cs (dBAccountNetwork held) <> " holds this key, which is of no account on " <> named.name
      checkOrigin originM (accountPath held)
      forM_ fingerprintM (learnFingerprint heldId)
      case recordLabel r of
        Just new | new /= dBAccountName held && isJust namedM -> do
          renamed <- attempt (renameAccount (dBAccountName held) new)
          acc <- getAccountById heldId
          return $ case renamed of
            Right _ -> AccountRenamed (dBAccountName held) acc
            Left why -> AccountKept acc (Just (cs why))
        _ -> (`AccountKept` Nothing) <$> getAccountById heldId
    -- An account the wallet does not hold: on the network the record or the
    -- command names, from a BIP44 account key
    newAccount = do
      namedM <- networkNamed
      net <- liftMaybe noNetwork (namedM <|> netM)
      xpub <- liftMaybe ("The key does not read as a key of " <> net.name) (xPubImport net ctx (recordRef r))
      unless (xpub.depth == 3 && testBit xpub.index 31) $
        throwError "The key is not that of a BIP44 account, three hardened steps below the master key"
      checkOrigin originM (bip44Deriv net (xPubIndex xpub))
      let name = fromMaybe (recoveredName net (xPubIndex xpub)) (recordLabel r)
      AccountCreated . snd <$> insertAccount net ctx name xpub fingerprintM

-- | The result of an action that may be refused, as a value
attempt :: (Monad m) => ExceptT String m a -> ExceptT String m (Either String a)
attempt action = (Right <$> action) `catchError` (return . Left)

-- | Refuse an origin that is not that of the BIP44 account at the path: a
-- key of another kind of wallet, such as a BIP84 one, derives other
-- addresses. A record without an origin, or with one hw cannot read, is
-- not checked.
checkOrigin :: (Monad m) => Maybe Origin -> HardPath -> ExceptT String m ()
checkOrigin originM path = forM_ originM $ \origin ->
  unless (originScripts origin == ["pkh"] && originPath origin == toGeneric path) $
    throwError $ "The key is not that of the BIP44 account " <> pathToStr path <> ": its origin is " <> cs (originText origin)

-- | The accounts of the wallet an origin names: those at its path whose
-- seed it names, or whose seed hw does not know yet. An origin hw cannot
-- read as that of a BIP44 account names none.
accountsOfOrigin :: [(DBAccountId, DBAccount)] -> Text -> [(DBAccountId, DBAccount)]
accountsOfOrigin accounts text =
  case parseOrigin text of
    Just origin
      | originScripts origin == ["pkh"] ->
          [ held
            | held@(_, acc) <- accounts,
              toGeneric (accountPath acc) == originPath origin,
              maybe True (== originFingerprint origin) (accountFingerprint acc)
          ]
    _ -> []

{- Labels, locks and what is left aside -}

-- | Import a record of anything but a key, and count what was done with it
importRecord :: (MonadUnliftIO m) => Ctx -> [(DBAccountId, DBAccount)] -> ImportReport -> (Natural, Record) -> ExceptT String (DB m) ImportReport
importRecord ctx accounts report (number, r) =
  case recordType r of
    AddrRecord -> labelAddressRecord ctx accounts report r (`textToAddr` recordRef r)
    PubkeyRecord ->
      case publicKeyAddress ctx (recordRef r) of
        Just addr -> labelAddressRecord ctx accounts report r (const (Just addr))
        Nothing -> return (leftAside report)
    TxRecord ->
      case hexToTxHash (recordRef r) of
        Just tid -> labelTxRecord accounts report r tid
        Nothing -> return (unreadable report)
    OutputRecord ->
      case readOutpoint (recordRef r) of
        Just outpoint -> lockOutputRecord report r outpoint
        Nothing -> return (unreadable report)
    InputRecord -> return (leftAside report)
    SpscanRecord -> return (leftAside report)
    XpubRecord -> return (leftAside report)
  where
    leftAside rep = rep {reportNotKept = reportNotKept rep + 1}
    unreadable rep = rep {reportUnreadable = reportUnreadable rep <> [number]}

-- | The P2PKH address of a public key written in hexadecimal, compressed or
-- not. None for a key of another kind, such as an x-only key.
publicKeyAddress :: Ctx -> Text -> Maybe Address
publicKeyAddress ctx hex = do
  bytes <- decodeHex hex
  key <- either (const Nothing) Just (unmarshal ctx bytes) :: Maybe PublicKey
  return (pubKeyAddr ctx key)

-- | Label the address of a record, read on the network of each account as
-- the function gives it: see 'placeAddress'
labelAddressRecord :: (MonadUnliftIO m) => Ctx -> [(DBAccountId, DBAccount)] -> ImportReport -> Record -> (Network -> Maybe Address) -> ExceptT String (DB m) ImportReport
labelAddressRecord ctx accounts report r readOn =
  case recordLabel r of
    Nothing -> return report
    Just label -> do
      placed <- placeAddress ctx (candidates accounts) readOn (parseKeypath =<< recordKeypath r)
      case placed of
        PlacedAt accId addr
          | dBAddressInternal addr -> return report {reportChangeAddresses = reportChangeAddresses report + 1}
          | otherwise -> do
              _ <- setAddrLabel accId (dBAddressIndex addr) label
              return report {reportAddressLabels = reportAddressLabels report + 1}
        PlacedOnChangeChain -> return report {reportChangeAddresses = reportChangeAddresses report + 1}
        NotPlaced -> return report {reportAddressesMissing = reportAddressesMissing report + 1}
  where
    -- The accounts the origin names, or every account without one. An
    -- address is of one account at most, so an origin only narrows the
    -- search.
    candidates held = case recordOrigin r of
      Just origin | named@(_ : _) <- accountsOfOrigin held origin -> named
      _ -> held

-- | Where the address of a record is in the wallet
data Placement
  = -- | The row of an account has it
    PlacedAt !DBAccountId !DBAddress
  | -- | Its key path derives it on the change chain of an account, where
    -- it takes no label and is not generated
    PlacedOnChangeChain
  | -- | Nowhere hw can tell
    NotPlaced

-- | Find the address of a record among the addresses of the accounts, read
-- on the network of each; or derive it from its key path in the account
-- whose key gives it, which then generates its receiving chain up to it,
-- as the wallet that wrote the file had it
placeAddress :: (MonadUnliftIO m) => Ctx -> [(DBAccountId, DBAccount)] -> (Network -> Maybe Address) -> Maybe SoftPath -> ExceptT String (DB m) Placement
placeAddress ctx accounts readOn keypathM = do
  heldM <- firstJust accounts $ \held@(accId, acc) -> do
    rows <- lift $ storedAmong (maybeToList (readOn (accountNetwork acc))) held
    return $ PlacedAt accId <$> listToMaybe rows
  case (heldM, chainIndex =<< keypathM) of
    (Just placed, _) -> return placed
    (Nothing, Nothing) -> return NotPlaced
    (Nothing, Just (addrType, index)) -> fromMaybe NotPlaced <$> firstJust accounts (derived addrType index)
  where
    derived addrType index (accId, acc) =
      case readOn (accountNetwork acc) of
        Nothing -> return Nothing
        Just addr -> do
          derivedM <- liftIO $ deriveChainAddress ctx (accountXPubKey ctx acc) (addrDeriv addrType) index
          if derivedM /= Just addr
            then return Nothing
            else case addrType of
              AddrInternal -> return (Just PlacedOnChangeChain)
              AddrExternal -> do
                extendChainTo ctx accId AddrExternal (fromIntegral index + 1)
                rowM <- lift $ P.get (DBAddressKey accId False (fromIntegral index))
                return $ PlacedAt accId <$> rowM

-- | The first result of the action on the items, if any
firstJust :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
firstJust [] _ = return Nothing
firstJust (x : xs) f = do
  found <- f x
  maybe (firstJust xs f) (return . Just) found

-- | Label the transaction of a record, in the accounts its origin names, or
-- in those that have it: a pending transaction in the account that holds
-- it, a transaction of the network in its labels, which need not have the
-- transaction yet
labelTxRecord :: (MonadUnliftIO m) => [(DBAccountId, DBAccount)] -> ImportReport -> Record -> TxHash -> ExceptT String (DB m) ImportReport
labelTxRecord accounts report r tid =
  case recordLabel r of
    Nothing -> return report
    Just label -> do
      pendingM <- findPendingTx tid
      case pendingM of
        Just held | maybe True (any ((== held.pendingTxAccount) . fst)) named -> do
          lift $ setPendingTxLabel held label
          return (labelled 1)
        _ -> case named of
          Just [] -> return report {reportOtherAccounts = reportOtherAccounts report + 1}
          Just ofOrigin -> labelIn ofOrigin label
          Nothing -> do
            holders <- filterM (\(accId, _) -> lift (isJust <$> P.get (DBTxInfoKey accId (hashBytes tid)))) accounts
            case (holders, accounts) of
              ([], [only]) -> labelIn [only] label
              ([], _) -> return report {reportTxsMissing = reportTxsMissing report + 1}
              _ -> labelIn holders label
  where
    -- The accounts the origin names, or Nothing without an origin
    named = accountsOfOrigin accounts <$> recordOrigin r
    labelled n = report {reportTxLabels = reportTxLabels report + n}
    labelIn held label = do
      forM_ held $ \(accId, _) -> lift (setTxLabel accId tid label)
      return (labelled (fromIntegral (length held)))

-- | Lock or unlock the coin of a record as its spendable flag says, when
-- the wallet has the coin. A record with a label alone is left aside: hw
-- keeps no label on a coin.
lockOutputRecord :: (MonadUnliftIO m) => ImportReport -> Record -> OutPoint -> ExceptT String (DB m) ImportReport
lockOutputRecord report r outpoint = do
  rowM <- lift $ P.get (DBCoinKey (outpointBytes outpoint))
  case (rowM, recordSpendable r) of
    (Nothing, _) -> return report {reportCoinsMissing = reportCoinsMissing report + 1}
    (Just _, Nothing) -> return report {reportNotKept = reportNotKept report + 1}
    (Just _, Just spendable) -> do
      setLockedByUser outpoint (not spendable)
      return $
        if spendable
          then report {reportCoinsUnlocked = reportCoinsUnlocked report + 1}
          else report {reportCoinsLocked = reportCoinsLocked report + 1}

{- The report in JSON -}

-- | The keys of a report, as the --json answer of importacc writes them
reportPairs :: ImportReport -> [Pair]
reportPairs report =
  [ "accounts" .= (accountImportJson <$> reportAccounts report),
    "addresslabels" .= reportAddressLabels report,
    "txlabels" .= reportTxLabels report,
    "coinslocked" .= reportCoinsLocked report,
    "coinsunlocked" .= reportCoinsUnlocked report,
    "addressesmissing" .= reportAddressesMissing report,
    "txsmissing" .= reportTxsMissing report,
    "coinsmissing" .= reportCoinsMissing report,
    "otheraccounts" .= reportOtherAccounts report,
    "changeaddresses" .= reportChangeAddresses report,
    "notkept" .= reportNotKept report,
    "unreadable" .= reportUnreadable report
  ]

parseReport :: Json.Object -> Parser ImportReport
parseReport o =
  ImportReport
    <$> (mapM parseAccountImport =<< o .: "accounts")
    <*> o .: "addresslabels"
    <*> o .: "txlabels"
    <*> o .: "coinslocked"
    <*> o .: "coinsunlocked"
    <*> o .: "addressesmissing"
    <*> o .: "txsmissing"
    <*> o .: "coinsmissing"
    <*> o .: "otheraccounts"
    <*> o .: "changeaddresses"
    <*> o .: "notkept"
    <*> o .: "unreadable"

-- | What became of an account in JSON: its status, the account when the
-- wallet has it, the former name of a renamed one, the note of one that
-- kept its name, and the name and reason of one refused
accountImportJson :: AccountImport -> Json.Value
accountImportJson = \case
  AccountCreated acc -> Json.object ["status" .= ("created" :: Text), "account" .= acc]
  AccountKept acc noteM -> Json.object (["status" .= ("kept" :: Text), "account" .= acc] <> ["note" .= note | Just note <- [noteM]])
  AccountRenamed was acc -> Json.object ["status" .= ("renamed" :: Text), "was" .= was, "account" .= acc]
  AccountRefused name why -> Json.object ["status" .= ("refused" :: Text), "name" .= name, "reason" .= why]

parseAccountImport :: Json.Value -> Parser AccountImport
parseAccountImport = Json.withObject "account import" $ \o -> do
  status <- o .: "status"
  case status :: Text of
    "created" -> AccountCreated <$> o .: "account"
    "kept" -> AccountKept <$> o .: "account" <*> o .:? "note"
    "renamed" -> AccountRenamed <$> o .: "was" <*> o .: "account"
    "refused" -> AccountRefused <$> o .: "name" <*> o .: "reason"
    _ -> fail "Unknown account import status"
