{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The accounts of the wallet and their labels as a BIP 329 file
-- ("Haskoin.Wallet.Bip329"), which exportacc writes and importacc reads.
--
-- An account goes in the file as an @xpub@ record: its key, its name as
-- the label, its origin @pkh([fingerprint/44'/coin'/index'])@, or
-- @wpkh([fingerprint/84'/coin'/index'])@ for a BIP84 account, when hw
-- knows the fingerprint of its seed, and its network and its purpose, 44
-- or 84, under hw's own keys. Then
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
-- A line of the file that is not a record may hold the output descriptor
-- of an account ("Haskoin.Wallet.Descriptor"), as another wallet exports
-- one, or a key alone, and the whole file may be the one Bitcoin Core's
-- listdescriptors writes, or the generic JSON of a Coldcard. A descriptor
-- is read as the @xpub@ record it amounts to, with the origin of its key
-- and no name, so the account it adds is named after its network, its
-- index and its kind, as recover names one. The kind of a new account is
-- what its record says of it: the script function of its origin, pkh or
-- wpkh, the purpose hw wrote, or the prefix of SLIP-132 of its key, zpub
-- or vpub for a BIP84 account; a key that says nothing, a bare xpub, is of
-- the kind the command names, BIP44 without one. hw holds BIP44 and BIP84
-- accounts: a descriptor of another kind, such as sh(wpkh()), is refused,
-- with the reason, and the import goes on.
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
import Control.Monad (filterM, foldM, forM, forM_, unless, when)
import Control.Monad.Except (ExceptT, catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Types as Json
import Data.Bits (testBit)
import Data.List (nub, nubBy)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, maybeToList)
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
import Haskoin.Wallet.Descriptor
import Haskoin.Wallet.Keys (AccountKind (..), accountDeriv, checkKindOnNetwork, kindName, kindOfPurpose, kindPurpose, xPubIndex)
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
    kind = accountKind acc
    originM = originText . (\fingerprint -> accountOrigin kind fingerprint (accountPath acc)) <$> accountFingerprint acc
    withOrigin r = r {recordOrigin = originM}
    keyRecord =
      (withOrigin (record XpubRecord (dBAccountXPubKey acc)))
        { recordLabel = Just (dBAccountName acc),
          recordNetwork = Just (dBAccountNetwork acc),
          recordPurpose = Just (kindPurpose kind)
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
  = -- | Created, with the note on what the import took on trust, if
    -- anything: the kind of a key that says none ('trustNote')
    AccountCreated !DBAccount !(Maybe Text)
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
  AccountCreated acc _ -> Just acc
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

-- | Import the content of a file: a BIP 329 file, whose lines may hold
-- descriptors of accounts too, a file of descriptors as another wallet
-- writes it, or a document of the accounts as exportacc wrote them
-- before. The network is the one of --network or of the configuration,
-- for a key the file places on none, and the kind is the one of --type,
-- for a key the file says no kind of.
importFile :: (MonadUnliftIO m) => Ctx -> Maybe Network -> Maybe AccountKind -> Text -> ExceptT String (DB m) ImportReport
importFile ctx netM kindM content =
  case wholeObject content of
    Just o
      | isJust (Json.parseMaybe (.: "xpubkey") o :: Maybe Json.Value) -> do
          doc <- liftEither (Json.parseEither (unmarshalValue ctx) (Json.Object o))
          importRecords ctx netM kindM [Right (documentRecord ctx doc)] [] []
      | Just descriptors <- fileDescriptors o ->
          importRecords ctx netM kindM (oneOfEachKey (descriptorRecord <$> descriptors)) [] []
    _ -> do
      let classified = classifyLine <$> readLines content
          keys = oneOfEachKey [key | KeyLine key <- classified]
          records = [(number, r) | RecordLine number r <- classified]
          otherTypes = length [() | OtherTypeLine <- classified]
          unreadable = [number | UnreadableLine number <- classified]
      report <- importRecords ctx netM kindM keys records unreadable
      return report {reportNotKept = reportNotKept report + fromIntegral otherTypes}

-- | The one JSON object a file holds, when it is one: the document of an
-- earlier exportacc, or a file of descriptors. A file of one record is one
-- object too, which its keys tell apart.
wholeObject :: Text -> Maybe Json.Object
wholeObject content = case Json.decodeStrict' (encodeUtf8 content) of
  Just (Json.Object o) -> Just o
  _ -> Nothing

-- | What a line of a file holds, for the import
data FileLine
  = -- | An xpub record, or a descriptor read as one, or a descriptor
    -- refused with the reason
    KeyLine !(Either AccountImport Record)
  | -- | A record of another type, with the number of its line
    RecordLine !Natural !Record
  | -- | A record of a type the BIP does not define
    OtherTypeLine
  | -- | A line that is neither a record nor a descriptor, by its number
    UnreadableLine !Natural

-- | A line as a record, when it starts as a JSON object does, else as a
-- descriptor. A line that is neither is not readable, unless it looks
-- like a descriptor, in which case it is refused with the reason.
classifyLine :: (Natural, Text) -> FileLine
classifyLine (number, line)
  | "{" `Text.isPrefixOf` line = case parseLine line of
      Right (LineRecord r) | recordType r == XpubRecord -> KeyLine (Right r)
      Right (LineRecord r) -> RecordLine number r
      Right (LineOfOtherType _) -> OtherTypeLine
      Left _ -> UnreadableLine number
  | otherwise = case descriptorRecord line of
      Right r -> KeyLine (Right r)
      Left refused
        | looksLikeDescriptor line -> KeyLine (Left refused)
        | otherwise -> UnreadableLine number

-- | The xpub record a descriptor amounts to: its key, with the origin of
-- the key when the descriptor gives it, and neither name nor network. A
-- text that is not the descriptor of an account is refused, named by its
-- start, with the reason.
descriptorRecord :: Text -> Either AccountImport Record
descriptorRecord text = case parseDescriptor text of
  Right d -> Right (record XpubRecord (descriptorKey d)) {recordOrigin = originText <$> descriptorOrigin d}
  Left why -> Left (AccountRefused (Text.take 12 text <> "...") (cs why))

-- | The key lines with each key once: the file of Bitcoin Core's
-- listdescriptors writes an account as two descriptors of its key, one
-- per chain
oneOfEachKey :: [Either AccountImport Record] -> [Either AccountImport Record]
oneOfEachKey = nubBy sameKey
  where
    sameKey (Right a) (Right b) = recordRef a == recordRef b
    sameKey _ _ = False

-- | The xpub record such a document amounts to: the hw that wrote it had
-- BIP44 accounts alone
documentRecord :: Ctx -> PubKeyDoc -> Record
documentRecord ctx (PubKeyDoc xpub net name fingerprintM) =
  (record XpubRecord (xPubExport net ctx xpub))
    { recordLabel = Just name,
      recordOrigin = originText . (\fingerprint -> accountOrigin Bip44 fingerprint (accountDeriv Bip44 net (xPubIndex xpub))) <$> fingerprintM,
      recordNetwork = Just (cs net.name),
      recordPurpose = Just (kindPurpose Bip44)
    }

-- | Import the accounts of the key lines, the refused ones as they are,
-- then the other records, each with the number of its line, and count
-- the lines given as not readable
importRecords :: (MonadUnliftIO m) => Ctx -> Maybe Network -> Maybe AccountKind -> [Either AccountImport Record] -> [(Natural, Record)] -> [Natural] -> ExceptT String (DB m) ImportReport
importRecords ctx netM kindM keys others unreadable = do
  accountImports <- mapM (either return (importKey ctx netM kindM)) keys
  accounts <- lift getAccounts
  foldM (importRecord ctx accounts) (emptyReport accountImports unreadable) others

{- Accounts -}

-- | Import the account of an xpub record: create it, or bring the one the
-- wallet holds in line with the record. A record the wallet cannot take
-- says why, and the import goes on. The key of the record is read first,
-- as BIP32 writes it: a zpub or a vpub names the same key as its xpub, and
-- says its account is a BIP84 one ('readAccountKey'), which --type may not
-- contradict.
importKey :: (MonadUnliftIO m) => Ctx -> Maybe Network -> Maybe AccountKind -> Record -> ExceptT String (DB m) AccountImport
importKey ctx netM kindM r = do
  outcome <- attempt $ do
    (prefixKindM, keyText) <- liftEither (readAccountKey (recordRef r))
    -- The kind --type names must be the one the prefix of the key marks,
    -- when it marks one: a zpub with --type bip44 is a contradiction, and
    -- the import takes neither side
    forM_ ((,) <$> prefixKindM <*> kindM) $ \(marked, typed) ->
      unless (marked == typed) $ throwError (typedKindClash marked typed)
    heldM <- lift $ P.getBy (UniqueXPubKey keyText)
    case heldM of
      Just (P.Entity heldId held) -> heldAccount prefixKindM heldId held
      Nothing -> newAccount prefixKindM keyText
  return $ either (AccountRefused shownName . cs) id outcome
  where
    originM = parseOrigin =<< recordOrigin r
    fingerprintM = originFingerprint <$> originM
    shownName = fromMaybe (Text.take 12 (recordRef r) <> "...") (recordLabel r)
    networkNamed = forM (recordNetwork r) $ \name ->
      liftMaybe ("Unknown network " <> cs name) (netByName (cs name))
    -- Why a key whose prefix marks one kind is refused with --type of the
    -- other
    typedKindClash :: AccountKind -> AccountKind -> String
    typedKindClash marked typed =
      "The key is a " <> cs (Text.take 4 (recordRef r)) <> ", which marks a " <> cs (kindName marked) <> " account in SLIP-132, and --type says " <> cs (kindName typed) <> ". Leave --type out, or write the key as an xpub or a tpub if it is of a " <> cs (kindName typed) <> " account after all."
    -- The kind of account the record says its key is of: by the script
    -- function of its origin, the purpose hw wrote, or the prefix of the
    -- key, which must agree; or none, for a key that says nothing
    recordKind prefixKindM = do
      purposeKindM <- forM (recordPurpose r) $ \purpose ->
        liftMaybe ("The purpose " <> show purpose <> " is of no kind of account hw holds: " <> cs (Text.intercalate " or " (kindName <$> [minBound .. maxBound]))) (kindOfPurpose purpose)
      case nub (catMaybes [kindOfScripts . originScripts =<< originM, purposeKindM, prefixKindM]) of
        [] -> return Nothing
        [kind] -> return (Just kind)
        kinds -> throwError $ "The record says its key is of accounts of two kinds, " <> cs (Text.intercalate " and " (kindName <$> kinds)) <> ": its origin, its purpose and the prefix of its key do not agree"
    -- An account the wallet holds: the record must agree with it, and it
    -- learns the fingerprint of the record. It takes the name of the record
    -- when hw wrote it, which the network key tells: another wallet labels
    -- a key with the name of its signer, such as BIP39, not of the account.
    heldAccount prefixKindM heldId held = do
      namedM <- networkNamed
      forM_ namedM $ \named ->
        unless (named.name == cs (dBAccountNetwork held)) $
          throwError $ "The account " <> cs (dBAccountName held) <> " on " <> cs (dBAccountNetwork held) <> " holds this key, which is of no account on " <> named.name
      saidKindM <- recordKind prefixKindM
      forM_ saidKindM $ \said ->
        unless (said == accountKind held) $
          throwError $ "The account " <> cs (dBAccountName held) <> " holds this key as a " <> cs (kindName (accountKind held)) <> " account, and the record says it is of a " <> cs (kindName said) <> " one"
      checkOrigin (accountKind held) originM (accountPath held)
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
    -- command names, of the kind the record says, or the command names, or
    -- BIP44, from an account key
    newAccount prefixKindM keyText = do
      when (isPrivateKey keyText) $
        throwError "The key is a private key. hw takes the public key of an account, and no file of accounts should hold a private one"
      namedM <- networkNamed
      net <- liftMaybe noNetwork (namedM <|> netM)
      xpub <- liftMaybe ("The key does not read as a key of " <> net.name) (xPubImport net ctx keyText)
      unless (xpub.depth == 3 && testBit xpub.index 31) $
        throwError "The key is not that of an account, three hardened steps below the master key"
      saidKindM <- recordKind prefixKindM
      let kind = fromMaybe (fromMaybe Bip44 kindM) saidKindM
          -- The kind of a key that says none is taken on trust
          note = case saidKindM of
            Nothing -> Just (trustNote kind kindM)
            Just _ -> Nothing
      liftEither $ checkKindOnNetwork net kind
      checkOrigin kind originM (accountDeriv kind net (xPubIndex xpub))
      let name = fromMaybe (recoveredName kind net (xPubIndex xpub)) (recordLabel r)
      (_, acc) <- insertAccount kind net ctx name xpub fingerprintM
      return (AccountCreated acc note)

-- | The note on an account made of a key that says no kind, an xpub or a
-- tpub alone, which says nothing of its path: the kind the import took on
-- trust, from --type or by default, and what tells when it was the wrong
-- one. Ledger Live writes an xpub for an account of any kind.
trustNote :: AccountKind -> Maybe AccountKind -> Text
trustNote kind typedM =
  Text.unwords
    [ "The key says no kind. The account is " <> kindName kind <> taken,
      "If the key is of a " <> kindName other <> " account" <> example <> ", a sync finds none of its transactions and signtx refuses the mnemonic.",
      "Then deleteacc, and import the key again with --type " <> kindName other <> "."
    ]
  where
    taken = if isJust typedM then " because --type says so." else " by default."
    -- The other kind hw holds
    other = case kind of
      Bip44 -> Bip84
      Bip84 -> Bip44
    example = if other == Bip84 then ", as Ledger Live writes one" else ""

-- | The result of an action that may be refused, as a value
attempt :: (Monad m) => ExceptT String m a -> ExceptT String m (Either String a)
attempt action = (Right <$> action) `catchError` (return . Left)

-- | Refuse an origin that is not that of the account of the kind at the
-- path: a key of another kind of wallet, such as a BIP49 one, derives
-- other addresses. A record without an origin, or with one hw cannot
-- read, is not checked.
checkOrigin :: (Monad m) => AccountKind -> Maybe Origin -> HardPath -> ExceptT String m ()
checkOrigin kind originM path = forM_ originM $ \origin ->
  unless (originScripts origin == kindScripts kind && originPath origin == toGeneric path) $
    throwError $ "The key is not that of the " <> cs (kindName kind) <> " account " <> pathToStr path <> ": its origin is " <> cs (originText origin)

-- | The accounts of the wallet an origin names: those of its kind at its
-- path whose seed it names, or whose seed hw does not know yet. An origin
-- hw cannot read as that of an account of a kind it holds names none.
accountsOfOrigin :: [(DBAccountId, DBAccount)] -> Text -> [(DBAccountId, DBAccount)]
accountsOfOrigin accounts text =
  case parseOrigin text of
    Just origin
      | Just kind <- kindOfScripts (originScripts origin) ->
          [ held
            | held@(_, acc) <- accounts,
              accountKind acc == kind,
              toGeneric (accountPath acc) == originPath origin,
              maybe True (== originFingerprint origin) (accountFingerprint acc)
          ]
    _ -> []

{- Labels, locks and what is left aside -}

-- | Import a record of anything but a key, and count what was done with it
importRecord :: (MonadUnliftIO m) => Ctx -> [(DBAccountId, DBAccount)] -> ImportReport -> (Natural, Record) -> ExceptT String (DB m) ImportReport
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
        Just outpoint -> lockOutputRecord report r outpoint
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
labelAddressRecord :: (MonadUnliftIO m) => Ctx -> [(DBAccountId, DBAccount)] -> ImportReport -> Record -> (DBAccount -> Maybe Address) -> ExceptT String (DB m) ImportReport
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
-- in each, on its network and of its kind; or derive it from its key path
-- in the account whose key gives it, which then generates its receiving
-- chain up to it, as the wallet that wrote the file had it
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
-- wallet has it, the former name of a renamed one, the note of a created
-- one on what was taken on trust or of one that kept its name, and the
-- name and reason of one refused
accountImportJson :: AccountImport -> Json.Value
accountImportJson = \case
  AccountCreated acc noteM -> Json.object (["status" .= ("created" :: Text), "account" .= acc] <> ["note" .= note | Just note <- [noteM]])
  AccountKept acc noteM -> Json.object (["status" .= ("kept" :: Text), "account" .= acc] <> ["note" .= note | Just note <- [noteM]])
  AccountRenamed was acc -> Json.object ["status" .= ("renamed" :: Text), "was" .= was, "account" .= acc]
  AccountRefused name why -> Json.object ["status" .= ("refused" :: Text), "name" .= name, "reason" .= why]

parseAccountImport :: Json.Value -> Parser AccountImport
parseAccountImport = Json.withObject "account import" $ \o -> do
  status <- o .: "status"
  case status :: Text of
    "created" -> AccountCreated <$> o .: "account" <*> o .:? "note"
    "kept" -> AccountKept <$> o .: "account" <*> o .:? "note"
    "renamed" -> AccountRenamed <$> o .: "was" <*> o .: "account"
    "refused" -> AccountRefused <$> o .: "name" <*> o .: "reason"
    _ -> fail "Unknown account import status"
