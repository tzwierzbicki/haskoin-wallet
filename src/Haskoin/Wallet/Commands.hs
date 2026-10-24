{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Haskoin.Wallet.Commands where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO(..), MonadTrans(lift))
import Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Json
import qualified Data.ByteString.Base64 as B64
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Database.Persist.Sqlite (transactionUndo)
import Haskoin
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Amounts
import Haskoin.Wallet.Backup (backupDatabase, backupDirectory, copyDatabase)
import Haskoin.Wallet.Coin
import Haskoin.Wallet.CoinSelection (CoinSelection, FeePayer)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Entropy
import Haskoin.Wallet.FileIO
import Haskoin.Wallet.Keys
import Haskoin.Wallet.MessageSigning
import Haskoin.Wallet.Migration (databaseVersion, latestDatabaseVersion)
import Haskoin.Wallet.Parser
import Haskoin.Wallet.PendingTx
import Haskoin.Wallet.Prompt
import Haskoin.Wallet.Store
import Haskoin.Wallet.Sync
import Haskoin.Wallet.TxBuilding (buildTxSignData)
import Haskoin.Wallet.TxInfo
import Haskoin.Wallet.TxSignData
import Haskoin.Wallet.TxSigning (signWalletTx)
import Haskoin.Wallet.TxSweep (buildSweepSignData, fetchSweepCoins, signTxWithKeys)
import Haskoin.Wallet.Util
import Numeric.Natural (Natural)
import qualified Paths_haskoin_wallet as Paths
import qualified System.Directory as D
import System.IO (hPutStrLn, stderr)
import System.Random (initStdGen)

{- Responses -}

data Response
  = ResponseError
      { responseError :: !Text
      }
  | ResponseMnemonic
      { responseEntropySource :: !Text,
        responseMnemonic :: ![Text],
        responseSplitMnemonic :: ![[Text]]
      }
  | ResponseAccount
      { responseAccount :: !DBAccount
      }
  | ResponseAccResult
      { responseAccount :: !DBAccount,
        responseResult :: !Bool,
        responseText :: !Text
      }
  | ResponseFile
      { responseTxFile :: !FilePath
      }
  | ResponseAccounts
      { responseAccounts :: ![DBAccount]
      }
  | ResponseAddress
      { responseAccount :: !DBAccount,
        responseAddress :: !DBAddress
      }
  | ResponseAddresses
      { responseAccount :: !DBAccount,
        responseAddresses :: ![DBAddress]
      }
  | ResponseTxs
      { responseAccount :: !DBAccount,
        responseTxs :: ![TxInfo]
      }
  | ResponseTx
      { responseAccount :: !DBAccount,
        responsePendingTx :: !TxInfo
      }
  | -- | A transaction sent from a file, on its network
    ResponseSentTx
      { responseNetwork :: !Network,
        responseTxHash :: !TxHash
      }
  | ResponseDeleteTx
      { responseNoSigHash :: !TxHash,
        responseFreedCoins :: !Natural,
        responseFreedAddrs :: !Natural
      }
  | ResponseCoins
      { responseAccount :: !DBAccount,
        responseCoins :: ![JsonCoin]
      }
  | ResponseSignMsg
      { responseMessage :: !Text,
        responseSig :: !Text,
        responseAddr :: !Text
      }
  | ResponseSync {responseSync :: [SyncRes]}
  | ResponseVersion
      { responseVersion :: !Text,
        responseDBVersion :: !Text
      }
  | ResponseRollDice
      { responseRollDice :: ![Natural],
        responseEntropySource :: !Text
      }
  deriving (Eq, Show)

jsonError :: String -> Json.Value
jsonError err = object ["type" .= Json.String "error", "error" .= err]

instance Json.ToJSON Response where
  toJSON =
    \case
      ResponseError err -> jsonError $ cs err
      ResponseMnemonic e w ws ->
        object
          [ "type" .= Json.String "mnemonic",
            "entropysource" .= e,
            "mnemonic" .= w,
            "splitmnemonic" .= ws
          ]
      ResponseAccount a ->
        object
          [ "type" .= Json.String "account",
            "account" .= a
          ]
      ResponseAccResult a b t ->
        object
          [ "type" .= Json.String "accresult",
            "account" .= a,
            "result" .= b,
            "text" .= t
          ]
      ResponseFile f ->
        object
          [ "type" .= Json.String "file",
            "file" .= f
          ]
      ResponseAccounts as ->
        object
          [ "type" .= Json.String "accounts",
            "accounts" .= as
          ]
      ResponseAddress a addr ->
        object
          [ "type" .= Json.String "address",
            "account" .= a,
            "address" .= addr
          ]
      ResponseAddresses a adrs ->
        object
          [ "type" .= Json.String "addresses",
            "account" .= a,
            "addresses" .= adrs
          ]
      ResponseTxs a txs ->
        object
          [ "type" .= Json.String "txs",
            "account" .= a,
            "txs" .= (marshalValue (accountNetwork a) <$> txs)
          ]
      ResponseTx a t -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "tx",
            "account" .= a,
            "tx" .= marshalValue net t
          ]
      ResponseSentTx net h ->
        object
          [ "type" .= Json.String "senttx",
            "network" .= net.name,
            "txid" .= h
          ]
      ResponseDeleteTx h c a ->
        object
          [ "type" .= Json.String "deletetx",
            "nosighash" .= h,
            "freedcoins" .= c,
            "freedaddrs" .= a
          ]
      ResponseCoins a coins -> do
        let net = accountNetwork a
        object
          [ "type" .= Json.String "coins",
            "account" .= a,
            "coins" .= (marshalValue net <$> coins)
          ]
      ResponseSignMsg m s a -> do
        object
          [ "type" .= Json.String "signmsg",
            "message" .= m,
            "signature" .= s,
            "address" .= a
          ]
      ResponseSync xs -> do
        let f (SyncRes as bb bh tc cc rb ck na) =
              object
                [ "account" .= as,
                  "bestblock" .= bb,
                  "bestheight" .= bh,
                  "txupdates" .= tc,
                  "coinupdates" .= cc,
                  "rebroadcast" .= rb,
                  "conflicted" .= ck,
                  "discovered" .= na
                ]
        object
          [ "type" .= Json.String "sync",
            "syncres" .= (f <$> xs)
          ]
      ResponseVersion v dbv ->
        object
          [ "type" .= Json.String "version",
            "version" .= v,
            "dbversion" .= dbv
          ]
      ResponseRollDice ns e ->
        object
          ["type" .= Json.String "rolldice", "entropysource" .= e, "dice" .= ns]

instance Json.FromJSON Response where
  parseJSON =
    Json.withObject "response" $ \o -> do
      Json.String resType <- o .: "type"
      case resType of
        "error" -> ResponseError <$> o .: "error"
        "mnemonic" ->
          ResponseMnemonic
            <$> o .: "entropysource"
            <*> o .: "mnemonic"
            <*> o .: "splitmnemonic"
        "account" ->
          ResponseAccount
            <$> o .: "account"
        "accresult" ->
          ResponseAccResult
            <$> o .: "account"
            <*> o .: "result"
            <*> o .: "text"
        "file" ->
          ResponseFile
            <$> o .: "file"
        "accounts" ->
          ResponseAccounts
            <$> o .: "accounts"
        "address" ->
          ResponseAddress
            <$> o .: "account"
            <*> o .: "address"
        "addresses" ->
          ResponseAddresses
            <$> o .: "account"
            <*> o .: "addresses"
        "txs" -> do
          a <- o .: "account"
          let net = accountNetwork a
          txs <- mapM (unmarshalValue net) =<< o .: "txs"
          return $ ResponseTxs a txs
        "tx" -> do
          a <- o .: "account"
          let net = accountNetwork a
          t <- unmarshalValue net =<< o .: "tx"
          return $ ResponseTx a t
        "senttx" -> do
          net <- maybe (fail "Unknown network") return . netByName =<< o .: "network"
          ResponseSentTx net <$> o .: "txid"
        "deletetx" ->
          ResponseDeleteTx
            <$> o .: "nosighash"
            <*> o .: "freedcoins"
            <*> o .: "freedaddrs"
        "coins" -> do
          a <- o .: "account"
          xs <- o .: "coins"
          coins <- mapM (unmarshalValue (accountNetwork a)) xs
          return $ ResponseCoins a coins
        "signmsg" -> do
          m <- o .: "message"
          s <- o .: "signature"
          a <- o .: "address"
          return $ ResponseSignMsg m s a
        "sync" -> do
          xs <- o .: "syncres"
          res <- forM xs $ \x ->
            SyncRes
              <$> x .: "account"
              <*> x .: "bestblock"
              <*> x .: "bestheight"
              <*> x .: "txupdates"
              <*> x .: "coinupdates"
              <*> x .: "rebroadcast"
              <*> x .: "conflicted"
              <*> x .: "discovered"
          return $ ResponseSync res
        "version" ->
          ResponseVersion
            <$> o .: "version"
            <*> o .: "dbversion"
        "rolldice" ->
          ResponseRollDice
            <$> o .: "dice"
            <*> o .: "entropysource"
        _ -> fail "Invalid JSON response type"

{- Running a command -}

runDBResponse ::
  (MonadUnliftIO m) => Config -> ExceptT String (DB m) Response -> m Response
runDBResponse cfg action = do
  result <- catchDatabaseInUse $ runDB cfg $ do
    resE <- runExceptT action
    case resE of
      Left err -> do
        transactionUndo -- Roll back the current sqlite transaction
        return $ ResponseError $ cs err
      Right res -> return res
  return $ either (ResponseError . cs) id result

catchResponseError :: (Monad m) => ExceptT String m Response -> m Response
catchResponseError m = do
  resE <- runExceptT m
  case resE of
    Left err -> return $ ResponseError $ cs err
    Right res -> return res

commandResponse :: Ctx -> Config -> AmountUnit -> Command -> IO Response
commandResponse ctx cfg unit cmd =
  case cmd of
    -- Mnemonic and account management
    CommandMnemonic e d s -> cmdMnemonic e d s
    CommandCreateAcc t n dM s -> cmdCreateAcc ctx cfg t n dM s
    CommandTestAcc nameM s -> cmdTestAcc ctx cfg nameM s
    CommandRenameAcc old new -> cmdRenameAcc cfg old new
    CommandAccounts nameM -> cmdAccounts cfg nameM
    CommandSyncAcc nameM full -> cmdSyncAcc ctx cfg nameM full
    CommandDeleteAcc name net deriv -> cmdDeleteAcc cfg name net deriv
    -- Address management
    CommandReceive nameM labM -> cmdReceive ctx cfg nameM labM
    CommandAddrs nameM internal p -> cmdAddrs cfg nameM internal p
    CommandLabel nameM i l -> cmdLabel cfg nameM i l
    -- Transaction management
    CommandTxs nameM p -> cmdTxs cfg nameM p
    CommandPrepareTx rcpts nameM fee dust payer minConf strategy o coins ->
      cmdPrepareTx ctx cfg rcpts nameM unit fee dust payer minConf strategy o coins
    CommandPendingTxs nameM p -> cmdPendingTxs ctx cfg nameM p
    CommandSignTx nameM h i o s -> cmdSignTx ctx cfg nameM h i o s
    CommandSendTx h i n -> cmdSendTx ctx cfg h i n
    CommandDeleteTx h -> cmdDeleteTx ctx cfg h
    CommandCoins nameM p -> cmdCoins cfg nameM p
    -- Import/export commands
    CommandExportAcc nameM f -> cmdExportAcc ctx cfg nameM f
    CommandImportAcc f -> cmdImportAcc ctx cfg f
    CommandReviewTx nameM file -> cmdReviewTx ctx cfg nameM file
    CommandExportTx h f -> cmdExportTx cfg h f
    CommandImportTx nameM file -> cmdImportTx ctx cfg nameM file
    -- Backup
    CommandBackup fileM -> cmdBackup cfg fileM
    CommandDiscoverAcc nameM -> cmdDiscoverAccount ctx cfg nameM
    -- Utilities
    CommandVersion -> cmdVersion cfg
    CommandSignMsg a m s -> cmdSignMessage ctx cfg a m s
    CommandPrepareSweep nameM prvKeyM sf st coins outputM f d ->
      cmdPrepareSweep ctx cfg nameM prvKeyM sf st coins outputM f d
    CommandSignSweep nameM h i o k -> cmdSignSweep ctx cfg nameM h i o k
    CommandRollDice n -> cmdRollDice n

{- Mnemonics and accounts -}

cmdMnemonic :: Natural -> Bool -> Natural -> IO Response
cmdMnemonic ent useDice splitMnemIn =
  catchResponseError $ do
    (orig, ms, splitMs) <- genMnemonic ent useDice splitMnemIn
    return $ ResponseMnemonic orig (Text.words ms) (Text.words <$> splitMs)

cmdCreateAcc ::
  Ctx -> Config -> Text -> Network -> Maybe Natural -> Natural -> IO Response
cmdCreateAcc ctx cfg name net derivM splitMnemIn = do
  runDBResponse cfg $ do
    mnem <- askMnemonicPass splitMnemIn
    walletFP <- liftEither $ walletFingerprint net ctx mnem
    d <- maybe (lift $ nextAccountDeriv walletFP net) return derivM
    prvKey <- liftEither $ signingKey net ctx mnem d
    let xpub = deriveXPubKey ctx prvKey
    (_, acc) <- insertAccount net ctx walletFP name xpub
    return $ ResponseAccount acc

cmdTestAcc :: Ctx -> Config -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx cfg nameM splitMnemIn =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        xPubKey = accountXPubKey ctx acc
        d = accountIndex acc
    mnem <- askMnemonicPass splitMnemIn
    xPrvKey <- liftEither $ signingKey net ctx mnem d
    return $
      if deriveXPubKey ctx xPrvKey == xPubKey
        then
          ResponseAccResult
            { responseAccount = acc,
              responseResult = True,
              responseText =
                "The mnemonic and passphrase matched the account"
            }
        else
          ResponseAccResult
            { responseAccount = acc,
              responseResult = False,
              responseText =
                "The mnemonic and passphrase did not match the account"
            }

cmdImportAcc :: Ctx -> Config -> FilePath -> IO Response
cmdImportAcc ctx cfg fp =
  runDBResponse cfg $ do
    (PubKeyDoc xpub net name wallet) <- liftEitherIO $ readMarshalFile ctx fp
    (_, acc) <- insertAccount net ctx wallet name xpub
    return $ ResponseAccount acc

cmdExportAcc :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx cfg nameM file =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    checkPathFree file
    let xpub = accountXPubKey ctx acc
        net = accountNetwork acc
        name = dBAccountName acc
        wallet = accountWallet acc
        doc = PubKeyDoc xpub net name wallet
    liftIO $ writeMarshalFile ctx file doc
    return $ ResponseFile file

cmdRenameAcc :: Config -> Text -> Text -> IO Response
cmdRenameAcc cfg oldName newName =
  runDBResponse cfg $ do
    acc <- renameAccount oldName newName
    return $ ResponseAccount acc

cmdDeleteAcc :: Config -> Text -> Network -> HardPath -> IO Response
cmdDeleteAcc cfg name net path =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName $ Just name
    unless (accountNetwork acc == net) $
      throwError "The network of the account to delete did not match"
    accPath <- liftMaybe "HardPath" $ parseHard $ cs $ dBAccountDerivation acc
    unless (path == accPath) $
      throwError
        "The full derivation path of the account to delete did not match"
    lift $ deleteAccount accId
    return $
      ResponseAccResult acc True $
        "The account " <> name <> " has been deleted"

cmdAccounts :: Config -> Maybe Text -> IO Response
cmdAccounts cfg nameM =
  runDBResponse cfg $ do
    case nameM of
      Just _ -> do
        (_, acc) <- getAccountByName nameM
        return $ ResponseAccounts [acc]
      _ -> do
        accs <- lift getAccounts
        return $ ResponseAccounts $ snd <$> accs

{- Addresses -}

cmdReceive :: Ctx -> Config -> Maybe Text -> Maybe Text -> IO Response
cmdReceive ctx cfg nameM labelM =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    addr <- genExtAddress ctx cfg accId $ fromMaybe "" labelM
    return $ ResponseAddress acc addr

cmdAddrs :: Config -> Maybe Text -> Bool -> Page -> IO Response
cmdAddrs cfg nameM internal page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    as <- lift $ addressPage accId (if internal then AddrInternal else AddrExternal) page
    return $ ResponseAddresses acc as

cmdLabel :: Config -> Maybe Text -> Natural -> Text -> IO Response
cmdLabel cfg nameM idx lab =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    adr <- setAddrLabel accId (fromIntegral idx) lab
    return $ ResponseAddress acc adr

{- Transactions and coins -}

cmdTxs :: Config -> Maybe Text -> Page -> IO Response
cmdTxs cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    txInfos <- txsPage accId page
    return $ ResponseTxs acc txInfos

cmdPrepareTx ::
  Ctx ->
  Config ->
  [(Text, Text)] ->
  Maybe Text ->
  AmountUnit ->
  Natural ->
  Natural ->
  FeePayer ->
  Natural ->
  CoinSelection ->
  Maybe FilePath ->
  [(TxHash, Natural)] ->
  IO Response
cmdPrepareTx ctx cfg rcpTxt nameM unit feeByte dust payer minConf strategy fileM coins =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        cControl = (\(a,b) -> OutPoint a (fromIntegral b)) <$> coins
    rcpts <- liftEither $ mapM (toRecipient net) rcpTxt
    gen <- liftIO initStdGen
    signDat <-
      buildTxSignData net ctx cfg gen strategy accId rcpts feeByte dust payer minConf cControl
    txInfo <- txInfoOf ctx acc signDat
    for_ fileM checkPathFree
    _ <- importPendingTx net ctx accId signDat
    for_ fileM $ \file -> liftIO $ writeJsonFile file $ Json.toJSON signDat
    newAcc <- getAccountById accId
    return $ ResponseTx newAcc txInfo
  where
    toRecipient net (a, v) = do
      addr <- textToAddrE net a
      val <- maybeToEither (cs $ badAmnt v) (readAmount unit v)
      return (addr, val)
    badAmnt v = "Could not parse the amount " <> v <> " as " <> showUnit unit 1

cmdPendingTxs :: Ctx -> Config -> Maybe Text -> Page -> IO Response
cmdPendingTxs ctx cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    txInfos <- pendingTxPage ctx accId page
    return $ ResponseTxs acc txInfos

cmdReviewTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdReviewTx ctx cfg nameM fp =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    tsd <- liftEitherIO $ readJsonFile fp
    txInfo <- txInfoOf ctx acc tsd
    return $ ResponseTx acc txInfo

cmdExportTx :: Config -> TxHash -> FilePath -> IO Response
cmdExportTx cfg nosigH fp =
  runDBResponse cfg $ do
    held <- requirePendingTx nosigH
    checkPathFree fp
    liftIO $ writeJsonFile fp $ Json.toJSON held.pendingTxData
    return $ ResponseFile fp

cmdImportTx :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdImportTx ctx cfg nameM fp =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    tsd <- liftEitherIO $ readJsonFile fp
    txInfo <- txInfoOf ctx acc tsd
    _ <- importPendingTx net ctx accId tsd
    return $ ResponseTx acc txInfo

cmdDeleteTx :: Ctx -> Config -> TxHash -> IO Response
cmdDeleteTx ctx cfg nosigH =
  runDBResponse cfg $ do
    (coins, addrs) <- deleteTx ctx (webStore ctx cfg) nosigH
    return $ ResponseDeleteTx nosigH coins addrs

cmdSignTx ::
  Ctx ->
  Config ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  Natural ->
  IO Response
cmdSignTx ctx cfg nameM nosigHM inputM outputM splitMnemIn =
  runDBResponse cfg $ do
    (tsd, online) <- parseSignInput nosigHM inputM outputM
    when online $
      throwError "The transaction is already online"
    when (isSigned (txSignDataTx tsd)) $
      throwError "The transaction is already signed"
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        idx = fromIntegral $ dBAccountIndex acc
        accPub = accountXPubKey ctx acc
    for_ outputM checkPathFree
    mnem <- askMnemonic splitMnemIn
    prvKey <- askPrvKey net ctx idx mnem accPub
    (newSignData, txInfo) <- liftEither $ signWalletTx net ctx tsd prvKey
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    when (isJust nosigHM) $ void $ importPendingTx net ctx accId newSignData
    for_ outputM $ \o -> liftIO $ writeJsonFile o $ Json.toJSON newSignData
    return $ ResponseTx acc txInfoL

parseSignInput ::
  (MonadUnliftIO m) =>
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  ExceptT String (DB m) (TxSignData, Bool)
parseSignInput nosigHM inputM outputM =
  case (nosigHM, inputM, outputM) of
    (Nothing, Nothing, _) ->
      throwError
        "Provide either a TXHASH or both a --input file and a --output file"
    (Just _, Just _, _) ->
      throwError "Can not specify both a TXHASH and a --input file"
    (_, Just _, Nothing) ->
      throwError "When using a --input file, also provide a --output file"
    (Just h, _, _) -> do
      held <- requirePendingTx h
      return (held.pendingTxData, held.pendingTxOnline)
    (_, Just i, _) -> do
      exist <- liftIO $ D.doesFileExist i
      unless exist $ throwError "Input file does not exist"
      tsd <- liftEitherIO (readJsonFile i)
      return (tsd, False)

cmdCoins :: Config -> Maybe Text -> Page -> IO Response
cmdCoins cfg nameM page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    coins <- coinPage net accId page
    return $ ResponseCoins acc coins

-- | Send a pending transaction of the wallet, by its nosigHash, or a
-- signed transaction from a file
cmdSendTx :: Ctx -> Config -> Maybe TxHash -> Maybe FilePath -> Maybe Network -> IO Response
cmdSendTx ctx cfg nosigHM inputM netM =
  case (nosigHM, inputM) of
    (Just nosigH, Nothing) -> runDBResponse cfg $ do
      (acc, txInfo) <- sendPending ctx (webStore ctx cfg) nosigH
      return $ ResponseTx acc txInfo
    (Nothing, Just file) -> sendTxFile ctx cfg file netM
    (Nothing, Nothing) -> return $ ResponseError "Give the NOSIGHASH of a pending transaction or an --input file"
    (Just _, Just _) -> return $ ResponseError "Can not specify both a NOSIGHASH and an --input file"

-- | Send a signed transaction from a file: one the wallet holds as pending
-- like one sent by its nosigHash, one it does not hold to the store of the
-- given network. See 'sendSigned'.
sendTxFile :: Ctx -> Config -> FilePath -> Maybe Network -> IO Response
sendTxFile ctx cfg file netM =
  runDBResponse cfg $ do
    exists <- liftIO $ D.doesFileExist file
    unless exists $ throwError "Input file does not exist"
    tsd <- liftEitherIO (readJsonFile file)
    sent <- sendSigned ctx (webStore ctx cfg) netM tsd
    return $ case sent of
      SentPending acc txInfo -> ResponseTx acc txInfo
      SentOutside net txid -> ResponseSentTx net txid

{- Synchronisation -}

cmdSyncAcc :: Ctx -> Config -> Maybe Text -> Bool -> IO Response
cmdSyncAcc ctx cfg nameM full =
  runDBResponse cfg $ do
    accs <-
      case nameM of
        Just _ -> (:[]) <$> getAccountByName nameM
        _ -> lift getAccounts
    when (null accs) $ throwError "There are no accounts in the wallet"
    res <- forM accs $ \(accId, acc) -> do
      let net = accountNetwork acc
      sync ctx cfg (webStore ctx cfg net) net accId full
    return $ ResponseSync res

cmdDiscoverAccount :: Ctx -> Config -> Maybe Text -> IO Response
cmdDiscoverAccount ctx cfg nameM = do
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
        store = webStore ctx cfg net
    fromStore $ storeHealth store
    (e, i) <- discoverAddrs ctx cfg store pub
    extendChainTo ctx accId AddrExternal e
    extendChainTo ctx accId AddrInternal i
    -- Perform a full sync after discovery
    res <- sync ctx cfg store net accId True
    return $ ResponseSync [res]

{- Backup and version -}

-- | Copy the database to the given file, which must not exist, or into the
-- backups directory of hw under a name carrying the format version and the
-- time. The database is at the latest format version, since every command
-- migrates it first.
cmdBackup :: Config -> Maybe FilePath -> IO Response
cmdBackup cfg fileM =
  catchResponseError $ do
    database <- liftIO $ databaseFile cfg
    copy <- case fileM of
      Just file -> do
        checkPathFree file
        liftIO $ copyDatabase database file
        return file
      Nothing -> liftIO $ do
        directory <- backupDirectory
        backupDatabase database directory latestDatabaseVersion
    return $ ResponseFile copy

cmdVersion :: Config -> IO Response
cmdVersion cfg =
  runDBResponse cfg $ do
    dbVersion <- databaseVersion
    return $ ResponseVersion (cs $ showVersion Paths.version) (maybe "none" (cs . show) dbVersion)

{- Sweeps -}

-- | Sweep coins to the destinations: the coins at the given addresses and
-- at the addresses of the given private keys, and the coins given by
-- outpoint. The keys are not needed here, only to sign: a sweep can be
-- prepared from addresses or coins alone. A sweep that pays or spends an
-- address of the wallet waits among its pending transactions, to be signed
-- by its nosigHash; one that does not involve the wallet can only be
-- written to a file, to be signed from there.
cmdPrepareSweep ::
  Ctx ->
  Config ->
  Maybe Text ->
  Maybe KeySource ->
  [Text] ->
  [Text] ->
  [(TxHash, Natural)] ->
  Maybe FilePath ->
  Natural ->
  Natural ->
  IO Response
cmdPrepareSweep ctx cfg nameM keysM sweepFromT sweepToT coins outputM feeByte dust =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    secKeys <- maybe (return []) (readSecKeys net) keysM
    sweepFrom <- liftEither $ mapM (textToAddrE net) sweepFromT
    sweepTo <- liftEither $ mapM (textToAddrE net) sweepToT
    found <- fetchSweepCoins net ctx (webStore ctx cfg net) accId secKeys sweepFrom coins sweepTo
    gen <- liftIO initStdGen
    tsd <- liftEither $ buildSweepSignData net ctx gen found sweepTo feeByte dust
    txInfo <- txInfoOf ctx acc tsd
    for_ outputM checkPathFree
    if involvesWallet (txInfoSummary txInfo)
      then void $ importPendingTx net ctx accId tsd
      else
        when (isNothing outputM) $
          throwError "The sweep does not involve this wallet: give an --output file to write it to"
    for_ outputM $ \file -> liftIO $ writeJsonFile file $ Json.toJSON tsd
    return $ ResponseTx acc txInfo

-- | Sign a sweep with private keys: a pending transaction of the wallet,
-- by its nosigHash, or one in a file, which need not involve the wallet
cmdSignSweep ::
  Ctx ->
  Config ->
  Maybe Text ->
  Maybe TxHash ->
  Maybe FilePath ->
  Maybe FilePath ->
  Maybe KeySource ->
  IO Response
cmdSignSweep ctx cfg nameM nosigHM inputM outputM keysM =
  runDBResponse cfg $ do
    (tsd, online) <- parseSignInput nosigHM inputM outputM
    when online $
      throwError "The transaction is already online"
    when (isSigned (txSignDataTx tsd)) $
      throwError "The transaction is already signed"
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
        pub = accountXPubKey ctx acc
    for_ outputM checkPathFree
    -- Without a file, the keys are typed: signing cannot do without them
    secKeys <- readSecKeys net (fromMaybe KeysOnTerminal keysM)
    (newTsd, txInfo) <- liftEither $ signTxWithKeys net ctx tsd pub secKeys
    txInfoL <- lift $ fillTxInfoLabels net txInfo
    when (isJust nosigHM) $ void $ importPendingTx net ctx accId newTsd
    for_ outputM $ \o -> liftIO $ writeJsonFile o $ Json.toJSON newTsd
    return $ ResponseTx acc txInfoL

{- Signed messages -}

cmdSignMessage :: Ctx -> Config -> Text -> Text -> Natural -> IO Response
cmdSignMessage ctx cfg addrTxt txtMsg splitMnemIn =
  runDBResponse cfg $ do
    (acc, addrDB) <- getAddressByText addrTxt
    let accPub = accountXPubKey ctx acc
        net = accountNetwork acc
        idx = fromIntegral $ dBAccountIndex acc
    addr <- liftEither $ textToAddrE net addrTxt
    -- Warn before asking for the mnemonic so that the user can abort. Use
    -- stderr to keep the armored block and the --json output clean.
    liftIO $ mapM_ (hPutStrLn stderr) $ messageWarnings txtMsg
    mnem <- askMnemonic splitMnemIn
    accPrvKey <- askPrvKey net ctx idx mnem accPub
    let addrPrvKey = derivePath ctx (addrPath addrDB) accPrvKey
    msgSig <- liftEither $ signMessage ctx addr addrPrvKey.key txtMsg
    return $ ResponseSignMsg txtMsg (cs $ B64.encode msgSig) addrTxt

{- Dice -}

-- | Dice rolls for the user, drawn from the system entropy
cmdRollDice :: Natural -> IO Response
cmdRollDice n = do
  (rolls, source) <- rollDice n
  return $ ResponseRollDice rolls source
