{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}

module Haskoin.Wallet.Commands where

import Conduit (MonadUnliftIO)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader (MonadIO(..), MonadTrans(lift))
import Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Base64 as B64
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Version (showVersion)
import Data.Word (Word64)
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
  | -- | An account with the balance it can spend now
    ResponseAccount
      { responseAccount :: !DBAccount,
        responseSpendable :: !Word64
      }
  | ResponseAccResult
      { responseAccount :: !DBAccount,
        responseResult :: !Bool,
        responseText :: !Text
      }
  | ResponseFile
      { responseTxFile :: !FilePath
      }
  | -- | Accounts, each with the balance it can spend now
    ResponseAccounts
      { responseAccounts :: ![(DBAccount, Word64)]
      }
  | -- | An address of the account, with the balance it can spend now
    ResponseAddress
      { responseAccount :: !DBAccount,
        responseAddress :: !DBAddress,
        responseSpendable :: !Word64
      }
  | -- | Addresses of the account, each with the balance it can spend now
    ResponseAddresses
      { responseAccount :: !DBAccount,
        responseAddresses :: ![(DBAddress, Word64)]
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
  | -- | The accounts of a seed recovered on a network, each with its sync
    ResponseRecover
      { responseNetwork :: !Network,
        responseAccountGap :: !Natural,
        responseRecovered :: ![SyncRes]
      }
  | ResponseVersion
      { responseVersion :: !Text,
        responseDBVersion :: !Text
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
      ResponseAccount a s ->
        object
          [ "type" .= Json.String "account",
            "account" .= accountWithSpendable a s
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
            "accounts" .= (uncurry accountWithSpendable <$> as)
          ]
      ResponseAddress a addr s ->
        object
          [ "type" .= Json.String "address",
            "account" .= a,
            "address" .= addressWithSpendable addr s
          ]
      ResponseAddresses a adrs ->
        object
          [ "type" .= Json.String "addresses",
            "account" .= a,
            "addresses" .= (uncurry addressWithSpendable <$> adrs)
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
      ResponseSync xs ->
        object
          [ "type" .= Json.String "sync",
            "syncres" .= (syncResJson <$> xs)
          ]
      ResponseRecover net gap xs ->
        object
          [ "type" .= Json.String "recover",
            "network" .= net.name,
            "accountgap" .= gap,
            "syncres" .= (syncResJson <$> xs)
          ]
      ResponseVersion v dbv ->
        object
          [ "type" .= Json.String "version",
            "version" .= v,
            "dbversion" .= dbv
          ]

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
          uncurry ResponseAccount <$> (parseAccountWithSpendable =<< o .: "account")
        "accresult" ->
          ResponseAccResult
            <$> o .: "account"
            <*> o .: "result"
            <*> o .: "text"
        "file" ->
          ResponseFile
            <$> o .: "file"
        "accounts" ->
          ResponseAccounts <$> (mapM parseAccountWithSpendable =<< o .: "accounts")
        "address" -> do
          a <- o .: "account"
          (addr, s) <- parseAddressWithSpendable =<< o .: "address"
          return $ ResponseAddress a addr s
        "addresses" ->
          ResponseAddresses
            <$> o .: "account"
            <*> (mapM parseAddressWithSpendable =<< o .: "addresses")
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
        "sync" -> ResponseSync <$> (mapM parseSyncRes =<< o .: "syncres")
        "recover" -> do
          net <- maybe (fail "Unknown network") return . netByName =<< o .: "network"
          ResponseRecover net <$> o .: "accountgap" <*> (mapM parseSyncRes =<< o .: "syncres")
        "version" ->
          ResponseVersion
            <$> o .: "version"
            <*> o .: "dbversion"
        _ -> fail "Invalid JSON response type"


{- The result of a sync in JSON -}

syncResJson :: SyncRes -> Json.Value
syncResJson (SyncRes as sp bb bh tc cc rb ck na) =
  object
    [ "account" .= accountWithSpendable as sp,
      "bestblock" .= bb,
      "bestheight" .= bh,
      "txupdates" .= tc,
      "coinupdates" .= cc,
      "rebroadcast" .= rb,
      "conflicted" .= ck,
      "discovered" .= na
    ]

parseSyncRes :: Json.Value -> Parser SyncRes
parseSyncRes =
  Json.withObject "syncres" $ \x -> do
    (acc, spendable) <- parseAccountWithSpendable =<< x .: "account"
    SyncRes acc spendable
      <$> x .: "bestblock"
      <*> x .: "bestheight"
      <*> x .: "txupdates"
      <*> x .: "coinupdates"
      <*> x .: "rebroadcast"
      <*> x .: "conflicted"
      <*> x .: "discovered"

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
    CommandCreateAcc t n s -> cmdCreateAcc ctx cfg t n s
    CommandRecover n g s -> cmdRecover ctx cfg n g s
    CommandTestAcc nameM s -> cmdTestAcc ctx cfg nameM s
    CommandRenameAcc old new -> cmdRenameAcc cfg old new
    CommandAccounts nameM -> cmdAccounts cfg nameM
    CommandSync nameM full -> cmdSync ctx cfg nameM full
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
    CommandCoins nameM locked p -> cmdCoins cfg nameM locked p
    CommandLockCoin outpoint -> cmdLockCoin cfg outpoint
    CommandUnlockCoin outpoint -> cmdUnlockCoin cfg outpoint
    -- Import/export commands
    CommandExportAcc nameM f -> cmdExportAcc ctx cfg nameM f
    CommandImportAcc f -> cmdImportAcc ctx cfg f
    CommandReviewTx nameM file -> cmdReviewTx ctx cfg nameM file
    CommandExportTx h f -> cmdExportTx cfg h f
    CommandImportTx nameM file -> cmdImportTx ctx cfg nameM file
    -- Backup
    CommandBackup fileM -> cmdBackup cfg fileM
    -- Utilities
    CommandVersion -> cmdVersion cfg
    CommandSignMsg a m s -> cmdSignMessage ctx cfg a m s
    CommandPrepareSweep nameM prvKeyM sf st coins outputM f d ->
      cmdPrepareSweep ctx cfg nameM prvKeyM sf st coins outputM f d
    CommandSignSweep nameM h i o k -> cmdSignSweep ctx cfg nameM h i o k

{- Mnemonics and accounts -}

cmdMnemonic :: Natural -> Bool -> Natural -> IO Response
cmdMnemonic ent useDice splitMnemIn =
  catchResponseError $ do
    let sources =
          Sources
            { readSystem = systemEntropy,
              askDice = if useDice then Just (askDiceEntropy terminal) else Nothing,
              say = tell terminal
            }
    (orig, ms, splitMs) <- genMnemonic sources ent splitMnemIn
    return $ ResponseMnemonic orig (Text.words ms) (Text.words <$> splitMs)

cmdCreateAcc :: Ctx -> Config -> Text -> Network -> Natural -> IO Response
cmdCreateAcc ctx cfg name net splitMnemIn = do
  runDBResponse cfg $ do
    -- Before the user types the mnemonic, not after
    checkNameFree name
    mnem <- askNewMnemonicPass terminal splitMnemIn
    seed <- liftEither $ seedOf ctx mnem
    (_, xpub) <- lift $ nextAccount net ctx seed
    created <- insertAccount net ctx name xpub (Just (seedFingerprint seed))
    -- The accounts of the seed created before hw recorded fingerprints
    lift $ recordFingerprint ctx seed
    accountResponse created

-- | Recover the accounts of a seed that were paid on a network, from its
-- mnemonic: see "Haskoin.Wallet.Sync".
cmdRecover :: Ctx -> Config -> Network -> Natural -> Natural -> IO Response
cmdRecover ctx cfg net accountGap splitMnemIn =
  runDBResponse cfg $ do
    let store = webStore ctx cfg net
    -- The store is needed: an offline computer hears it before the mnemonic is typed
    fromStore $ storeHealth store
    -- Typed once: a mistyped mnemonic finds no paid account
    mnem <- askMnemonicPass terminal splitMnemIn
    seed <- liftEither $ seedOf ctx mnem
    ResponseRecover net accountGap <$> recoverSeed ctx cfg store net seed accountGap

cmdTestAcc :: Ctx -> Config -> Maybe Text -> Natural -> IO Response
cmdTestAcc ctx cfg nameM splitMnemIn =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    mnem <- askMnemonicPass terminal splitMnemIn
    matched <- matchAccount terminal net ctx (accountIndex acc) (accountXPubKey ctx acc) mnem
    case matched of
      Just (seed, _) -> do
        -- The seed is proven: every account of it gets its fingerprint
        lift $ recordFingerprint ctx seed
        proven <- getAccountById accId
        return $ ResponseAccResult proven True "The mnemonic and passphrase matched the account"
      Nothing -> return $ ResponseAccResult acc False "The mnemonic and passphrase did not match the account"

cmdImportAcc :: Ctx -> Config -> FilePath -> IO Response
cmdImportAcc ctx cfg fp =
  runDBResponse cfg $ do
    PubKeyDoc xpub net name fingerprintM <- liftEitherIO $ readMarshalFile ctx fp
    heldM <- lift $ accountWithKey net ctx xpub
    case (heldM, fingerprintM) of
      -- An account the wallet has already, whose document now names its seed
      (Just (accId, held), Just fingerprint) | isNothing (dBAccountFingerprint held) -> do
        learnFingerprint accId fingerprint
        accountResponse . (accId,) =<< getAccountById accId
      _ -> accountResponse =<< insertAccount net ctx name xpub fingerprintM

cmdExportAcc :: Ctx -> Config -> Maybe Text -> FilePath -> IO Response
cmdExportAcc ctx cfg nameM file =
  runDBResponse cfg $ do
    (_, acc) <- getAccountByName nameM
    checkPathFree file
    let xpub = accountXPubKey ctx acc
        net = accountNetwork acc
        name = dBAccountName acc
        doc = PubKeyDoc xpub net name (accountFingerprint acc)
    liftIO $ writeMarshalFile ctx file doc
    return $ ResponseFile file

cmdRenameAcc :: Config -> Text -> Text -> IO Response
cmdRenameAcc cfg oldName newName =
  runDBResponse cfg $ do
    _ <- renameAccount oldName newName
    accountResponse =<< getAccountByName (Just newName)

cmdDeleteAcc :: Config -> Text -> Network -> HardPath -> IO Response
cmdDeleteAcc cfg name net path =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName $ Just name
    unless (accountNetwork acc == net) $
      throwError "The network of the account to delete did not match"
    unless (path == accountPath acc) $
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
        named <- getAccountByName nameM
        ResponseAccounts <$> mapM withSpendable [named]
      _ -> do
        accs <- lift getAccounts
        ResponseAccounts <$> mapM withSpendable accs

-- | An account with the balance it can spend now
withSpendable :: (MonadUnliftIO m) => (DBAccountId, DBAccount) -> ExceptT String (DB m) (DBAccount, Word64)
withSpendable (accId, acc) = (acc,) <$> spendableBalance (accountNetwork acc) accId

-- | The answer showing an account, with the balance it can spend now
accountResponse :: (MonadUnliftIO m) => (DBAccountId, DBAccount) -> ExceptT String (DB m) Response
accountResponse account = uncurry ResponseAccount <$> withSpendable account

{- Addresses -}

-- | The answer showing an address of the account, with the balance it can
-- spend now
addressResponse :: (MonadUnliftIO m) => DBAccountId -> DBAccount -> DBAddress -> ExceptT String (DB m) Response
addressResponse accId acc addr = do
  balances <- spendableAtAddresses (accountNetwork acc) accId [dBAddressAddress addr]
  return $ ResponseAddress acc addr (spendableAt balances addr)

-- | What an address can spend now, among the spendable balances read for
-- it: nothing when it holds no such coin
spendableAt :: Map Text Word64 -> DBAddress -> Word64
spendableAt balances addr = Map.findWithDefault 0 (dBAddressAddress addr) balances

cmdReceive :: Ctx -> Config -> Maybe Text -> Maybe Text -> IO Response
cmdReceive ctx cfg nameM labelM =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    addr <- genExtAddress ctx cfg accId $ fromMaybe "" labelM
    addressResponse accId acc addr

cmdAddrs :: Config -> Maybe Text -> Bool -> Page -> IO Response
cmdAddrs cfg nameM internal page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    as <- lift $ addressPage accId (if internal then AddrInternal else AddrExternal) page
    balances <- spendableAtAddresses (accountNetwork acc) accId (dBAddressAddress <$> as)
    return $ ResponseAddresses acc [(addr, spendableAt balances addr) | addr <- as]

cmdLabel :: Config -> Maybe Text -> Natural -> Text -> IO Response
cmdLabel cfg nameM idx lab =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    adr <- setAddrLabel accId (fromIntegral idx) lab
    addressResponse accId acc adr

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
      val <- readAmount unit v
      return (addr, fromIntegral val)

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
    (seed, prvKey) <- askSigningKey terminal net ctx splitMnemIn idx accPub
    -- The signed data names the seed, for an online wallet to learn it
    let toSign = tsd {txSignDataFingerprint = Just (seedFingerprint seed)}
    (newSignData, txInfo) <- liftEither $ signWalletTx net ctx toSign prvKey
    lift $ recordFingerprint ctx seed
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

cmdCoins :: Config -> Maybe Text -> Bool -> Page -> IO Response
cmdCoins cfg nameM locked page =
  runDBResponse cfg $ do
    (accId, acc) <- getAccountByName nameM
    let net = accountNetwork acc
    coins <- coinPage net accId (if locked then LockedCoins else AllCoins) page
    return $ ResponseCoins acc coins

-- | Lock a coin at the user's request, and show it
cmdLockCoin :: Config -> OutPoint -> IO Response
cmdLockCoin cfg outpoint =
  runDBResponse cfg $ do
    (acc, coin) <- lockCoin outpoint
    return $ ResponseCoins acc [coin]

-- | Unlock a coin the user locked, and show it
cmdUnlockCoin :: Config -> OutPoint -> IO Response
cmdUnlockCoin cfg outpoint =
  runDBResponse cfg $ do
    (acc, coin) <- unlockCoin outpoint
    return $ ResponseCoins acc [coin]

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

cmdSync :: Ctx -> Config -> Maybe Text -> Bool -> IO Response
cmdSync ctx cfg nameM full =
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
    secKeys <- maybe (return []) (readSecKeys terminal net) keysM
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
    secKeys <- readSecKeys terminal net (fromMaybe KeysOnTerminal keysM)
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
    (seed, accPrvKey) <- askSigningKey terminal net ctx splitMnemIn idx accPub
    lift $ recordFingerprint ctx seed
    let addrPrvKey = derivePath ctx (addrPath addrDB) accPrvKey
    msgSig <- liftEither $ signMessage ctx addr addrPrvKey.key txtMsg
    return $ ResponseSignMsg txtMsg (cs $ B64.encode msgSig) addrTxt

