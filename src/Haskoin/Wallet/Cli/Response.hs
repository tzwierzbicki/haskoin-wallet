{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answer of hw to a command, which the pretty printer shows to the
-- user and --json writes for scripts. In JSON, an answer is an object that
-- names its type, and reads back as the answer it was.
module Haskoin.Wallet.Cli.Response (Response (..), responseFailed) where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Database.Account (accountNetwork, accountWithSpendable, parseAccountWithSpendable)
import Haskoin.Wallet.Database.Address (CoinBalance, addressWithCoinBalance, addressWithSpendable, parseAddressWithCoinBalance, parseAddressWithSpendable)
import Haskoin.Wallet.Database.Coin (JsonCoin, LeftCoin)
import Haskoin.Wallet.Database (DBAccount, DBAddress)
import Haskoin.Wallet.Keys (foreignKindName, foreignKindNamed)
import Haskoin.Wallet.Labels (ImportReport, parseReport, reportPairs)
import Haskoin.Wallet.Tx.FeeRates (FeeRates)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..), foreignPath)
import Haskoin.Wallet.Database.TxInfo (TxInfo)
import Numeric.Natural (Natural)

data Response
  = -- | The command failed, and why
    ResponseError !Text
  | -- | A new mnemonic: the source of its entropy, and its words
    ResponseMnemonic !Text ![Text]
  | -- | The shares of a new backup: the source of its entropy, the number
    -- of shares that rebuild the seed, and the words of each share
    ResponseShares !Text !Natural ![[Text]]
  | -- | An account, with the balance it can spend now
    ResponseAccount !DBAccount !Word64
  | -- | An account, whether the command did what it was asked, and what it
    -- says of it
    ResponseAccResult !DBAccount !Bool !Text
  | -- | The file the command wrote
    ResponseFile !FilePath
  | -- | What importacc did with a file: the accounts it names, the labels
    -- and locks set, and what it could not place
    ResponseImport !ImportReport
  | -- | Accounts, each with the balance it can spend now
    ResponseAccounts ![(DBAccount, Word64)]
  | -- | Accounts, each with its output descriptor and, for a bip84 one, its
    -- key with the zpub prefix of SLIP-132, as descriptor shows them
    ResponseDescriptors ![(DBAccount, Text, Maybe Text)]
  | -- | An address of the account, with the balance it can spend now
    ResponseAddress !DBAccount !DBAddress !Word64
  | -- | Addresses of the account, each with the balance it can spend now
    ResponseAddresses !DBAccount ![(DBAddress, Word64)]
  | -- | Addresses of the account that hold coins, each with the balance of
    -- its coins
    ResponseBalances !DBAccount ![(DBAddress, CoinBalance)]
  | -- | The address of the account at a path of one of its chains, as
    -- checkaddr derives it, with the label the wallet has for it, if any
    ResponseCheckAddr !DBAccount !SoftPath !Text !Text
  | -- | Transactions of the account
    ResponseTxs !DBAccount ![TxInfo]
  | -- | Pending transactions, each with its account: those of the account
    -- asked for, or of every account when none is
    ResponsePendingTxs !(Maybe DBAccount) ![(DBAccount, TxInfo)]
  | -- | A transaction of the account
    ResponseTx !DBAccount !TxInfo
  | -- | A transaction just labelled, as each account that holds it sees it:
    -- the account of a pending one, or every account whose history has it
    ResponseLabelledTxs ![(DBAccount, TxInfo)]
  | -- | A transaction of the account just prepared by preparetx or
    -- preparesendall, with the coins it leaves in the account and why: the
    -- coins that coin control chose and could not spend, and for a
    -- send-all every coin it does not send
    ResponsePrepared !DBAccount !TxInfo ![LeftCoin]
  | -- | A transaction outside the wallet, a sweep between outside
    -- addresses, on its network
    ResponseOutsideTx !Network !TxInfo
  | -- | A transaction the wallet does not hold, sent on its network
    ResponseSentTx !Network !TxHash
  | -- | A pending transaction deleted, by its nosigHash and its id if it
    -- was signed, with the number of coins and of addresses it held
    ResponseDeleteTx !TxHash !(Maybe TxHash) !Natural !Natural
  | -- | Coins of the account
    ResponseCoins !DBAccount ![JsonCoin]
  | -- | A signed message, its signature in base64, and the address that
    -- signed it
    ResponseSignMsg !Text !Text !Text
  | -- | A message, a signature in base64, the address it is said to be
    -- signed from, and whether it is
    ResponseVerifyMsg !Text !Text !Text !Bool
  | -- | The sync of each account synced, and the accounts not synced, each
    -- with the failure of its store
    ResponseSync ![SyncRes] ![(DBAccount, Text)]
  | -- | The accounts of a seed recovered on a network, searched with the
    -- gap of accounts, each with its sync, and the funds of the seed at
    -- the accounts hw does not hold
    ResponseRecover !Network !Natural ![SyncRes] ![ForeignFunds]
  | -- | The fee rates recent blocks paid, on the network asked
    ResponseFees !Network !FeeRates
  deriving (Eq, Show)

{- JSON -}

-- | An answer in JSON: an object naming its type, with its fields
typed :: Text -> [Pair] -> Value
typed kind fields = object (("type" .= kind) : fields)

instance ToJSON Response where
  toJSON = \case
    ResponseError err -> typed "error" ["error" .= err]
    ResponseMnemonic source ws ->
      typed "mnemonic" ["entropysource" .= source, "mnemonic" .= ws]
    ResponseShares source threshold shares ->
      typed "shares" ["entropysource" .= source, "threshold" .= threshold, "shares" .= shares]
    ResponseAccount acc spendable -> typed "account" ["account" .= accountWithSpendable acc spendable]
    ResponseAccResult acc result text -> typed "accresult" ["account" .= acc, "result" .= result, "text" .= text]
    ResponseFile file -> typed "file" ["file" .= file]
    ResponseImport report -> typed "import" (reportPairs report)
    ResponseAccounts accs -> typed "accounts" ["accounts" .= (uncurry accountWithSpendable <$> accs)]
    ResponseDescriptors held ->
      typed "descriptors" ["descriptors" .= [object ["account" .= acc, "descriptor" .= descriptor, "slip132" .= slip132] | (acc, descriptor, slip132) <- held]]
    ResponseAddress acc addr spendable ->
      typed "address" ["account" .= acc, "address" .= addressWithSpendable addr spendable]
    ResponseAddresses acc addrs ->
      typed "addresses" ["account" .= acc, "addresses" .= (uncurry addressWithSpendable <$> addrs)]
    ResponseBalances acc addrs ->
      typed "balances" ["account" .= acc, "addresses" .= (uncurry addressWithCoinBalance <$> addrs)]
    ResponseCheckAddr acc path addr label ->
      typed "checkaddr" ["account" .= acc, "derivation" .= path, "address" .= addr, "label" .= label]
    ResponseTxs acc txs -> typed "txs" ["account" .= acc, "txs" .= (marshalValue (accountNetwork acc) <$> txs)]
    ResponsePendingTxs asked held ->
      typed "pendingtxs" ["account" .= asked, "txs" .= (heldTxJson <$> held)]
    ResponseTx acc info -> typed "tx" ["account" .= acc, "tx" .= marshalValue (accountNetwork acc) info]
    ResponseLabelledTxs held -> typed "labelledtxs" ["txs" .= (heldTxJson <$> held)]
    ResponsePrepared acc info left ->
      typed "prepared" ["account" .= acc, "tx" .= marshalValue (accountNetwork acc) info, "left" .= left]
    ResponseOutsideTx net info -> typed "outsidetx" ["network" .= net.name, "tx" .= marshalValue net info]
    ResponseSentTx net txid -> typed "senttx" ["network" .= net.name, "txid" .= txid]
    ResponseDeleteTx nosigH txidM coins addrs ->
      typed "deletetx" ["nosighash" .= nosigH, "txid" .= txidM, "freedcoins" .= coins, "freedaddrs" .= addrs]
    ResponseCoins acc coins -> typed "coins" ["account" .= acc, "coins" .= (marshalValue (accountNetwork acc) <$> coins)]
    ResponseSignMsg message signature addr ->
      typed "signmsg" ["message" .= message, "signature" .= signature, "address" .= addr]
    ResponseVerifyMsg message signature addr valid ->
      typed "verifymsg" ["message" .= message, "signature" .= signature, "address" .= addr, "valid" .= valid]
    ResponseSync results notSynced ->
      typed "sync" ["syncres" .= (syncResJson <$> results), "notsynced" .= (notSyncedJson <$> notSynced)]
    ResponseRecover net gap results elsewhere ->
      typed "recover" ["network" .= net.name, "accountgap" .= gap, "syncres" .= (syncResJson <$> results), "unspendable" .= (foreignFundsJson net <$> elsewhere)]
    ResponseFees net rates -> typed "fees" ["network" .= net.name, "feerates" .= rates]

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "error" -> ResponseError <$> o .: "error"
      "mnemonic" -> ResponseMnemonic <$> o .: "entropysource" <*> o .: "mnemonic"
      "shares" -> ResponseShares <$> o .: "entropysource" <*> o .: "threshold" <*> o .: "shares"
      "account" -> uncurry ResponseAccount <$> (parseAccountWithSpendable =<< o .: "account")
      "accresult" -> ResponseAccResult <$> o .: "account" <*> o .: "result" <*> o .: "text"
      "file" -> ResponseFile <$> o .: "file"
      "import" -> ResponseImport <$> parseReport o
      "accounts" -> ResponseAccounts <$> (mapM parseAccountWithSpendable =<< o .: "accounts")
      "descriptors" -> ResponseDescriptors <$> (mapM heldDescriptor =<< o .: "descriptors")
      "address" -> do
        acc <- o .: "account"
        uncurry (ResponseAddress acc) <$> (parseAddressWithSpendable =<< o .: "address")
      "addresses" -> ResponseAddresses <$> o .: "account" <*> (mapM parseAddressWithSpendable =<< o .: "addresses")
      "balances" -> ResponseBalances <$> o .: "account" <*> (mapM parseAddressWithCoinBalance =<< o .: "addresses")
      "checkaddr" -> ResponseCheckAddr <$> o .: "account" <*> o .: "derivation" <*> o .: "address" <*> o .: "label"
      "txs" -> do
        acc <- o .: "account"
        ResponseTxs acc <$> (mapM (unmarshalValue (accountNetwork acc)) =<< o .: "txs")
      "tx" -> do
        acc <- o .: "account"
        ResponseTx acc <$> (unmarshalValue (accountNetwork acc) =<< o .: "tx")
      "pendingtxs" -> ResponsePendingTxs <$> o .: "account" <*> (mapM heldTx =<< o .: "txs")
      "labelledtxs" -> ResponseLabelledTxs <$> (mapM heldTx =<< o .: "txs")
      "prepared" -> do
        acc <- o .: "account"
        ResponsePrepared acc <$> (unmarshalValue (accountNetwork acc) =<< o .: "tx") <*> o .: "left"
      "outsidetx" -> do
        net <- network o
        ResponseOutsideTx net <$> (unmarshalValue net =<< o .: "tx")
      "senttx" -> ResponseSentTx <$> network o <*> o .: "txid"
      "deletetx" -> ResponseDeleteTx <$> o .: "nosighash" <*> o .:? "txid" <*> o .: "freedcoins" <*> o .: "freedaddrs"
      "coins" -> do
        acc <- o .: "account"
        ResponseCoins acc <$> (mapM (unmarshalValue (accountNetwork acc)) =<< o .: "coins")
      "signmsg" -> ResponseSignMsg <$> o .: "message" <*> o .: "signature" <*> o .: "address"
      "verifymsg" -> ResponseVerifyMsg <$> o .: "message" <*> o .: "signature" <*> o .: "address" <*> o .: "valid"
      "sync" -> ResponseSync <$> (mapM parseSyncRes =<< o .: "syncres") <*> (mapM parseNotSynced =<< o .: "notsynced")
      "recover" ->
        ResponseRecover
          <$> network o
          <*> o .: "accountgap"
          <*> (mapM parseSyncRes =<< o .: "syncres")
          <*> (mapM parseForeignFunds =<< o .: "unspendable")
      "fees" -> ResponseFees <$> network o <*> o .: "feerates"
      _ -> fail "Invalid JSON response type"
    where
      network o = maybe (fail "Unknown network") return . netByName =<< o .: "network"
      -- The descriptor of an account
      heldDescriptor = withObject "descriptor of an account" $ \entry ->
        (,,) <$> entry .: "account" <*> entry .: "descriptor" <*> entry .:? "slip132"
      -- A transaction with its account, which names its network
      heldTx = withObject "transaction of an account" $ \entry -> do
        acc <- entry .: "account"
        (,) acc <$> (unmarshalValue (accountNetwork acc) =<< entry .: "tx")

-- | A transaction with its account in JSON, in a list of transactions of
-- several accounts
heldTxJson :: (DBAccount, TxInfo) -> Value
heldTxJson (acc, info) = object ["account" .= acc, "tx" .= marshalValue (accountNetwork acc) info]

-- | The sync of an account in JSON, with what the account can spend now
syncResJson :: SyncRes -> Value
syncResJson (SyncRes acc spendable best height txs coins rebroadcast conflicted discovered) =
  object
    [ "account" .= accountWithSpendable acc spendable,
      "bestblock" .= best,
      "bestheight" .= height,
      "txupdates" .= txs,
      "coinupdates" .= coins,
      "rebroadcast" .= rebroadcast,
      "conflicted" .= conflicted,
      "discovered" .= discovered
    ]

-- | An account not synced in JSON, with the failure of its store
notSyncedJson :: (DBAccount, Text) -> Value
notSyncedJson (acc, failure) = object ["account" .= acc, "error" .= failure]

parseNotSynced :: Value -> Parser (DBAccount, Text)
parseNotSynced = withObject "notsynced" $ \o -> (,) <$> o .: "account" <*> o .: "error"

parseSyncRes :: Value -> Parser SyncRes
parseSyncRes =
  withObject "syncres" $ \o -> do
    (acc, spendable) <- parseAccountWithSpendable =<< o .: "account"
    SyncRes acc spendable
      <$> o .: "bestblock"
      <*> o .: "bestheight"
      <*> o .: "txupdates"
      <*> o .: "coinupdates"
      <*> o .: "rebroadcast"
      <*> o .: "conflicted"
      <*> o .: "discovered"

-- | The funds of a seed at an account hw does not hold, in JSON: the kind
-- of the account, its path on the network and its index, how many of its
-- addresses were used, and what they hold in satoshis
foreignFundsJson :: Network -> ForeignFunds -> Value
foreignFundsJson net funds =
  object
    [ "type" .= foreignKindName (foreignKind funds),
      "path" .= pathToStr (foreignPath net funds),
      "index" .= foreignIndex funds,
      "addresses" .= foreignUsed funds,
      "balance" .= foreignBalance funds
    ]

-- | The funds back from their JSON: the path follows from the kind and
-- the index, so it is not read
parseForeignFunds :: Value -> Parser ForeignFunds
parseForeignFunds =
  withObject "unspendable" $ \o -> do
    kind <- maybe (fail "Unknown kind of account") return . foreignKindNamed =<< o .: "type"
    ForeignFunds kind <$> o .: "index" <*> o .: "addresses" <*> o .: "balance"

-- | Whether the answer is a failure, which hw exits with: an error, a
-- signature found not valid, or a sync that left accounts out, so that a
-- script can tell from the exit code
responseFailed :: Response -> Bool
responseFailed (ResponseError _) = True
responseFailed (ResponseVerifyMsg _ _ _ valid) = not valid
responseFailed (ResponseSync _ notSynced) = not (null notSynced)
responseFailed _ = False
