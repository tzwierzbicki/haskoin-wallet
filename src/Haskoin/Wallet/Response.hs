{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answer of hw to a command, which the pretty printer shows to the
-- user and --json writes for scripts. In JSON, an answer is an object that
-- names its type, and reads back as the answer it was.
module Haskoin.Wallet.Response (Response (..)) where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Account (accountNetwork, accountWithSpendable, parseAccountWithSpendable)
import Haskoin.Wallet.Address (CoinBalance, addressWithCoinBalance, addressWithSpendable, parseAddressWithCoinBalance, parseAddressWithSpendable)
import Haskoin.Wallet.Coin (JsonCoin, LeftCoin)
import Haskoin.Wallet.Database (DBAccount, DBAddress)
import Haskoin.Wallet.Sync (SyncRes (..))
import Haskoin.Wallet.TxInfo (TxInfo)
import Numeric.Natural (Natural)

data Response
  = -- | The command failed, and why
    ResponseError !Text
  | -- | A new mnemonic: the source of its entropy, its words, and the words
    -- of each of its parts when it is split
    ResponseMnemonic !Text ![Text] ![[Text]]
  | -- | An account, with the balance it can spend now
    ResponseAccount !DBAccount !Word64
  | -- | An account, whether the command did what it was asked, and what it
    -- says of it
    ResponseAccResult !DBAccount !Bool !Text
  | -- | The file the command wrote
    ResponseFile !FilePath
  | -- | Accounts, each with the balance it can spend now
    ResponseAccounts ![(DBAccount, Word64)]
  | -- | An address of the account, with the balance it can spend now
    ResponseAddress !DBAccount !DBAddress !Word64
  | -- | Addresses of the account, each with the balance it can spend now
    ResponseAddresses !DBAccount ![(DBAddress, Word64)]
  | -- | Addresses of the account that hold coins, each with the balance of
    -- its coins
    ResponseBalances !DBAccount ![(DBAddress, CoinBalance)]
  | -- | Transactions of the account
    ResponseTxs !DBAccount ![TxInfo]
  | -- | Pending transactions, each with its account: those of the account
    -- asked for, or of every account when none is
    ResponsePendingTxs !(Maybe DBAccount) ![(DBAccount, TxInfo)]
  | -- | A transaction of the account
    ResponseTx !DBAccount !TxInfo
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
  | -- | A pending transaction deleted, by its nosigHash, with the number of
    -- coins and of addresses it held
    ResponseDeleteTx !TxHash !Natural !Natural
  | -- | Coins of the account
    ResponseCoins !DBAccount ![JsonCoin]
  | -- | A signed message, its signature in base64, and the address that
    -- signed it
    ResponseSignMsg !Text !Text !Text
  | -- | The sync of each account
    ResponseSync ![SyncRes]
  | -- | The accounts of a seed recovered on a network, searched with the
    -- gap of accounts, each with its sync
    ResponseRecover !Network !Natural ![SyncRes]
  | -- | The version of hw, and the format version of its database
    ResponseVersion !Text !Text
  deriving (Eq, Show)

{- JSON -}

-- | An answer in JSON: an object naming its type, with its fields
typed :: Text -> [Pair] -> Value
typed kind fields = object (("type" .= kind) : fields)

instance ToJSON Response where
  toJSON = \case
    ResponseError err -> typed "error" ["error" .= err]
    ResponseMnemonic source ws parts ->
      typed "mnemonic" ["entropysource" .= source, "mnemonic" .= ws, "splitmnemonic" .= parts]
    ResponseAccount acc spendable -> typed "account" ["account" .= accountWithSpendable acc spendable]
    ResponseAccResult acc result text -> typed "accresult" ["account" .= acc, "result" .= result, "text" .= text]
    ResponseFile file -> typed "file" ["file" .= file]
    ResponseAccounts accs -> typed "accounts" ["accounts" .= (uncurry accountWithSpendable <$> accs)]
    ResponseAddress acc addr spendable ->
      typed "address" ["account" .= acc, "address" .= addressWithSpendable addr spendable]
    ResponseAddresses acc addrs ->
      typed "addresses" ["account" .= acc, "addresses" .= (uncurry addressWithSpendable <$> addrs)]
    ResponseBalances acc addrs ->
      typed "balances" ["account" .= acc, "addresses" .= (uncurry addressWithCoinBalance <$> addrs)]
    ResponseTxs acc txs -> typed "txs" ["account" .= acc, "txs" .= (marshalValue (accountNetwork acc) <$> txs)]
    ResponsePendingTxs asked held ->
      typed "pendingtxs" ["account" .= asked, "txs" .= [object ["account" .= acc, "tx" .= marshalValue (accountNetwork acc) info] | (acc, info) <- held]]
    ResponseTx acc info -> typed "tx" ["account" .= acc, "tx" .= marshalValue (accountNetwork acc) info]
    ResponsePrepared acc info left ->
      typed "prepared" ["account" .= acc, "tx" .= marshalValue (accountNetwork acc) info, "left" .= left]
    ResponseOutsideTx net info -> typed "outsidetx" ["network" .= net.name, "tx" .= marshalValue net info]
    ResponseSentTx net txid -> typed "senttx" ["network" .= net.name, "txid" .= txid]
    ResponseDeleteTx nosigH coins addrs ->
      typed "deletetx" ["nosighash" .= nosigH, "freedcoins" .= coins, "freedaddrs" .= addrs]
    ResponseCoins acc coins -> typed "coins" ["account" .= acc, "coins" .= (marshalValue (accountNetwork acc) <$> coins)]
    ResponseSignMsg message signature addr ->
      typed "signmsg" ["message" .= message, "signature" .= signature, "address" .= addr]
    ResponseSync results -> typed "sync" ["syncres" .= (syncResJson <$> results)]
    ResponseRecover net gap results ->
      typed "recover" ["network" .= net.name, "accountgap" .= gap, "syncres" .= (syncResJson <$> results)]
    ResponseVersion version dbVersion -> typed "version" ["version" .= version, "dbversion" .= dbVersion]

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "error" -> ResponseError <$> o .: "error"
      "mnemonic" -> ResponseMnemonic <$> o .: "entropysource" <*> o .: "mnemonic" <*> o .: "splitmnemonic"
      "account" -> uncurry ResponseAccount <$> (parseAccountWithSpendable =<< o .: "account")
      "accresult" -> ResponseAccResult <$> o .: "account" <*> o .: "result" <*> o .: "text"
      "file" -> ResponseFile <$> o .: "file"
      "accounts" -> ResponseAccounts <$> (mapM parseAccountWithSpendable =<< o .: "accounts")
      "address" -> do
        acc <- o .: "account"
        uncurry (ResponseAddress acc) <$> (parseAddressWithSpendable =<< o .: "address")
      "addresses" -> ResponseAddresses <$> o .: "account" <*> (mapM parseAddressWithSpendable =<< o .: "addresses")
      "balances" -> ResponseBalances <$> o .: "account" <*> (mapM parseAddressWithCoinBalance =<< o .: "addresses")
      "txs" -> do
        acc <- o .: "account"
        ResponseTxs acc <$> (mapM (unmarshalValue (accountNetwork acc)) =<< o .: "txs")
      "tx" -> do
        acc <- o .: "account"
        ResponseTx acc <$> (unmarshalValue (accountNetwork acc) =<< o .: "tx")
      "pendingtxs" -> ResponsePendingTxs <$> o .: "account" <*> (mapM heldTx =<< o .: "txs")
      "prepared" -> do
        acc <- o .: "account"
        ResponsePrepared acc <$> (unmarshalValue (accountNetwork acc) =<< o .: "tx") <*> o .: "left"
      "outsidetx" -> do
        net <- network o
        ResponseOutsideTx net <$> (unmarshalValue net =<< o .: "tx")
      "senttx" -> ResponseSentTx <$> network o <*> o .: "txid"
      "deletetx" -> ResponseDeleteTx <$> o .: "nosighash" <*> o .: "freedcoins" <*> o .: "freedaddrs"
      "coins" -> do
        acc <- o .: "account"
        ResponseCoins acc <$> (mapM (unmarshalValue (accountNetwork acc)) =<< o .: "coins")
      "signmsg" -> ResponseSignMsg <$> o .: "message" <*> o .: "signature" <*> o .: "address"
      "sync" -> ResponseSync <$> (mapM parseSyncRes =<< o .: "syncres")
      "recover" -> ResponseRecover <$> network o <*> o .: "accountgap" <*> (mapM parseSyncRes =<< o .: "syncres")
      "version" -> ResponseVersion <$> o .: "version" <*> o .: "dbversion"
      _ -> fail "Invalid JSON response type"
    where
      network o = maybe (fail "Unknown network") return . netByName =<< o .: "network"
      -- A pending transaction with its account, which names its network
      heldTx = withObject "pending transaction" $ \entry -> do
        acc <- entry .: "account"
        (,) acc <$> (unmarshalValue (accountNetwork acc) =<< entry .: "tx")

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
