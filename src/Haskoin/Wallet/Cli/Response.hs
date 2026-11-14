{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The answer of hw to a command, which the pretty printer shows to the
-- user and --json writes for scripts. In JSON, an answer is an object that
-- names its type, and reads back as the answer it was.
--
-- An answer says where what it shows is: the wallet and the portfolio by
-- their indexes, the network, and for an address the type of its account
-- ('PortfolioRef', 'Place', 'AccountRef'). A listing that covers a whole
-- portfolio comes in groups ('Group'), one for each network, or for each
-- account for the addresses: each group says where it is, then lists its
-- rows.
--
-- An answer about one address, one coin or one transaction carries the
-- active portfolio too. A command given one of them finds its portfolio
-- by itself, whichever is active, so the answer may be about
-- another one. The screen then says so, since the paths and the labels it
-- shows are of that portfolio, and names that portfolio after each path.
-- The listing of the pending transactions carries it for the same
-- reason: it covers every portfolio. JSON leaves the active portfolio
-- out: the answer names its own portfolio, and portfolios names the
-- active one. An answer read back from JSON has none, and the screen then
-- says nothing of it.
--
-- An answer does not say whether the screen names the wallet of a
-- portfolio ('Haskoin.Wallet.Database.Portfolio.WalletNaming'). That
-- follows from the wallets of the database, the same for every answer,
-- so the pretty printer is given it apart, as it is given the unit of
-- the amounts.
module Haskoin.Wallet.Cli.Response
  ( Response (..),
    NewSecret (..),
    Group (..),
    AccountCounts (..),
    groupAccountRef,
    responseFailed,
    joinListings,
  )
where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Database (DBAddress)
import Haskoin.Wallet.Database.Account (PortfolioView)
import Haskoin.Wallet.Database.Address (CoinBalance (..), addressWithCoinBalance, addressWithSpendable, parseAddressWithCoinBalance, parseAddressWithSpendable)
import Haskoin.Wallet.Database.Coin (JsonCoin, LeftCoin)
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), ActiveAfter, Place (..), PortfolioRef (..), accountRefNetwork, parsePlace, parsePortfolioRef, placePairs, portfolioRefPairs)
import Haskoin.Wallet.Database.TxInfo (ListedTx, TxInfo)
import Haskoin.Wallet.Keys (AccountKind, foreignKindName, foreignKindNamed)
import Haskoin.Wallet.Labels (FileShown, ImportReport, fileShownPairs, parseFileShown, parseReport, reportPairs)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..), foreignPath)
import Haskoin.Wallet.Tx.FeeRates (FeeRates)
import Numeric.Natural (Natural)

-- | The secret newportfolio made and shows once: a mnemonic, or the shares
-- of a backup. hw keeps neither.
data NewSecret
  = -- | A new mnemonic: the source of its entropy, and its words
    NewMnemonic !Text ![Text]
  | -- | The shares of a new backup: the source of its entropy, the number
    -- of shares that rebuild the seed, and the words of each share
    NewShares !Text !Natural ![[Text]]
  deriving (Eq, Show)

-- | An account of a group of a listing: its type, and how many addresses
-- it has on its receiving chain and on its change chain. The screen pads
-- the paths of a listing to the longest path these give, the same for
-- every page of it.
data AccountCounts = AccountCounts
  { countsKind :: !AccountKind,
    countsExternal :: !Natural,
    countsInternal :: !Natural
  }
  deriving (Eq, Show)

-- | A group of a listing: the rows of a portfolio on one network. addresses
-- lists the addresses of one account in a group, and balances, coins and
-- txs what both accounts of the network hold together. The group says
-- where it is and which accounts it covers, then lists its rows.
data Group row = Group
  { groupPlace :: !Place,
    -- | The accounts the group covers, BIP44 first: one for a group of
    -- addresses, or for a listing narrowed to one type
    groupAccounts :: ![AccountCounts],
    groupRows :: ![row]
  }
  deriving (Eq, Show)

-- | The network of a group
groupNetwork :: Group row -> Network
groupNetwork = placeNetwork . groupPlace

-- | The account of a group of addresses, which covers one
groupAccountRef :: Group row -> Maybe AccountRef
groupAccountRef g = case groupAccounts g of
  [only] -> Just (AccountRef (groupPlace g) (countsKind only))
  _ -> Nothing

-- | The answer of a command: what it did, or why it did nothing, with
-- where it is. Each answer reads back from its JSON.
data Response
  = -- | The command failed, and why
    ResponseError !Text
  | -- | What newportfolio did: the secret it made, when it made one, the
    -- portfolio it created, and the active portfolio after it, which is
    -- the new one in a database that had none
    ResponseNewPortfolio !(Maybe NewSecret) !PortfolioView !ActiveAfter
  | -- | A portfolio as it is now, after a command that changed it
    ResponsePortfolio !PortfolioView
  | -- | The portfolio switch made the active one
    ResponseSwitched !PortfolioView
  | -- | Whether a mnemonic matched a portfolio, and what hw says of it
    ResponseChecked !PortfolioRef !Bool !Text
  | -- | Something deleted, a portfolio or a wallet: what hw says of it
    ResponseDeleted !Text
  | -- | The defaults setdefault wrote to the configuration file: each key
    -- with its new value, and the file
    ResponseDefaults ![(Text, Text)] !FilePath
  | -- | The file the command wrote
    ResponseFile !FilePath
  | -- | The two files of a backup in the user-backups directory: the copy
    -- of the database, and its labels as exportwallet writes them
    ResponseBackup !FilePath !FilePath
  | -- | What importwallet did with a file: the wallets and the accounts it
    -- names, the labels and locks set, and what it could not place; then
    -- the active portfolio after it, if the database has one
    ResponseImport !ImportReport !(Maybe ActiveAfter)
  | -- | A file of a wallet as show shows it, before it is imported: each
    -- wallet and each account as importwallet would leave it, with its
    -- labels and locks, and the rest of the file. The database keeps
    -- nothing of it.
    ResponseWalletFile !FileShown
  | -- | Portfolios, each with its figures on each network. The JSON gives
    -- each account with its output descriptor.
    ResponsePortfolios ![PortfolioView]
  | -- | An address of an account, with the balance it can spend now, and
    -- the active portfolio, which may be another one
    ResponseAddress !AccountRef !DBAddress !Word64 !(Maybe PortfolioRef)
  | -- | Addresses, grouped by account, each with the balance it can spend
    -- now
    ResponseAddresses ![Group (DBAddress, Word64)]
  | -- | The addresses that hold coins, grouped by network, each with the
    -- balance of its coins. Each group comes with the total of its
    -- network, which the listing gives before the addresses. The total
    -- counts every coin the listing covers on the network, whatever part
    -- of them a page shows.
    ResponseBalances ![(Group (DBAddress, CoinBalance), CoinBalance)]
  | -- | The address of an account at a path of one of its chains, derived
    -- from the key of the account, which the wallet does not hold: what
    -- show answers for such a path. The address is cut, its middle hidden
    -- ("Haskoin.Wallet.CutAddress"), in JSON too, so that no script pays
    -- it either. JSON gives it as "cutaddress", since it is no address.
    ResponseDerivedAddr !AccountRef !SoftPath !Text
  | -- | Transactions, grouped by network, as the portfolio sees them
    ResponseTxs ![Group ListedTx]
  | -- | Pending transactions, each with where it is, and the active
    -- portfolio. The listing covers every portfolio, and the screen names
    -- the portfolio after the paths of a transaction of another one than
    -- the active one.
    ResponsePendingTxs !(Maybe PortfolioRef) ![(Place, TxInfo)]
  | -- | A transaction of a portfolio on a network, and the active
    -- portfolio, which may be another one: a transfer between two
    -- portfolios is of the one that pays it
    ResponseTx !Place !TxInfo !(Maybe PortfolioRef)
  | -- | A transaction just labelled, as each portfolio that holds it sees
    -- it: the portfolio of a pending one, or every portfolio whose history
    -- has it. Then the active portfolio, which may be none of them.
    ResponseLabelledTxs ![(Place, TxInfo)] !(Maybe PortfolioRef)
  | -- | A transaction as each portfolio that holds it sees it, as show
    -- answers: the portfolio of a pending one, or every portfolio whose
    -- history has it, such as both ends of a payment between two
    -- portfolios. Then the active portfolio, which may be none of them.
    ResponseHeldTxs ![(Place, TxInfo)] !(Maybe PortfolioRef)
  | -- | A transaction just prepared by preparetx, preparesendall or
    -- bumpfee, with the coins it leaves in the portfolio and why: the
    -- coins that coin control chose and could not spend, and for a
    -- send-all every coin it does not send. Then the active portfolio. A
    -- replacement is of the portfolio of the transaction it replaces,
    -- which may be another one.
    ResponsePrepared !Place !TxInfo ![LeftCoin] !(Maybe PortfolioRef)
  | -- | A transaction outside the wallet, a sweep between outside
    -- addresses, on its network
    ResponseOutsideTx !Network !TxInfo
  | -- | A transaction the wallet does not hold, sent on its network
    ResponseSentTx !Network !TxHash
  | -- | A pending transaction deleted, by its nosigHash and its id if it
    -- was signed, with the number of coins and of addresses it held
    ResponseDeleteTx !TxHash !(Maybe TxHash) !Natural !Natural
  | -- | Coins, grouped by network
    ResponseCoins ![Group JsonCoin]
  | -- | One coin with every detail, with the account that holds it, as
    -- show, lockcoin and unlockcoin answer, and the active portfolio,
    -- which may be another one: a coin tells where it is. It is an answer
    -- of its own, rather than a listing of one coin, so that it says its
    -- account, as the answer of an address does.
    ResponseCoin !AccountRef !JsonCoin !(Maybe PortfolioRef)
  | -- | A signed message, its signature in base64, and the address that
    -- signed it
    ResponseSignMsg !Text !Text !Text
  | -- | A message, a signature in base64, the address it is said to be
    -- signed from, and whether it is
    ResponseVerifyMsg !Text !Text !Text !Bool
  | -- | The sync of each portfolio on each network synced, the networks
    -- the host does not serve, the networks not synced, each with the
    -- failure of its store, and the active portfolio, if the database has
    -- one. A sync covers every portfolio. txs, a step after it, lists the
    -- active portfolio alone, so new transactions in another portfolio
    -- need a switch first. pendingtxs lists every portfolio, so a pending
    -- transaction in conflict needs none.
    ResponseSync ![SyncRes] ![Network] ![(Network, Text)] !(Maybe PortfolioRef)
  | -- | What recover found of a seed. In order: the networks searched;
    -- the gap of portfolios the search ended on; the wallet of the seed,
    -- if it has one now; the indexes of the portfolios created; the sync
    -- of each portfolio of the wallet on each network searched; the funds
    -- of the seed at the
    -- accounts hw does not hold, each with its network; the networks the
    -- host does not serve; the networks left out, each with the failure
    -- of its store; and the active portfolio after it, if the database has
    -- one.
    ResponseRecover ![Network] !Natural !(Maybe Natural) ![Natural] ![SyncRes] ![(Network, ForeignFunds)] ![Network] ![(Network, Text)] !(Maybe ActiveAfter)
  | -- | The fee rates recent blocks paid, on the network asked
    ResponseFees !Network !FeeRates
  deriving (Eq, Show)

-- | Two answers of one listing as one: the groups of the second after
-- those of the first. A listing that covers a whole portfolio is read
-- group by group, and --json writes it as one answer. Any other pair
-- gives the first.
joinListings :: Response -> Response -> Response
joinListings (ResponseAddresses a) (ResponseAddresses b) = ResponseAddresses (a <> b)
joinListings (ResponseBalances a) (ResponseBalances b) = ResponseBalances (a <> b)
joinListings (ResponseTxs a) (ResponseTxs b) = ResponseTxs (a <> b)
joinListings (ResponseCoins a) (ResponseCoins b) = ResponseCoins (a <> b)
joinListings (ResponsePendingTxs active a) (ResponsePendingTxs _ b) = ResponsePendingTxs active (a <> b)
joinListings (ResponsePortfolios a) (ResponsePortfolios b) = ResponsePortfolios (a <> b)
joinListings first _ = first

{- JSON -}

-- | An answer in JSON: an object naming its type, with its fields
typed :: Text -> [Pair] -> Value
typed kind fields = object (("type" .= kind) : fields)

instance ToJSON NewSecret where
  toJSON = \case
    NewMnemonic source ws -> object ["entropysource" .= source, "mnemonic" .= ws]
    NewShares source threshold shares -> object ["entropysource" .= source, "threshold" .= threshold, "shares" .= shares]

instance FromJSON NewSecret where
  parseJSON = withObject "new secret" $ \o -> do
    sharesM <- o .:? "shares"
    case sharesM of
      Just shares -> NewShares <$> o .: "entropysource" <*> o .: "threshold" <*> return shares
      Nothing -> NewMnemonic <$> o .: "entropysource" <*> o .: "mnemonic"

instance ToJSON AccountCounts where
  toJSON counts = object ["type" .= countsKind counts, "external" .= countsExternal counts, "internal" .= countsInternal counts]

instance FromJSON AccountCounts where
  parseJSON = withObject "account of a group" $ \o -> AccountCounts <$> o .: "type" <*> o .: "external" <*> o .: "internal"

-- | The keys of a group, without its rows: where it is, and its accounts
groupPairs :: Group row -> [Pair]
groupPairs g = placePairs (groupPlace g) <> ["accounts" .= groupAccounts g]

-- | A group back from its keys, with its rows read by the function given
-- its network
parseGroup :: (Network -> Object -> Parser [row]) -> Object -> Parser (Group row)
parseGroup rowsOf o = do
  place <- parsePlace o
  Group place <$> o .: "accounts" <*> rowsOf (placeNetwork place) o

-- | The balance of the coins of a network, as the JSON of balances gives
-- it for a group
coinBalancePairs :: CoinBalance -> [Pair]
coinBalancePairs balance =
  [ "total" .= coinBalanceTotal balance,
    "spendable" .= coinBalanceSpendable balance,
    "lockedbytx" .= coinBalanceLockedByTx balance,
    "lockedbyuser" .= coinBalanceLockedByUser balance
  ]

-- | The balance of the coins of a network back from the keys
-- 'coinBalancePairs' writes
parseCoinBalance :: Object -> Parser CoinBalance
parseCoinBalance o = CoinBalance <$> o .: "total" <*> o .: "spendable" <*> o .: "lockedbytx" <*> o .: "lockedbyuser"

instance ToJSON Response where
  toJSON = \case
    ResponseError err -> typed "error" ["error" .= err]
    ResponseNewPortfolio secretM view active ->
      typed "newportfolio" ["secret" .= secretM, "portfolio" .= view, "activeportfolio" .= active]
    ResponsePortfolio view -> typed "portfolio" ["portfolio" .= view]
    ResponseSwitched view -> typed "switch" ["portfolio" .= view]
    ResponseChecked ref result text -> typed "checked" (portfolioRefPairs ref <> ["result" .= result, "text" .= text])
    ResponseDeleted text -> typed "deleted" ["text" .= text]
    ResponseDefaults defaults file ->
      typed "defaults" ["defaults" .= [object ["key" .= key, "value" .= value] | (key, value) <- defaults], "file" .= file]
    ResponseFile file -> typed "file" ["file" .= file]
    ResponseBackup database labels -> typed "backup" ["database" .= database, "labels" .= labels]
    ResponseImport report active -> typed "import" (reportPairs report <> ["activeportfolio" .= active])
    ResponseWalletFile shown -> typed "walletfile" (fileShownPairs shown)
    ResponsePortfolios views -> typed "portfolios" ["portfolios" .= views]
    ResponseAddress ref addr spendable _ ->
      typed "address" ["account" .= ref, "address" .= addressWithSpendable (accountRefNetwork ref) addr spendable]
    ResponseAddresses groups ->
      typed "addresses" ["groups" .= [object (groupPairs g <> ["addresses" .= (uncurry (addressWithSpendable (groupNetwork g)) <$> groupRows g)]) | g <- groups]]
    ResponseBalances groups ->
      typed
        "balances"
        [ "groups"
            .= [ object (groupPairs g <> ["total" .= object (coinBalancePairs total), "addresses" .= (uncurry (addressWithCoinBalance (groupNetwork g)) <$> groupRows g)])
                 | (g, total) <- groups
               ]
        ]
    ResponseDerivedAddr ref path cut ->
      typed "derivedaddr" ["account" .= ref, "derivation" .= path, "cutaddress" .= cut]
    ResponseTxs groups ->
      typed "txs" ["groups" .= [object (groupPairs g <> ["txs" .= (marshalValue (placeNetwork (groupPlace g)) <$> groupRows g)]) | g <- groups]]
    -- The active portfolio is left out, as in the answers about one thing
    ResponsePendingTxs _ held -> typed "pendingtxs" ["txs" .= (heldTxJson <$> held)]
    -- The active portfolio is left out of the answers about one thing:
    -- see the module header
    ResponseTx place info _ -> typed "tx" (placePairs place <> ["tx" .= marshalValue (placeNetwork place) info])
    ResponseLabelledTxs held _ -> typed "labelledtxs" ["txs" .= (heldTxJson <$> held)]
    ResponseHeldTxs held _ -> typed "heldtxs" ["txs" .= (heldTxJson <$> held)]
    ResponsePrepared place info left _ ->
      typed "prepared" (placePairs place <> ["tx" .= marshalValue (placeNetwork place) info, "left" .= (marshalValue (placeNetwork place) <$> left)])
    ResponseOutsideTx net info -> typed "outsidetx" ["network" .= net.name, "tx" .= marshalValue net info]
    ResponseSentTx net txid -> typed "senttx" ["network" .= net.name, "txid" .= txid]
    ResponseDeleteTx nosigH txidM coins addrs ->
      typed "deletetx" ["nosighash" .= nosigH, "txid" .= txidM, "freedcoins" .= coins, "freedaddrs" .= addrs]
    ResponseCoins groups ->
      typed "coins" ["groups" .= [object (groupPairs g <> ["coins" .= (marshalValue (placeNetwork (groupPlace g)) <$> groupRows g)]) | g <- groups]]
    ResponseCoin ref coin _ ->
      typed "coin" ["account" .= ref, "coin" .= marshalValue (accountRefNetwork ref) coin]
    ResponseSignMsg message signature addr ->
      typed "signmsg" ["message" .= message, "signature" .= signature, "address" .= addr]
    ResponseVerifyMsg message signature addr valid ->
      typed "verifymsg" ["message" .= message, "signature" .= signature, "address" .= addr, "valid" .= valid]
    ResponseSync results notServed notSynced activeM ->
      typed
        "sync"
        [ "syncres" .= (syncResJson <$> results),
          "notserved" .= ((.name) <$> notServed),
          "notsynced" .= (failedNetworkJson <$> notSynced),
          "activeportfolio" .= activeM
        ]
    ResponseRecover searched gap walletM created results elsewhere notServed notSearched active ->
      typed
        "recover"
        [ "networks" .= ((.name) <$> searched),
          "portfoliogap" .= gap,
          "wallet" .= walletM,
          "portfolios" .= created,
          "syncres" .= (syncResJson <$> results),
          "unspendable" .= (uncurry foreignFundsJson <$> elsewhere),
          "notserved" .= ((.name) <$> notServed),
          "notsearched" .= (failedNetworkJson <$> notSearched),
          "activeportfolio" .= active
        ]
    ResponseFees net rates -> typed "fees" ["network" .= net.name, "feerates" .= rates]

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "error" -> ResponseError <$> o .: "error"
      "newportfolio" -> ResponseNewPortfolio <$> o .:? "secret" <*> o .: "portfolio" <*> o .: "activeportfolio"
      "portfolio" -> ResponsePortfolio <$> o .: "portfolio"
      "switch" -> ResponseSwitched <$> o .: "portfolio"
      "checked" -> ResponseChecked <$> parsePortfolioRef o <*> o .: "result" <*> o .: "text"
      "deleted" -> ResponseDeleted <$> o .: "text"
      "defaults" -> ResponseDefaults <$> (mapM keyValue =<< o .: "defaults") <*> o .: "file"
      "file" -> ResponseFile <$> o .: "file"
      "backup" -> ResponseBackup <$> o .: "database" <*> o .: "labels"
      "import" -> ResponseImport <$> parseReport o <*> o .:? "activeportfolio"
      "walletfile" -> ResponseWalletFile <$> parseFileShown o
      "portfolios" -> ResponsePortfolios <$> o .: "portfolios"
      "address" -> do
        ref <- o .: "account"
        (addr, spendable) <- parseAddressWithSpendable =<< o .: "address"
        return (ResponseAddress ref addr spendable activeLeftOut)
      "addresses" -> ResponseAddresses <$> (mapM (withObject "group" (parseGroup (\_ g -> mapM parseAddressWithSpendable =<< g .: "addresses"))) =<< o .: "groups")
      "balances" -> ResponseBalances <$> (mapM balanceGroup =<< o .: "groups")
      "derivedaddr" -> ResponseDerivedAddr <$> o .: "account" <*> o .: "derivation" <*> o .: "cutaddress"
      "txs" -> ResponseTxs <$> (mapM (marshalled "txs") =<< o .: "groups")
      "tx" -> do
        place <- parsePlace o
        info <- unmarshalValue (placeNetwork place) =<< o .: "tx"
        return (ResponseTx place info activeLeftOut)
      "pendingtxs" -> ResponsePendingTxs activeLeftOut <$> (mapM heldTx =<< o .: "txs")
      "labelledtxs" -> (`ResponseLabelledTxs` activeLeftOut) <$> (mapM heldTx =<< o .: "txs")
      "heldtxs" -> (`ResponseHeldTxs` activeLeftOut) <$> (mapM heldTx =<< o .: "txs")
      "prepared" -> do
        place <- parsePlace o
        info <- unmarshalValue (placeNetwork place) =<< o .: "tx"
        left <- mapM (unmarshalValue (placeNetwork place)) =<< o .: "left"
        return (ResponsePrepared place info left activeLeftOut)
      "outsidetx" -> do
        net <- network o
        ResponseOutsideTx net <$> (unmarshalValue net =<< o .: "tx")
      "senttx" -> ResponseSentTx <$> network o <*> o .: "txid"
      "deletetx" -> ResponseDeleteTx <$> o .: "nosighash" <*> o .:? "txid" <*> o .: "freedcoins" <*> o .: "freedaddrs"
      "coins" -> ResponseCoins <$> (mapM (marshalled "coins") =<< o .: "groups")
      "coin" -> do
        ref <- o .: "account"
        coin <- unmarshalValue (accountRefNetwork ref) =<< o .: "coin"
        return (ResponseCoin ref coin activeLeftOut)
      "signmsg" -> ResponseSignMsg <$> o .: "message" <*> o .: "signature" <*> o .: "address"
      "verifymsg" -> ResponseVerifyMsg <$> o .: "message" <*> o .: "signature" <*> o .: "address" <*> o .: "valid"
      "sync" ->
        ResponseSync
          <$> (mapM parseSyncRes =<< o .: "syncres")
          <*> (mapM networkNamed =<< o .: "notserved")
          <*> (mapM parseFailedNetwork =<< o .: "notsynced")
          <*> o .:? "activeportfolio"
      "recover" ->
        ResponseRecover
          <$> (mapM networkNamed =<< o .: "networks")
          <*> o .: "portfoliogap"
          <*> o .:? "wallet"
          <*> o .: "portfolios"
          <*> (mapM parseSyncRes =<< o .: "syncres")
          <*> (mapM parseForeignFunds =<< o .: "unspendable")
          <*> (mapM networkNamed =<< o .: "notserved")
          <*> (mapM parseFailedNetwork =<< o .: "notsearched")
          <*> o .:? "activeportfolio"
      "fees" -> ResponseFees <$> network o <*> o .: "feerates"
      _ -> fail "Invalid JSON response type"
    where
      -- The JSON of an answer about one thing leaves out the active
      -- portfolio: see the module header
      activeLeftOut = Nothing
      network o = networkNamed =<< o .: "network"
      keyValue :: (FromJSON v) => Value -> Parser (Text, v)
      keyValue = withObject "default" $ \entry -> (,) <$> entry .: "key" <*> entry .: "value"
      -- A transaction with where it is, which names its network
      heldTx = withObject "transaction of a portfolio" $ \entry -> do
        place <- parsePlace entry
        (,) place <$> (unmarshalValue (placeNetwork place) =<< entry .: "tx")
      -- A group of a listing whose rows are read on its network
      marshalled :: (MarshalJSON Network row) => Data.Aeson.Key -> Value -> Parser (Group row)
      marshalled key = withObject "group of a listing" (parseGroup (\net g -> mapM (unmarshalValue net) =<< g .: key))
      balanceGroup = withObject "balances of a network" $ \entry ->
        (,)
          <$> parseGroup (\_ g -> mapM parseAddressWithCoinBalance =<< g .: "addresses") entry
          <*> (parseCoinBalance =<< entry .: "total")

-- | The network of a name in JSON
networkNamed :: String -> Parser Network
networkNamed = maybe (fail "Unknown network") return . netByName

-- | A transaction with where it is in JSON, in a list of transactions of
-- several portfolios
heldTxJson :: (Place, TxInfo) -> Value
heldTxJson (place, info) = object (placePairs place <> ["tx" .= marshalValue (placeNetwork place) info])

-- | The sync of a portfolio on a network in JSON, with what it holds and
-- can spend now
syncResJson :: SyncRes -> Value
syncResJson res =
  object $
    placePairs (syncResPlace res)
      <> [ "balance"
             .= object
               [ "confirmed" .= syncResConfirmed res,
                 "unconfirmed" .= syncResUnconfirmed res,
                 "coins" .= syncResCoins res,
                 "spendable" .= syncResSpendable res
               ],
           "bestblock" .= syncResBlockHash res,
           "bestheight" .= syncResBlockHeight res,
           "txupdates" .= syncResTxUpdates res,
           "coinupdates" .= syncResCoinUpdates res,
           "rebroadcast" .= syncResRebroadcast res,
           "notpublished" .= [object ["txid" .= txid, "reason" .= why] | (txid, why) <- syncResNotPublished res],
           "conflicted" .= syncResConflicted res,
           "discovered" .= syncResNewAddrs res
         ]

parseSyncRes :: Value -> Parser SyncRes
parseSyncRes =
  withObject "syncres" $ \o -> do
    place <- parsePlace o
    balance <- o .: "balance"
    SyncRes place
      <$> balance .: "confirmed"
      <*> balance .: "unconfirmed"
      <*> balance .: "coins"
      <*> balance .: "spendable"
      <*> o .: "bestblock"
      <*> o .: "bestheight"
      <*> o .: "txupdates"
      <*> o .: "coinupdates"
      <*> o .: "rebroadcast"
      <*> (mapM notPublished =<< o .: "notpublished")
      <*> o .: "conflicted"
      <*> o .: "discovered"

-- | A transaction the store did not publish back from its JSON, with what
-- the store said of it, as 'syncResJson' writes it
notPublished :: Value -> Parser (TxHash, String)
notPublished = withObject "transaction not published" $ \o -> (,) <$> o .: "txid" <*> o .: "reason"

-- | A network that was left out in JSON, with the failure of its store
failedNetworkJson :: (Network, Text) -> Value
failedNetworkJson (net, failure) = object ["network" .= net.name, "error" .= failure]

-- | A network left out back from its JSON, as 'failedNetworkJson' writes it
parseFailedNetwork :: Value -> Parser (Network, Text)
parseFailedNetwork = withObject "network left out" $ \o -> (,) <$> (networkNamed =<< o .: "network") <*> o .: "error"

-- | The funds of a seed at an account hw does not hold, in JSON: the
-- network, the kind of the account, its path on the network and its
-- index, how many of its addresses were used, and what they hold in
-- satoshis. The network is needed to read the path: m/44'/0' is an
-- account of Bitcoin on Bitcoin Cash too.
foreignFundsJson :: Network -> ForeignFunds -> Value
foreignFundsJson net funds =
  object
    [ "network" .= net.name,
      "type" .= foreignKindName (foreignKind funds),
      "path" .= pathToStr (foreignPath net funds),
      "index" .= foreignIndex funds,
      "addresses" .= foreignUsed funds,
      "balance" .= foreignBalance funds
    ]

-- | The funds back from their JSON, with their network: the path follows
-- from the network, the kind and the index, so it is not read
parseForeignFunds :: Value -> Parser (Network, ForeignFunds)
parseForeignFunds =
  withObject "unspendable" $ \o -> do
    net <- networkNamed =<< o .: "network"
    kind <- maybe (fail "Unknown kind of account") return . foreignKindNamed =<< o .: "type"
    (,) net <$> (ForeignFunds kind <$> o .: "index" <*> o .: "addresses" <*> o .: "balance")

-- | Whether the answer is a failure, which hw exits with: an error, a
-- signature found not valid, a mnemonic that does not match the
-- portfolio, a sync that left a network out because its store is down, or
-- a recover that did, so that a script can tell from the exit code. A
-- network the host does not serve is no failure.
--
-- A check that says no is a failure, as for verifymsg: `hw checkmnemonic
-- && …` must not go on with a mnemonic that would not spend the funds.
-- The answer still says what was checked, and --json still gives it.
responseFailed :: Response -> Bool
responseFailed (ResponseError _) = True
responseFailed (ResponseVerifyMsg _ _ _ valid) = not valid
responseFailed (ResponseChecked _ matched _) = not matched
responseFailed (ResponseSync _ _ notSynced _) = not (null notSynced)
responseFailed (ResponseRecover _ _ _ _ _ _ _ notSearched _) = not (null notSearched)
responseFailed _ = False
