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
-- (the types t'PortfolioRef', t'Place' and t'AccountRef'). A listing that
-- covers a whole portfolio comes in groups (the type t'Group'), one for
-- each network, or for each account for the addresses: each group says
-- where it is, then lists its rows.
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
-- An answer about the address book names no portfolio: a contact is of
-- the whole database ("Haskoin.Wallet.Contacts"). It names the network
-- of the contact, which the address alone does not always tell.
--
-- An answer does not say whether the screen names the wallet of a portfolio
-- (the type t'Haskoin.Wallet.Database.Portfolio.WalletNaming'). That
-- follows from the wallets of the database, the same for every answer, so
-- the pretty printer is given it apart, as it is given the unit of the
-- amounts.
module Haskoin.Wallet.Cli.Response
  ( Response (..),
    NewSecret (..),
    ActiveShown (..),
    JournalSwitch (..),
    BackupFiles (..),
    Group (..),
    ChangeFollowed (..),
    AccountCounts (..),
    groupAccountRef,
    responseFailed,
    joinListings,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Pair, Parser)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Backup (BackupsShown, backupsShownPairs, parseBackupsShown)
import Haskoin.Wallet.Database (DBAddress)
import Haskoin.Wallet.Database.Account (PortfolioView)
import Haskoin.Wallet.Database.Address (CoinBalance (..), addressWithCoinBalance, addressWithSpendable, parseAddressWithCoinBalance, parseAddressWithSpendable)
import Haskoin.Wallet.Database.Coin (JsonCoin, LeftCoin)
import Haskoin.Wallet.DatabaseFile (DatabaseShown, databaseShownPairs, parseDatabaseShown)
import Haskoin.Wallet.Contacts (Contact)
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse)
import Haskoin.Wallet.Database.PendingTx (Retag (..))
import Haskoin.Wallet.Database.Portfolio (AccountRef (..), ActiveAfter, Place (..), PortfolioRef (..), accountRefNetwork, parsePlace, parsePortfolioRef, placePairs, portfolioRefPairs)
import Haskoin.Wallet.Database.TxInfo (ListedTx, TxInfo)
import Haskoin.Wallet.Keys (AccountKind, foreignKindName, foreignKindNamed)
import Haskoin.Wallet.Labels (FileShown, ImportReport, fileShownPairs, parseFileShown, parseReport, reportPairs)
import Haskoin.Wallet.Sync (ForeignFunds (..), SyncRes (..), foreignPath)
import Haskoin.Wallet.Tags (ShownTag, Tag)
import Haskoin.Wallet.Tx.FeeRates (FeeRates)
import Haskoin.Wallet.Tx.Sweep (UnsweptCoin (..), UnsweptReason (..))
import Haskoin.Wallet.Util (hidingSecKeys)
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

-- | What showdefault says of the active portfolio, after the keys of the
-- configuration file. The active portfolio is in the database, not in
-- the file, so it is an answer of its own beside the keys.
data ActiveShown
  = -- | Nothing: the options asked for keys of the file alone
    ActiveNotAsked
  | -- | The active portfolio, named with its wallet, for --portfolio or
    -- for no option at all. Nothing when the database has no portfolio.
    ActivePortfolioShown !(Maybe PortfolioRef)
  | -- | The index of the wallet of the active portfolio, for --wallet
    -- without --portfolio. Nothing when the database has no portfolio.
    ActiveWalletShown !(Maybe Natural)
  deriving (Eq, Show)

-- | What setdefault changed about the journal hw keeps for ledger next to
-- the database, when --journal or --no-journal was given. The answer
-- names the file, since the user includes it in a journal of their own
-- by its path.
data JournalSwitch = JournalSwitch
  { -- | Whether hw keeps the journal from now on. hw removes the file
    -- when it stops keeping it.
    journalKept :: !Bool,
    -- | The file of the journal
    journalPath :: !FilePath
  }
  deriving (Eq, Show)

instance ToJSON JournalSwitch where
  toJSON switched = object ["kept" .= journalKept switched, "file" .= journalPath switched]

instance FromJSON JournalSwitch where
  parseJSON = withObject "JournalSwitch" $ \o -> JournalSwitch <$> o .: "kept" <*> o .: "file"

-- | The two files of a backup: the copy of the database, and its labels
-- as exportwallet writes them. hw makes them by itself in the
-- auto-backups directory before a command loses something, and the
-- answer of that command names them, so that the user knows where what
-- was lost still is.
data BackupFiles = BackupFiles
  { backupDatabase :: !FilePath,
    backupLabels :: !FilePath
  }
  deriving (Eq, Show)

instance ToJSON BackupFiles where
  toJSON files = object ["database" .= backupDatabase files, "labels" .= backupLabels files]

instance FromJSON BackupFiles where
  parseJSON = withObject "backup files" $ \o -> BackupFiles <$> o .: "database" <*> o .: "labels"

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
-- | The change addresses that followed an address tag or untag was given
-- with --with-change: those down the chain of its payments that took the
-- tags with it, or lost them ("Haskoin.Wallet.Database.ChangeChain"). The
-- answer names them, since the command changed addresses the user did not
-- type. There may be none, when no payment spent from the address, or
-- when the change of each stays as it is.
data ChangeFollowed = ChangeFollowed
  { -- | Whether they took tags or lost some
    followedRetag :: !Retag,
    -- | Each as it is now, with the balance it can spend and its tags,
    -- in the order of their paths
    followedRows :: ![(DBAddress, Word64, [ShownTag])]
  }
  deriving (Eq, Show)

-- | The change addresses that followed, in the JSON of an answer about
-- an address: under "changetagged" when they took tags, and under
-- "changeuntagged" when they lost some, each as addresses gives an
-- address. An answer has one of the two keys at most, and neither
-- without --with-change.
changeFollowedPairs :: Network -> ChangeFollowed -> [Pair]
changeFollowedPairs net followed =
  [changeFollowedKey followed.followedRetag .= [addressWithSpendable net addr spendable tags | (addr, spendable, tags) <- followed.followedRows]]

-- | The key of 'changeFollowedPairs' for what the command did
changeFollowedKey :: Retag -> Key.Key
changeFollowedKey = \case
  AddTags -> "changetagged"
  RemoveTags -> "changeuntagged"

-- | The change addresses that followed, as 'changeFollowedPairs' writes
-- them, or Nothing when the answer has neither key
parseChangeFollowed :: Object -> Parser (Maybe ChangeFollowed)
parseChangeFollowed o = do
  tagged <- rowsUnder AddTags
  untagged <- rowsUnder RemoveTags
  return (tagged <|> untagged)
  where
    rowsUnder retag = do
      rowsM <- o .:? changeFollowedKey retag
      forM rowsM (fmap (ChangeFollowed retag) . mapM parseAddressWithSpendable)

-- | A group of the rows of a listing that covers a whole portfolio: where
-- the rows are, the accounts they cover, and the rows
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
    -- the new one in a database that had none. Then the warning of
    -- newportfolio --existing, when the secret typed was of no wallet of
    -- a database that has wallets, so that it created one more: the user
    -- may have meant a wallet the database has, and mistyped the
    -- passphrase.
    ResponseNewPortfolio !(Maybe NewSecret) !(Maybe Text) !PortfolioView !ActiveAfter !(Maybe Text)
  | -- | A portfolio as it is now, after a command that changed it
    ResponsePortfolio !PortfolioView
  | -- | The portfolio switch made the active one
    ResponseSwitched !PortfolioView
  | -- | Whether a mnemonic matched a portfolio, and what hw says of it
    ResponseChecked !PortfolioRef !Bool !Text
  | -- | Something deleted, a portfolio or a wallet: what hw says of it,
    -- and the backup hw made of the database before it deleted, unless
    -- the configuration turns the copies of the database off
    ResponseDeleted !Text !(Maybe BackupFiles)
  | -- | The defaults setdefault wrote to the configuration file: each key
    -- with its new value, and the file. Then what it changed about the
    -- journal hw keeps, when it turned it on or off.
    ResponseDefaults ![(Text, Text)] !FilePath !(Maybe JournalSwitch)
  | -- | The defaults showdefault read from the configuration file: each
    -- key asked for with its value, in the order setdefault writes them,
    -- and the file. Then what it says of the active portfolio.
    ResponseShownDefaults ![(Text, Text)] !FilePath !ActiveShown
  | -- | The file the command wrote
    ResponseFile !FilePath
  | -- | The two files of a backup in the user-backups directory: the copy
    -- of the database, and its labels as exportwallet writes them
    ResponseBackup !FilePath !FilePath
  | -- | What restoredatabase did: the copy it read, as the user named it,
    -- what the database holds now, which is what the copy held, and the
    -- backup hw made of the database it replaced. There is no backup
    -- when that database held no wallet, or when the configuration turns
    -- the copies of the database off.
    --
    -- A damaged database has no backup: hw cannot read it. The last
    -- field is then where restoredatabase moved its file
    -- ("Haskoin.Wallet.DatabaseFailure").
    ResponseRestored !FilePath !DatabaseShown !(Maybe BackupFiles) !(Maybe FilePath)
  | -- | The copies hw keeps, as showbackups shows them: how many of each
    -- kind, and the newest of each ("Haskoin.Wallet.Backup")
    ResponseBackups !BackupsShown
  | -- | What importwallet did with a file: the wallets and the accounts it
    -- names, the labels and locks set, and what it could not place; then
    -- the active portfolio after it, if the database has one
    ResponseImport !ImportReport !(Maybe ActiveAfter)
  | -- | A file of a wallet as show shows it, before it is imported: the
    -- file, as the user named it, then each wallet and each account as
    -- importwallet would leave it, with its labels and locks, and the
    -- rest of the file. The database keeps nothing of it. The answer
    -- names the file so that the screen can say which file it shows, and
    -- of which kind: show also takes a database file, and a user with a
    -- backup of each must not take one for the other.
    ResponseWalletFile !FilePath !FileShown
  | -- | A database file as show shows it, before the user puts it in
    -- place of the database: the file, as the user named it, then its
    -- format version, its wallets and their portfolios
    -- ("Haskoin.Wallet.DatabaseFile"). The file stays as it is.
    ResponseDatabaseFile !FilePath !DatabaseShown
  | -- | Portfolios, each with its figures on each network. The JSON gives
    -- each account with its output descriptor.
    ResponsePortfolios ![PortfolioView]
  | -- | An address of an account, with the balance it can spend now,
    -- its tags, and the active portfolio, which may be another one. The
    -- answer of tag and of untag with --with-change also gives the change
    -- addresses that followed the address.
    ResponseAddress !AccountRef !DBAddress !Word64 ![ShownTag] !(Maybe ChangeFollowed) !(Maybe PortfolioRef)
  | -- | Addresses, grouped by account, each with the balance it can spend
    -- now and its tags
    ResponseAddresses ![Group (DBAddress, Word64, [ShownTag])]
  | -- | The addresses that hold coins, grouped by network, each with the
    -- balance of its coins. Each group comes with the total of its
    -- network, which the listing gives before the addresses. The total
    -- counts every coin the listing covers on the network, whatever part
    -- of them a page shows. Each address comes with its tags.
    ResponseBalances ![(Group (DBAddress, CoinBalance, [ShownTag]), CoinBalance)]
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
  | -- | A sweep just prepared by preparesweep, on its network: of the
    -- portfolio it pays, if it pays one, with the active portfolio, which
    -- may be another one. Then the addresses given to sweep from that
    -- hold no coin, and the coins worth less than their fee that the sweep
    -- does not take. The sweep leaves them out, and the user who gave them
    -- would otherwise believe that they were swept.
    ResponseSweep !(Maybe Place) !Network !TxInfo ![Address] ![UnsweptCoin] !(Maybe PortfolioRef)
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
  | -- | Tags of the database, each with its colour and with how many
    -- addresses and transactions have it: what tags lists, and what tag
    -- answers when it only colours a tag
    ResponseTags ![TagUse]
  | -- | A tag renamed in the whole database: the name it had, and the
    -- tag as it is now, with its colour and with how many addresses,
    -- transactions and contacts have it
    ResponseTagRenamed !Tag !TagUse
  | -- | A tag merged into another one: the name of the tag that is gone,
    -- and the tag it was merged into, as it is now. It is an answer of its
    -- own, and not a renamed tag with a mark, so that a script tells the
    -- two apart by the type of the answer.
    ResponseTagMerged !Tag !TagUse
  | -- | A tag deleted from the whole database, with how many addresses,
    -- transactions and contacts had it
    ResponseTagDeleted !Tag !TagDeleted
  | -- | A contact of the address book on its network, as it is now: what
    -- newcontact answers, and label, tag, untag and show for an address
    -- of the address book
    ResponseContact !Network !Contact
  | -- | The contacts of the address book, grouped by network, in the
    -- order contacts lists them
    ResponseContacts ![(Network, [Contact])]
  | -- | A contact deleted from the address book, as it was
    ResponseContactDeleted !Network !Contact
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
joinListings (ResponseContacts a) (ResponseContacts b) = ResponseContacts (a <> b)
joinListings first _ = first

{- JSON -}

-- | An answer in JSON: an object naming its type, with its fields
typed :: Text -> [Pair] -> Value
typed kind fields = object (("type" .= kind) : fields)

-- | The keys of the configuration file in the JSON of setdefault and of
-- showdefault: an object for each key, with its value, in the order of
-- the answer
keyValues :: [(Text, Text)] -> [Value]
keyValues defaults = [object ["key" .= key, "value" .= value] | (key, value) <- defaults]

-- | What showdefault says of the active portfolio, back from its JSON. A
-- key that is there, even null, says what was asked for, so a key left
-- out reads apart from a key set to null ('.:!').
activeShown :: Object -> Parser ActiveShown
activeShown o = do
  portfolioM <- o .:! "activeportfolio"
  walletM <- o .:! "activewallet"
  return $ case (portfolioM, walletM) of
    (Just refM, _) -> ActivePortfolioShown refM
    (Nothing, Just indexM) -> ActiveWalletShown indexM
    (Nothing, Nothing) -> ActiveNotAsked

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
    "lockedbyuser" .= coinBalanceLockedByUser balance,
    "lockedbytag" .= coinBalanceLockedByTag balance,
    "lockedbytokens" .= coinBalanceLockedByTokens balance
  ]

-- | The balance of the coins of a network back from the keys
-- 'coinBalancePairs' writes
parseCoinBalance :: Object -> Parser CoinBalance
parseCoinBalance o = CoinBalance <$> o .: "total" <*> o .: "spendable" <*> o .: "lockedbytx" <*> o .: "lockedbyuser" <*> o .: "lockedbytag" <*> o .: "lockedbytokens"

instance ToJSON Response where
  toJSON = \case
    -- No private key that the error quotes goes to a script
    ResponseError err -> typed "error" ["error" .= hidingSecKeys err]
    -- The master private key is a key only when it was asked for, and
    -- the warning only when there is one
    ResponseNewPortfolio secretM masterKeyM view active warningM ->
      typed "newportfolio" (["secret" .= secretM] <> ["masterkey" .= key | Just key <- [masterKeyM]] <> ["portfolio" .= view, "activeportfolio" .= active] <> ["warning" .= warning | Just warning <- [warningM]])
    ResponsePortfolio view -> typed "portfolio" ["portfolio" .= view]
    ResponseSwitched view -> typed "switch" ["portfolio" .= view]
    ResponseChecked ref result text -> typed "checked" (portfolioRefPairs ref <> ["result" .= result, "text" .= text])
    -- The backup is a key only when one was made
    ResponseDeleted text backupM -> typed "deleted" (("text" .= text) : ["backup" .= backup | Just backup <- [backupM]])
    -- The journal is a key only when the command turned it on or off
    ResponseDefaults defaults file journalM ->
      typed "defaults" (["defaults" .= keyValues defaults, "file" .= file] <> ["journal" .= switched | Just switched <- [journalM]])
    -- The active portfolio and the active wallet are keys only when asked
    -- for. Each is null in a database without a portfolio.
    ResponseShownDefaults defaults file active ->
      typed "showdefault" (["defaults" .= keyValues defaults, "file" .= file] <> activePairs active)
      where
        activePairs = \case
          ActiveNotAsked -> []
          ActivePortfolioShown refM -> ["activeportfolio" .= refM]
          ActiveWalletShown walletM -> ["activewallet" .= walletM]
    ResponseFile file -> typed "file" ["file" .= file]
    ResponseBackup database labels -> typed "backup" ["database" .= database, "labels" .= labels]
    ResponseRestored file shown backupM damagedM ->
      typed "restored" $
        (("file" .= file) : databaseShownPairs shown)
          <> ["backup" .= backup | Just backup <- [backupM]]
          <> ["damaged" .= damaged | Just damaged <- [damagedM]]
    ResponseBackups shown -> typed "backups" (backupsShownPairs shown)
    ResponseImport report active -> typed "import" (reportPairs report <> ["activeportfolio" .= active])
    ResponseWalletFile file shown -> typed "walletfile" (("file" .= file) : fileShownPairs shown)
    ResponseDatabaseFile file shown -> typed "databasefile" (("file" .= file) : databaseShownPairs shown)
    ResponsePortfolios views -> typed "portfolios" ["portfolios" .= views]
    ResponseAddress ref addr spendable tags followedM _ ->
      typed "address" $
        ["account" .= ref, "address" .= addressWithSpendable (accountRefNetwork ref) addr spendable tags]
          <> maybe [] (changeFollowedPairs (accountRefNetwork ref)) followedM
    ResponseAddresses groups ->
      typed
        "addresses"
        [ "groups"
            .= [ object (groupPairs g <> ["addresses" .= [addressWithSpendable (groupNetwork g) addr spendable tags | (addr, spendable, tags) <- groupRows g]])
                 | g <- groups
               ]
        ]
    ResponseBalances groups ->
      typed
        "balances"
        [ "groups"
            .= [ object (groupPairs g <> ["total" .= object (coinBalancePairs total), "addresses" .= [addressWithCoinBalance (groupNetwork g) addr balance tags | (addr, balance, tags) <- groupRows g]])
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
    -- A sweep that pays no portfolio names its network alone
    ResponseSweep placeM net info empty unswept _ ->
      typed "sweep" $
        maybe ["network" .= net.name] placePairs placeM
          <> ["tx" .= marshalValue net info, "emptyaddrs" .= mapMaybe (addrToText net) empty, "dustcoins" .= (unsweptJson net <$> leftFor WorthLessThanItsFee unswept), "tokencoins" .= (unsweptJson net <$> leftFor HoldsTokens unswept)]
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
    ResponseTags uses -> typed "tags" ["tags" .= uses]
    ResponseTagRenamed old use -> typed "tagrenamed" ["from" .= old, "tag" .= use]
    ResponseTagMerged old use -> typed "tagmerged" ["from" .= old, "tag" .= use]
    ResponseTagDeleted tag had ->
      typed "tagdeleted" ["tag" .= tag, "addresses" .= had.deletedFromAddresses, "txs" .= had.deletedFromTxs, "contacts" .= had.deletedFromContacts]
    ResponseContact net contact -> typed "contact" ["network" .= net.name, "contact" .= contact]
    ResponseContacts groups ->
      typed "contacts" ["groups" .= [object ["network" .= net.name, "contacts" .= contacts] | (net, contacts) <- groups]]
    ResponseContactDeleted net contact -> typed "contactdeleted" ["network" .= net.name, "contact" .= contact]

instance FromJSON Response where
  parseJSON = withObject "response" $ \o -> do
    kind <- o .: "type"
    case kind :: Text of
      "error" -> ResponseError <$> o .: "error"
      "newportfolio" -> ResponseNewPortfolio <$> o .:? "secret" <*> o .:? "masterkey" <*> o .: "portfolio" <*> o .: "activeportfolio" <*> o .:? "warning"
      "portfolio" -> ResponsePortfolio <$> o .: "portfolio"
      "switch" -> ResponseSwitched <$> o .: "portfolio"
      "checked" -> ResponseChecked <$> parsePortfolioRef o <*> o .: "result" <*> o .: "text"
      "deleted" -> ResponseDeleted <$> o .: "text" <*> o .:? "backup"
      "defaults" -> ResponseDefaults <$> (mapM keyValue =<< o .: "defaults") <*> o .: "file" <*> o .:? "journal"
      "showdefault" -> ResponseShownDefaults <$> (mapM keyValue =<< o .: "defaults") <*> o .: "file" <*> activeShown o
      "file" -> ResponseFile <$> o .: "file"
      "backup" -> ResponseBackup <$> o .: "database" <*> o .: "labels"
      "restored" -> ResponseRestored <$> o .: "file" <*> parseDatabaseShown o <*> o .:? "backup" <*> o .:? "damaged"
      "backups" -> ResponseBackups <$> parseBackupsShown o
      "import" -> ResponseImport <$> parseReport o <*> o .:? "activeportfolio"
      "walletfile" -> ResponseWalletFile <$> o .: "file" <*> parseFileShown o
      "databasefile" -> ResponseDatabaseFile <$> o .: "file" <*> parseDatabaseShown o
      "portfolios" -> ResponsePortfolios <$> o .: "portfolios"
      "address" -> do
        ref <- o .: "account"
        (addr, spendable, tags) <- parseAddressWithSpendable =<< o .: "address"
        followedM <- parseChangeFollowed o
        return (ResponseAddress ref addr spendable tags followedM activeLeftOut)
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
      "sweep" -> do
        net <- network o
        portfolioM <- o .:? "portfolio" :: Parser (Maybe Value)
        placeM <- traverse (const (parsePlace o)) portfolioM
        info <- unmarshalValue net =<< o .: "tx"
        empty <- mapM (maybe (fail "Invalid address") return . textToAddr net) =<< o .: "emptyaddrs"
        dust <- mapM (parseUnswept net WorthLessThanItsFee) =<< o .: "dustcoins"
        withTokens <- mapM (parseUnswept net HoldsTokens) =<< o .: "tokencoins"
        return (ResponseSweep placeM net info empty (withTokens <> dust) activeLeftOut)
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
      "tags" -> ResponseTags <$> o .: "tags"
      "tagrenamed" -> ResponseTagRenamed <$> o .: "from" <*> o .: "tag"
      "tagmerged" -> ResponseTagMerged <$> o .: "from" <*> o .: "tag"
      "tagdeleted" -> ResponseTagDeleted <$> o .: "tag" <*> (TagDeleted <$> o .: "addresses" <*> o .: "txs" <*> o .: "contacts")
      "contact" -> ResponseContact <$> network o <*> o .: "contact"
      "contacts" -> ResponseContacts <$> (mapM contactGroup =<< o .: "groups")
      "contactdeleted" -> ResponseContactDeleted <$> network o <*> o .: "contact"
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
      contactGroup = withObject "contacts of a network" $ \entry ->
        (,) <$> network entry <*> entry .: "contacts"
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

-- | A coin a sweep leaves in JSON, with the keys a coin of the wallet has
-- for them: its outpoint, its address in the text of the network, and its
-- value. Why the sweep leaves it is told by the list it is in: the dust
-- coins under "dustcoins", the coins that hold tokens under "tokencoins".
unsweptJson :: Network -> UnsweptCoin -> Value
unsweptJson net left =
  object ["outpoint" .= left.outpoint, "address" .= marshalValue net left.address, "value" .= left.value]

-- | The coins a sweep leaves for a reason, in their order
leftFor :: UnsweptReason -> [UnsweptCoin] -> [UnsweptCoin]
leftFor reason = filter ((== reason) . (.reason))

-- | Read a coin a sweep leaves for a reason back from 'unsweptJson'
parseUnswept :: Network -> UnsweptReason -> Value -> Parser UnsweptCoin
parseUnswept net reason =
  withObject "UnsweptCoin" $ \o ->
    UnsweptCoin <$> o .: "outpoint" <*> (unmarshalValue net =<< o .: "address") <*> o .: "value" <*> pure reason

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
           "locksmoved" .= [object ["from" .= old, "to" .= heir] | (old, heir) <- syncResLocksMoved res],
           "tokencoins" .= syncResTokenCoins res,
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
      <*> (mapM lockMoved =<< o .: "locksmoved")
      <*> o .: "tokencoins"
      <*> o .: "rebroadcast"
      <*> (mapM notPublished =<< o .: "notpublished")
      <*> o .: "conflicted"
      <*> o .: "discovered"

-- | A lock of the user that moved back from its JSON: the coin that is
-- gone, then the coin that took its lock, as 'syncResJson' writes them
lockMoved :: Value -> Parser (OutPoint, OutPoint)
lockMoved = withObject "lock moved" $ \o -> (,) <$> o .: "from" <*> o .: "to"

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
-- satoshis. The network is needed to read the path: m\/44'\/0' is an
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
