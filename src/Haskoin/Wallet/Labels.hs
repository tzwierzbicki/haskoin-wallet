{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Wallets and their labels as a BIP 329 file ("Haskoin.Wallet.Bip329"),
-- which exportwallet writes and importwallet reads.
--
-- An account goes in the file as an @xpub@ record: its key, its origin
-- @pkh([fingerprint\/44'\/coin'\/index'])@, or
-- @wpkh([fingerprint\/84'\/coin'\/index'])@ for a BIP84 account, which names
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
-- one file, and one file keeps every label. hw writes the same file by
-- itself after each command that changes the labels, and keeps the
-- newest copies. The backup command writes it beside its copy of the
-- database ("Haskoin.Wallet.Backup").
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
-- A file of the whole database ends with the address book: an @addr@
-- record for each contact, with its label, its tags and its network,
-- marked with hw's own key @contact@ ('contactRecords'). A contact is of
-- no portfolio, so its record has no origin, and a file of one wallet or
-- of one portfolio leaves the address book out: such a file is made to
-- show less than the whole database. importwallet reads the contacts of
-- a file whatever part of its wallets it is asked for, since they are of
-- none of them (@importContactRecord@).
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
-- for the wallet (@checkScopeHeld@).
--
-- A wallet and a portfolio take the labels of their key records as their
-- descriptions when they have none, and keep their own otherwise
-- (@takeLabel@). The report counts each wallet and each portfolio once,
-- though each of its key records carries its label. A label taken counts
-- among the labels set, beside those of addresses and transactions. A
-- label that meets a description of the user's own counts apart, among
-- what is left aside. So the answer accounts for every label of the file.
--
-- The label of an address, of a transaction or of a contact goes the
-- other way: the label of the file takes the place of the one in the
-- database. A copy of the labels must put back a label that was changed
-- since, which is what a restore is for. Such a label is counted as
-- replaced (@replacedBy@), and show names it beside the record of the
-- file, so that the user reads what goes before the import.
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
-- (@labelTxRecord@). A coin that is not spendable is
-- locked, as lockcoin locks it. What hw cannot place, the wallet may not
-- have yet: the answer says so, and a sync followed by another import
-- places it. What hw keeps no label for, such as inputs, coins and change
-- addresses, is left aside and counted.
--
-- The tags of an address do not wait for a second import. They are kept
-- at once, also for an address the wallet has not generated, a change
-- address or a receiving address past the thousand
-- (@labelAddressRecord@): a tag may lock its coins, and the lock must be
-- there when the first sync finds them. The tags that nothing has travel
-- too, with their colours and their locks, on the first key record of a
-- file of the whole database (@namingUnusedTags@): such a file brings
-- back every tag of the database.
--
-- show shows such a file before the user imports it ('fileShown'). It
-- imports the file as importwallet does, and undoes that. So it shows what
-- importwallet would make of each wallet, each portfolio and each account,
-- with the labels and locks of its records under it, what importwallet
-- would make of each record (the type t'RecordFate'), and the report
-- importwallet would answer with. The database keeps nothing of it. show
-- puts a record under an account of the file by its origin only, where
-- importwallet looks for each in the wallet: the view is laid out as the
-- file is.
module Haskoin.Wallet.Labels
  ( -- * The file exportwallet writes
    walletRecords,
    databaseRecords,
    contactRecords,
    isContactRecord,

    -- * The file importwallet reads
    WalletFile (..),
    readWalletFile,
    ImportScope (..),
    wholeFile,
    importFile,
    ImportReport (..),
    WalletImport (..),
    PortfolioImport (..),
    LabelImport (..),
    AccountImport (..),
    reportPairs,
    parseReport,
    reportNeedsSync,
    importCreated,

    -- * The file show shows
    FileShown (..),
    FileRecord (..),
    RecordFate (..),
    FileTag (..),
    recordFileTags,
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
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Json
import Data.Aeson.Types (Pair, Parser)
import Data.Bifunctor (first)
import Data.Bits (clearBit, testBit)
import Data.Either (rights)
import Data.Function (on)
import Data.List (elemIndex, genericLength, intercalate, nub, nubBy, partition, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Traversable (mapAccumM)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Bip329
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.BestBlock (bestHeight, getConfirmations)
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Contact (addContactDated, contactAt, contactText, networkContacts, setContactLabel, tagContact)
import Haskoin.Wallet.Database.Coin (coinOutpoint, coinRows, readBlock, setLockedByUser, userLockedCoins)
import Haskoin.Wallet.Database.PendingTx (PendingTx (..), findPendingTx, intercalateAnd, pendingTxs, setPendingTxLabel, txInfoOf)
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.TxInfo (TxInfo (..), TxSummary (..), allPools, liveSummary, txInfoKey)
import Haskoin.Wallet.Database.Tag (ChainAddress (..), TagSettings (..), accountAddressTags, chainAddressOf, ensureTag, heldTag, noTagFilter, poolTxTags, setTagColor, setTagLocksCoins, tagChainAddress, tagSettings, tagTx, txTags)
import Haskoin.Wallet.Database.TxLabel (poolTxLabels, setTxLabel, txLabel)
import Haskoin.Wallet.Descriptor
import Haskoin.Wallet.Keys (AccountKind (..), accountDeriv, checkKindOnNetwork, kindName, kindOfPurpose, kindPurpose, portfolioAccounts, portfolioNetworks, sameKey, xPubIndex)
import Haskoin.Wallet.Tags (ShownTag (..), Tag, colorName, readTag, readTagColor, tagText)
import Haskoin.Wallet.Tx.SignData (TxSignData (..), isSigned, nosigHash)
import Haskoin.Wallet.Util (addrToTextE, visibleText)
import Numeric.Natural (Natural)

{- The file exportwallet writes -}

-- | The records of portfolios, in their order: those of each account of
-- each (@accountRecords@). The portfolios may be of several wallets. The
-- origin of each record names its wallet, by the fingerprint of the
-- portfolio the record is of.
walletRecords :: (MonadUnliftIO m) => [Portfolio] -> ExceptT String (DB m) [Record]
walletRecords portfolios =
  fmap concat . forM portfolios $ \portfolio -> do
    accounts <- lift $ portfolioAccountRows portfolio.portfolioId
    -- The transactions are of the portfolio on a network: they are
    -- written once, with the first account of the network
    let firstOfPool = [accId | net <- nub (accountNetwork . snd <$> accounts), (accId, _) <- take 1 [a | a@(_, acc) <- accounts, accountNetwork acc == net]]
    concat <$> forM accounts (\held@(accId, _) -> accountRecords portfolio (accId `elem` firstOfPool) held)

-- | The records of an account of a portfolio: its key, then its
-- receiving addresses that have a label and its addresses of either
-- chain that have tags, the transactions of its portfolio on its network
-- that have a label or tags, when they are asked for, on the chain or
-- signed and pending, and the coins the user locked.
--
-- A record carries the tags of its address or of its transaction, with
-- the colours of those that have one and with those that lock their
-- coins ('withTags'). A tag nothing has is
-- written nowhere: the file holds what the addresses and the
-- transactions have, and a file of one portfolio names no tag of
-- another.
accountRecords :: (MonadUnliftIO m) => Portfolio -> Bool -> (DBAccountId, DBAccount) -> ExceptT String (DB m) [Record]
accountRecords portfolio withTransactions (accId, acc) = do
  heightM <- bestHeight net
  settings <- lift tagSettings
  labelled <- lift $ labelledAddresses accId
  tagged <- lift $ accountAddressTags accId
  pending <- if withTransactions then pendingTxs pool else return []
  labels <- if withTransactions then lift (poolTxLabels pool) else return []
  -- A pending transaction is tagged under its nosigHash, and written
  -- under its id once signed, with the other pending ones below
  let pendingHashes = [nosigHash (txSignDataTx p.pendingTxData) | p <- pending]
  taggedTxs <- if withTransactions then lift (filter ((`notElem` pendingHashes) . fst) <$> poolTxTags pool) else return []
  let labelOf = Map.fromList labels
      tagsOf = Map.fromList taggedTxs
  chainRecords <- forM (Set.toAscList (Map.keysSet labelOf <> Map.keysSet tagsOf)) $ \tid -> do
    liveM <- liveSummary pool tid
    return . withTags settings (Map.findWithDefault [] tid tagsOf) $
      (ofAccount (record TxRecord (txHashToHex tid)))
        { recordLabel = Map.lookup tid labelOf,
          recordHeight = settledHeight heightM . fst =<< liveM,
          recordFee = summaryFee . snd <$> liveM,
          recordValue = summaryAmount . snd <$> liveM
        }
  pendingRecords <- fmap concat . forM pending $ \p -> do
    let tx = txSignDataTx p.pendingTxData
        label = p.pendingTxData.txSignDataLabel
    tags <- lift $ map shownTag <$> txTags pool (nosigHash tx)
    if isSigned tx && not (Text.null label && null tags)
      then do
        info <- txInfoOf pool p.pendingTxData
        return
          [ withTags settings tags $
              (ofAccount (record TxRecord (txHashToHex (txHash tx))))
                { recordLabel = if Text.null label then Nothing else Just label,
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
  return $ keyRecord : addressRecords settings labelled tagged <> chainRecords <> pendingRecords <> coinRecords
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
    -- An address is written once, with its label and its tags, in the
    -- order of the chains: the receiving addresses first
    addressRecords settings labelled tagged =
      [ withTags settings (Map.findWithDefault [] place tagsAt) $
          (ofAccount (record AddrRecord address))
            { recordLabel = Map.lookup place labelAt,
              recordKeypath = Just (keypathText (chainPath internal index))
            }
        | place@(internal, index, address) <- Set.toAscList (Map.keysSet labelAt <> Map.keysSet tagsAt)
      ]
      where
        labelAt = Map.fromList [((False, dBAddressIndex addr, dBAddressAddress addr), dBAddressLabel addr) | addr <- labelled]
        tagsAt = Map.fromList tagged

-- | The records of the address book: an addr record for each contact of
-- each network, in the order contacts lists them, with its label when it
-- has one, its tags, its network, and the date it was added
-- ('recordAdded'). Each is marked as a contact ('recordContact'), and
-- names no origin: a contact is of no wallet.
--
-- They are written for the whole database only. exportwallet adds them
-- to a file of every wallet, and the copies of the labels hw keeps hold
-- them. A file of one wallet or of one portfolio does not: the user
-- narrowed it so that it shows less.
contactRecords :: (MonadUnliftIO m) => DB m [Record]
contactRecords = do
  settings <- tagSettings
  fmap concat . forM portfolioNetworks $ \net -> do
    contacts <- networkContacts net noTagFilter
    return
      [ withTags settings (shownTag <$> contactTags contact) $
          (record AddrRecord (contactAddress contact))
            { recordLabel = if Text.null (contactLabel contact) then Nothing else Just (contactLabel contact),
              recordNetwork = Just (cs net.name),
              recordContact = Just True,
              recordAdded = Just (contactCreated contact)
            }
        | contact <- contacts
      ]

-- | The records of the whole database: those of the portfolios given,
-- which are all of them, then those of the address book, which is of no
-- portfolio ('contactRecords'). exportwallet without options writes
-- them, and so do the copies of the labels hw keeps, so that a copy
-- brings the address book back with the labels.
--
-- The first key record also names the tags that no record of the file
-- has (@namingUnusedTags@), so the file holds every tag of the database.
databaseRecords :: (MonadUnliftIO m) => [Portfolio] -> ExceptT String (DB m) [Record]
databaseRecords portfolios = do
  records <- (<>) <$> walletRecords portfolios <*> lift contactRecords
  settings <- lift tagSettings
  return (namingUnusedTags settings records)

-- | The records of a file of the whole database, the first key record
-- naming besides the tags that no record of the file has, as a record
-- names the tags of its address ('withTags'): under @tags@, with their
-- colours and their locks under @tagcolors@ and @taglocks@.
--
-- A tag can lock its coins before any address has it, so that the first
-- coin it receives is locked. A record carries the colour and the lock
-- of the tags of its address or of its transaction, so the file said
-- nothing of such a tag: a wallet made again from its file had lost the
-- lock, and the address tagged afterwards was free. A lock is a setting
-- of the database and not of a record, so it travels with the file
-- whatever has the tag.
--
-- It goes on a key record, which carries the other keys of hw that are
-- not of an address or a transaction, and not on a line of a new type:
-- every line stays a record the BIP defines, with keys another wallet
-- passes over. One record says it, the first, since the tags are of the
-- database and of no account. A tag without a colour and without a lock
-- is named too: it has nothing to lose but itself, and one rule for
-- every tag is simpler to tell than a rule with an exception. The tags
-- of the wallet made again are then those that hw tags listed.
--
-- Only a file of the whole database names them, as it alone holds the
-- address book ('contactRecords'): a file of one wallet or of one
-- portfolio is made to show less.
namingUnusedTags :: Map Tag TagSettings -> [Record] -> [Record]
namingUnusedTags settings records =
  case break ((== XpubRecord) . recordType) records of
    (before, key : after) -> before <> (withTags settings unused key : after)
    _ -> records
  where
    named = Set.fromList (concatMap recordTagsRead records)
    unused = filter (`Set.notMember` named) (Map.keys settings)

-- | Whether a record is one of the address book: an addr record marked
-- as a contact
isContactRecord :: Record -> Bool
isContactRecord r = recordType r == AddrRecord && recordContact r == Just True

-- | A record with the tags given, the colours of those that have one,
-- and those that lock their coins, from the settings of every tag. A
-- record of something without a tag is left as it is, without the three
-- keys, and each of the last two is left out when it would be empty.
withTags :: Map Tag TagSettings -> [Tag] -> Record -> Record
withTags _ [] r = r
withTags settings tags r =
  r
    { recordTags = Just (tagText <$> tags),
      recordTagColors = if Map.null coloured then Nothing else Just coloured,
      recordTagLocks = if null locking then Nothing else Just locking
    }
  where
    coloured = Map.fromList [(tagText tag, colorName color) | tag <- tags, Just color <- [tagSetColor =<< Map.lookup tag settings]]
    locking = [tagText tag | tag <- tags, maybe False tagSetLocksCoins (Map.lookup tag settings)]

-- | The tags a record gives its address or its transaction: the words of
-- its tags that are tags ("Haskoin.Wallet.Tags".'readTag'). A word that
-- is none is passed over. Another program may have written the file, and
-- hw keeps what it can of it.
recordTagsRead :: Record -> [Tag]
recordTagsRead r = rights (readTag <$> fromMaybe [] (recordTags r))

-- | Give the tags of a record the colours the record names, those that
-- have none yet. A tag that has a colour keeps it, as a description the
-- wallet has is kept: what the user set here wins over a file. A colour
-- hw does not know is passed over.
--
-- A tag the record says locks its coins is made to lock them. The other
-- way never happens: a file unlocks nothing, as it takes no tag off. A
-- file written before the tag was made to lock, read again, would
-- otherwise free the savings without a word.
--
-- Each tag is made a tag of the database first. The tags of an address
-- or of a transaction are tags already. Those of a key record are on
-- nothing (@namingUnusedTags@), and this is where they come back, the
-- ones without a colour and without a lock too.
takeTagSettings :: (MonadUnliftIO m) => Record -> DB m ()
takeTagSettings r =
  forM_ (recordTagsRead r) $ \tag -> do
    ensureTag tag
    heldM <- heldTag tag
    case (shownTagColor =<< heldM, readTagColor =<< Map.lookup (tagText tag) colours) of
      (Nothing, Just color) -> setTagColor tag (Just color)
      _ -> return ()
    when (tagText tag `elem` locks) $ setTagLocksCoins tag True
  where
    colours = fromMaybe Map.empty (recordTagColors r)
    locks = fromMaybe [] (recordTagLocks r)

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
    -- | The addresses the file put tags on
    reportAddressTags :: !Natural,
    -- | The transactions the file put tags on, each portfolio counting
    -- once
    reportTxTags :: !Natural,
    -- | The contacts of the file that are in the address book after the
    -- import: those it added, and those it found there
    reportContacts :: !Natural,
    -- | The contacts of the file that are addresses of the wallet, which
    -- the address book does not take
    reportContactsOwn :: !Natural,
    -- | The labels set on addresses that took the place of another label,
    -- which the address had in the database
    reportAddressLabelsReplaced :: !Natural,
    -- | The labels set on transactions that took the place of another
    -- label, each portfolio counting once
    reportTxLabelsReplaced :: !Natural,
    -- | The labels of contacts that took the place of another label,
    -- which the contact had in the address book
    reportContactLabelsReplaced :: !Natural,
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

-- | The label a file gives a wallet or a portfolio, which hw keeps as its
-- description, and what became of it. Each key record of a portfolio
-- carries the label of the portfolio and that of its wallet. So a wallet
-- or a portfolio has one label of the file, however many of its key
-- records the file holds (@firstLabel@).
--
-- The description kept in its place is given, so that show can say,
-- before the import, which text the wallet goes on with (@takeLabel@).
data LabelImport = LabelImport
  { -- | The label of the file
    labelGiven :: !Text,
    -- | The description of its own that the wallet or the portfolio keeps
    -- in its place, which differs from the label. None when it bears the
    -- label of the file after the import: it had no description and took
    -- it, or had that one already.
    labelKeptInstead :: !(Maybe Text)
  }
  deriving (Eq, Show)

-- | Count in a report what became of the labels of wallets and
-- portfolios, from what became of each wallet and portfolio
withLabelsTaken :: ImportReport -> ImportReport
withLabelsTaken report =
  report
    { reportWalletLabels = countOf taken walletLabels,
      reportPortfolioLabels = countOf taken portfolioLabels,
      reportWalletLabelsKept = countOf kept walletLabels,
      reportPortfolioLabelsKept = countOf kept portfolioLabels
    }
  where
    walletLabels = walletImportLabel <$> reportWallets report
    portfolioLabels = portfolioImportLabel <$> concatMap walletImportPortfolios (reportWallets report)
    taken = maybe False (isNothing . labelKeptInstead)
    kept = maybe False (isJust . labelKeptInstead)
    countOf is = fromIntegral . length . filter is

-- | What became of a wallet the file names
data WalletImport = WalletImport
  { -- | The index of the wallet in the database
    walletImportIndex :: !Natural,
    -- | The fingerprint of its seed
    walletImportFingerprint :: !Fingerprint,
    -- | Whether the import created it
    walletImportCreated :: !Bool,
    -- | The label the file gives it, if any, and what became of it
    walletImportLabel :: !(Maybe LabelImport),
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
    portfolioImportCreated :: !Bool,
    -- | The label the file gives it, if any, and what became of it
    portfolioImportLabel :: !(Maybe LabelImport)
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
      reportAddressTags = 0,
      reportTxTags = 0,
      reportContacts = 0,
      reportContactsOwn = 0,
      reportAddressLabelsReplaced = 0,
      reportTxLabelsReplaced = 0,
      reportContactLabelsReplaced = 0,
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
-- names the wallet too (@checkScopeHeld@).
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
importFile :: (MonadUnliftIO m) => ImportScope -> Text -> ExceptT String (DB m) ImportReport
importFile scope content = undoneIfRefused (fst <$> importTelling scope (readWalletFile content))

-- | What a file of a wallet holds, as importwallet reads it, before
-- anything is done with it. importwallet imports it, and show shows it
-- ('fileShown').
data WalletFile = WalletFile
  { -- | The key of each account, in the order of the file, each key once
    -- on its network, with the number of the line that gives it first. A
    -- refusal names the line of a key that does not read.
    fileKeys :: ![(Natural, Record)],
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
    { fileKeys = nubBy ((==) `on` snd) [(number, r) | KeyLine number r <- classified],
      fileRecords = [(number, r) | RecordLine number r <- classified],
      fileOtherTypes = fromIntegral (length [() | OtherTypeLine <- classified]),
      fileUnreadable = [number | UnreadableLine number <- classified]
    }
  where
    classified = classifyLine <$> readLines content

-- | What a line of a file holds, for the import
data FileLine
  = -- | An xpub record, with the number of its line
    KeyLine !Natural !Record
  | -- | A record of another type, with the number of its line
    RecordLine !Natural !Record
  | -- | A record of a type the BIP does not define
    OtherTypeLine
  | -- | A line that is not a record, by its number
    UnreadableLine !Natural

-- | A line as a record. A line that is none is not readable.
classifyLine :: (Natural, Text) -> FileLine
classifyLine (number, line) = case parseLine line of
  Right (LineRecord r) | recordType r == XpubRecord -> KeyLine number r
  Right (LineRecord r) -> RecordLine number r
  Right (LineOfOtherType _) -> OtherTypeLine
  Left _ -> UnreadableLine number

-- | Import a file: the wallets of its key records, then its other
-- records, and count the lines that are not records and the records of a
-- type the BIP does not define. A scope the records cannot answer is
-- refused (@checkScopeHeld@).
--
-- With the report come the records show lists, in the order of the file,
-- each with what the import made of it ('recordFate'). importwallet
-- answers with the report alone. show undoes the import and shows both
-- ('fileShown'), so the two commands say the same of a file.
importTelling :: (MonadUnliftIO m) => ImportScope -> WalletFile -> ExceptT String (DB m) (ImportReport, [FileRecord])
importTelling scope file = do
  liftEither $ checkScopeHeld scope (snd <$> keys) (snd <$> fileRecords file)
  (wallets, accountImports) <- importKeys scope (fileUnreadable file) keys
  -- The tags a key record names are of the database and of no portfolio,
  -- so they are taken whatever the scope, as the contacts are
  lift $ mapM_ (takeTagSettings . snd) keys
  accounts <- lift getAccounts
  -- The records of the wallets and portfolios left out are counted, and
  -- not read. Their labels would be looked for in what is read.
  let (inScope, leftOut) = partition (recordInScope scope . snd) (fileRecords file)
      keysImported = withLabelsTaken (emptyReport wallets accountImports (fileUnreadable file))
  (report, told) <- mapAccumM (importNext accounts) keysImported inScope
  return
    ( report
        { reportOtherPortfolios = fromIntegral (length leftOut),
          reportNotKept = reportNotKept report + fileOtherTypes file,
          -- A record whose reference does not read is found after the
          -- lines that are not records were numbered
          reportUnreadable = sort (reportUnreadable report)
        },
      catMaybes told
    )
  where
    keys = fileKeys file
    importNext accounts report numbered@(_, r) = do
      (next, replaced) <- importRecord accounts report numbered
      return (next, FileRecord r (nub replaced) <$> recordFate report next)

-- | What importwallet makes of a record of anything but a key, as show
-- says it on the line of the record
data RecordFate
  = -- | It sets what the record holds: a label, tags, a lock or a contact
    FateSet
  | -- | The wallet does not have the address, the transaction or the coin
    -- of the record yet. A sync may bring it, and a second import then
    -- places the record.
    FateNotInWallet
  | -- | The origin of the record names an account the wallet does not
    -- have: the record is of another wallet
    FateOtherAccount
  | -- | The record is a contact, and its address is one of the wallet,
    -- which the address book does not take
    FateOwnAddress
  deriving (Eq, Show)

-- | What the import made of one record, told from the report before the
-- record and the report after it: the counts the record moved. Nothing
-- for a record show does not list, which sets nothing hw keeps: a record
-- with neither a label nor a tag, the label of a change address, a
-- record hw keeps no label for, and one whose reference does not read.
-- The report counts those.
--
-- The counts are the one account of what an import did, and importwallet
-- answers with them. Read from them, the line of a record in show cannot
-- say another thing than the counts under the records do. The other way,
-- a fate given beside each count, would write every rule twice.
--
-- A record that sets a part of what it holds is set: the tags of a change
-- address are, when its label is left aside. One that waits for its
-- address is not in the wallet yet, though its tags are kept at once.
recordFate :: ImportReport -> ImportReport -> Maybe RecordFate
recordFate before after
  | any grew [reportAddressesMissing, reportTxsMissing, reportCoinsMissing] = Just FateNotInWallet
  | grew reportOtherAccounts = Just FateOtherAccount
  | grew reportContactsOwn = Just FateOwnAddress
  | any grew whatIsSet = Just FateSet
  | otherwise = Nothing
  where
    grew :: (ImportReport -> Natural) -> Bool
    grew count = count after > count before
    whatIsSet = [reportAddressLabels, reportTxLabels, reportAddressTags, reportTxTags, reportContacts, reportCoinsLocked, reportCoinsUnlocked]

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
checkScopeHeld :: ImportScope -> [Record] -> [Record] -> Either String ()
checkScopeHeld scope keys others = do
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
    (Nothing, Just wanted, firstWallet : _ : _) ->
      Left $
        "The file holds the wallets "
          <> intercalateAnd (shown <$> wallets)
          <> ", and --portfolio alone does not say which of them "
          <> cs (portfolioName wanted)
          <> " is of. Name the wallet by its fingerprint too, as in --wallet "
          <> shown firstWallet
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
    keyPlaces = [(key.walletKeyFingerprint, key.walletKeyIndex) | Right key <- readWalletKey <$> keys]
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
-- must agree. An origin must be the path of that account on that network:
-- the key holds the index of its account, and the refusal names both the
-- origin of the record and the account of the key.
readWalletKey :: Record -> Either Text WalletKey
readWalletKey r = do
  (prefixKindM, keyText) <- failing (readAccountKey (recordRef r))
  when (isPrivateKey keyText) $
    Left "The key is a private key. hw takes the public key of an account, and no file of a wallet should hold a private one"
  netName <- maybe (Left noNetworkKey) Right (recordNetwork r)
  net <- maybe (Left ("Unknown network " <> visibleText netName)) Right (netByName (cs netName))
  xpub <- maybe (Left (keyNotRead net keyText)) Right (xPubImport net keyText)
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
      Left ("The record gives its key the origin " <> originText origin <> ", and the key is that of the " <> kindName kind <> " account " <> cs (pathToStr path) <> ": the two do not agree")
  return (WalletKey r net kind index xpub (originFingerprint <$> originM))
  where
    originM = parseOrigin =<< recordOrigin r
    failing :: Either String a -> Either Text a
    failing = either (Left . cs) Right
    -- A key whose checksum fails was damaged: a character of the file was
    -- changed, or the line was cut. A key that passes it is whole, and is
    -- the key of another network.
    keyNotRead :: Network -> Text -> Text
    keyNotRead net keyText
      | keyChecksumHolds keyText = "The key does not read as a key of " <> cs net.name
      | otherwise = "The key does not read: its checksum is wrong, as when a character of the key was changed, added or lost"
    noNetworkKey = "The record names no network. hw imports the wallets exportwallet writes, not a key alone: a key is one account, and does not say which portfolio it belongs to"
    noTypeKey = "The record does not say the type of its account. hw imports the wallets exportwallet writes, not a key alone"

-- | Import the wallets the key records name, within the scope, and say
-- what became of each wallet, with its portfolios and their labels, and
-- of each account, in the order of the records. Each record comes with
-- the number of its line, and the numbers of the lines of the file that
-- are not records come first.
--
-- The keys are sorted by wallet, by the fingerprint of their origin. A
-- key without an origin names the wallet of the account that holds it in
-- the database, and is left aside when none does. Each wallet is then
-- imported ('importWallet').
--
-- The scope leaves out the keys of the other portfolios, and the wallets
-- of the other fingerprints. The answer says nothing of a key left out
-- this way, since the user asked for it.
--
-- A refusal of a wallet starts with the lines of the file that were not
-- read ('linesNotRead'). They are most often its cause: a portfolio that
-- lacks an account lacks the one of a damaged line. Without them, a file
-- of twelve lines is told that it gives eleven accounts, and the user
-- counts twelve.
--
-- A file from which no key is taken is refused when one of its keys names
-- a network, as the keys of hw do, and was left aside ('noKeyImported').
importKeys :: (MonadUnliftIO m) => ImportScope -> [Natural] -> [(Natural, Record)] -> ExceptT String (DB m) ([WalletImport], [AccountImport])
importKeys scope unreadable records = do
  let readKeys = [(number, r, readWalletKey r) | (number, r) <- records]
      ofPortfolio :: WalletKey -> Bool
      ofPortfolio key = maybe True (== key.walletKeyIndex) scope.scopePortfolio
      ofWallet :: Fingerprint -> Bool
      ofWallet fingerprint = maybe True (== fingerprint) scope.scopeWallet
  -- Each key of a portfolio in scope, with the fingerprint of the wallet
  -- it names
  named <- forM [(number, key) | (number, _, Right key) <- readKeys, ofPortfolio key] $ \(number, key) ->
    case key.walletKeyFingerprint of
      Just fingerprint -> return (number, key, Just fingerprint)
      Nothing -> do
        heldM <- lift $ accountWithKey key.walletKeyNetwork key.walletKeyXPub
        ownerM <- forM heldM $ \(_, acc) -> portfolioOfAccount acc
        return (number, key, portfolioFingerprint <$> ownerM)
  let fingerprints = nub [fingerprint | (_, _, Just fingerprint) <- named, ofWallet fingerprint]
      -- The keys left aside, by line, each with the reason
      aside = Map.fromList ([(number, why) | (number, _, Left why) <- readKeys] <> [(number, noWalletNamed) | (number, _, Nothing) <- named])
      notRead = linesNotRead (Map.toAscList (aside <> Map.fromList [(number, notARecord) | number <- unreadable]))
  imported <- withExceptT (concatMap (<> ". ") notRead <>) . forM fingerprints $ \fingerprint ->
    importWallet scope fingerprint [key | (_, key, Just other) <- named, other == fingerprint]
  let taken = [(key.walletKeyRecord, outcome) | (key, outcome) <- concatMap snd imported]
      outcomeOf (number, r) = lookup r taken <|> (AccountLeftAside (shownKey r) <$> Map.lookup number aside)
      ofHw = [() | (number, r) <- records, number `Map.member` aside, isJust (recordNetwork r)]
  when (null taken && not (null ofHw)) $ throwError (noKeyImported notRead)
  return (fst <$> imported, mapMaybe outcomeOf records)
  where
    noWalletNamed = "The record has no origin, and no account of the database holds its key: nothing says which wallet it is of"
    notARecord = "It is not a record: the line may be cut or damaged"

-- | The lines of a file that importwallet did not read, as the sentences
-- of a refusal: the first one with its reason, and the count of the
-- others. Each line comes with its number and its reason, in the order of
-- the file. A damaged file is replaced by a good copy, not mended line by
-- line, so the count of the others says enough, and the refusal of a long
-- file stays short.
linesNotRead :: [(Natural, Text)] -> [String]
linesNotRead = \case
  [] -> []
  (number, why) : others ->
    ("Line " <> show number <> " of the file was not read. " <> cs why) : case length others of
      0 -> []
      1 -> ["1 other line was not read either"]
      count -> [show count <> " other lines were not read either"]

-- | Why a file from which no key was taken is refused, with the lines
-- that were not read ('linesNotRead'). Such a file was a success before,
-- which advised a sync in a database that could have no wallet: a script
-- took a restore that brought nothing back for one that worked.
--
-- The file of another wallet is not concerned. Its key record names no
-- network, is left aside with a word, and the labels of the file are
-- still set.
noKeyImported :: [String] -> String
noKeyImported notRead = intercalate ". " ("None of the keys of the file was imported" : notRead)

-- | Import the keys of one wallet of a file, the wallet of the
-- fingerprint, and say what became of the wallet, with its portfolios and
-- their labels, and of each key.
--
-- Each portfolio of the file is taken whole ('importPortfolio'). A wallet
-- the database does not have is created, with its portfolio0, which every
-- wallet has: a file that does not bring that portfolio is refused, and
-- so is importwallet --portfolio N of such a wallet, with the advice to
-- import the whole file, or the whole wallet when --wallet names it. The
-- wallet takes the label the file gives it as its description when it
-- has none (@takeLabel@).
importWallet ::
  (MonadUnliftIO m) =>
  ImportScope ->
  Fingerprint ->
  [WalletKey] ->
  ExceptT String (DB m) (WalletImport, [(WalletKey, AccountImport)])
importWallet scope fingerprint keys = do
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
    importPortfolio (walletId, wallet) created index [key | key <- keys, key.walletKeyIndex == index]
  let summary =
        WalletImport
          { walletImportIndex = walletIndex wallet,
            walletImportFingerprint = fingerprint,
            walletImportCreated = created,
            walletImportLabel = walletLabel,
            walletImportPortfolios = fst <$> imported
          }
  return (summary, concatMap snd imported)

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
takeLabel :: (Monad m) => Text -> Maybe Text -> (Text -> m ()) -> m (Maybe LabelImport)
takeLabel held givenM setDescription = forM givenM $ \given ->
  if
    | given == held -> return (LabelImport given Nothing)
    | Text.null held -> LabelImport given Nothing <$ setDescription given
    | otherwise -> return (LabelImport given (Just held))

-- | Import the keys of one portfolio of a wallet, at an index, and say
-- what became of the portfolio, with its label, and of each key. The flag
-- says whether the import has just created the wallet, which the refusals
-- then name by its fingerprint: a refusal undoes it.
--
-- A portfolio the wallet has is whole already. Every key of the file must
-- be the one its account holds, or the import is refused: the file and
-- the database are then not of one mnemonic. Nothing is added to it, and
-- it takes the label the file gives it as its description when it has
-- none (@takeLabel@).
--
-- A portfolio the wallet does not have is created with the twelve
-- accounts of the file, and with the label the file gives it as its
-- description: the file must give a key for each of them
-- ('wholePortfolioKeys').
importPortfolio ::
  (MonadUnliftIO m) =>
  (DBWalletId, DBWallet) ->
  Bool ->
  Natural ->
  [WalletKey] ->
  ExceptT String (DB m) (PortfolioImport, [(WalletKey, AccountImport)])
importPortfolio (walletId, wallet) newWallet index keys = do
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
        unless (accountXPubKey acc == key.walletKeyXPub) $
          throwError $
            "The " <> cs (kindName key.walletKeyKind) <> " account of " <> portfolioText portfolio <> " on " <> key.walletKeyNetwork.name
              <> " has another key than the file gives it: the file and the database are not of one mnemonic"
        return (key, AccountHeld (refOf portfolio key))
      label <- takeLabel (dBPortfolioDescription row) description (setPortfolioDescription portfolioKey)
      return (PortfolioImport index False label, outcomes)
    Nothing -> do
      accountKeys <- liftEither $ wholePortfolioKeys place keys
      portfolio <- createPortfolioWith (walletId, wallet) index (fromMaybe "" description) accountKeys
      let label = (`LabelImport` Nothing) <$> description
      return (PortfolioImport index True label, [(key, AccountAdded (refOf portfolio key)) | key <- keys])

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

-- | A report with the labels of the database that the label of a record
-- replaced: the label an address or a contact had, or the label a
-- transaction had in each portfolio that had another
type Imported = (ImportReport, [Text])

-- | Of the labels a thing has, those a new label replaces: every one that
-- is a label and differs from it. An import replaces a label, as the same
-- file imported after a restore must put every label back, and the user
-- is told of each label that goes ('reportAddressLabelsReplaced').
replacedBy :: Text -> [Text] -> [Text]
replacedBy label held = [old | old <- held, not (Text.null old), old /= label]

-- | Import a record of anything but a key, count what was done with it,
-- and give the labels it replaced
importRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> (Natural, Record) -> ExceptT String (DB m) Imported
importRecord accounts report (number, r) =
  case recordType r of
    AddrRecord
      | isContactRecord r -> importContactRecord accounts report r
      | otherwise -> labelAddressRecord accounts report r (\acc -> textToAddr (accountNetwork acc) (recordRef r))
    PubkeyRecord
      | isJust (publicKeyOf (recordRef r)) ->
          labelAddressRecord accounts report r (\acc -> publicKeyAddress (accountKind acc) (recordRef r))
      | otherwise -> return (leftAside, [])
    TxRecord ->
      case hexToTxHash (recordRef r) of
        Just tid -> labelTxRecord accounts report r tid
        Nothing -> return (unreadable, [])
    OutputRecord ->
      case readOutpoint (recordRef r) of
        Just outpoint -> (,[]) <$> lockOutputRecord accounts report r outpoint
        Nothing -> return (unreadable, [])
    InputRecord -> return (leftAside, [])
    SpscanRecord -> return (leftAside, [])
    XpubRecord -> return (leftAside, [])
  where
    leftAside = report {reportNotKept = reportNotKept report + 1}
    unreadable = report {reportUnreadable = reportUnreadable report <> [number]}

-- | Put the contact of a record in the address book of the network the
-- record names, with its label and its tags, and count it.
--
-- A contact the address book lacks is added. One it has takes the label
-- of the record, when the record gives one, in place of its own, as an
-- address of the wallet takes the label of its record. It keeps its
-- tags, and gets those of the record besides. So a copy of the labels
-- brings back a contact that was deleted, and puts back a label that
-- was changed.
--
-- An address of a wallet of the database is not added, and is counted
-- apart. The address book is for the addresses of others
-- ("Haskoin.Wallet.Contacts"). This happens when the file comes from a
-- database that did not hold the wallet of the address.
--
-- A record that names no network hw knows, or whose address does not
-- read on its network, is counted among the records hw keeps nothing of.
-- Only hw writes such a record, with its network.
--
-- A contact the import adds is dated as the record dates it
-- ('recordAdded'), and at the time of the import when the record has no
-- date. A contact the address book has keeps its date: the user added it
-- here on that day.
importContactRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> ExceptT String (DB m) Imported
importContactRecord accounts report r =
  case named of
    Nothing -> return (report {reportNotKept = reportNotKept report + 1}, [])
    Just (net, addr) -> do
      address <- liftEither (contactText net addr)
      held <- lift $ forM [(accId, acc) | (_, accId, acc) <- accounts, accountNetwork acc == net] (storedAmong [addr])
      if any (not . null) held
        then return (report {reportContactsOwn = reportContactsOwn report + 1}, [])
        else do
          contactM <- lift $ contactAt net address
          case contactM of
            -- A contact the import creates is dated as the file dates it
            Nothing -> addContactDated net addr (fromMaybe "" (recordLabel r)) =<< maybe (liftIO getCurrentTime) return (recordAdded r)
            Just _ -> lift $ forM_ (recordLabel r) (setContactLabel net address)
          lift $ tagContact net address (recordTagsRead r)
          lift $ takeTagSettings r
          let replaced = maybe [] (`replacedBy` (contactLabel <$> maybeToList contactM)) (recordLabel r)
          return
            ( report
                { reportContacts = reportContacts report + 1,
                  reportContactLabelsReplaced = reportContactLabelsReplaced report + genericLength replaced
                },
              replaced
            )
  where
    named = do
      net <- netByName . cs =<< recordNetwork r
      addr <- textToAddr net (recordRef r)
      return (net, addr)

-- | A public key written in hexadecimal, compressed or not. None for a
-- key of another kind, such as an x-only key.
publicKeyOf :: Text -> Maybe PublicKey
publicKeyOf hex = do
  bytes <- decodeHex hex
  either (const Nothing) Just (decodeSerial bytes)

-- | The address of a public key written in hexadecimal in an account of
-- the kind: its P2PKH address in a BIP44 account, compressed or not, its
-- P2WPKH address in a BIP84 one, which an uncompressed key does not have
publicKeyAddress :: AccountKind -> Text -> Maybe Address
publicKeyAddress kind hex = do
  key <- publicKeyOf hex
  case kind of
    Bip44 -> Just (pubKeyAddr key)
    Bip84 -> pubKeyWitnessAddr key

-- | Label and tag the address of a record, read in each account as the
-- function gives it: see 'placeAddress'. A record with neither a label
-- nor a tag sets nothing.
--
-- The label goes to a receiving address, and that of a change address is
-- counted aside, since hw keeps none. A label waits for the row of its
-- address: that of a receiving address too far to generate is counted
-- among the addresses the wallet does not have yet, and a sync followed
-- by a second import places it.
--
-- The tags go to an address of either chain, besides the tags it has,
-- whether the wallet has generated the address or not
-- ("Haskoin.Wallet.Database.Tag".'tagChainAddress'). They do not wait
-- for a second import, as a label does. A tag may lock its coins, and a
-- wallet made again from its file must have that lock in place when its
-- first sync finds the coins. Before, the tags of a change address were
-- left for a second import, since hw generates no change address on the
-- word of a file: the sync then found the change of the savings with no
-- tag on its address, and any payment could spend it.
labelAddressRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> (DBAccount -> Maybe Address) -> ExceptT String (DB m) Imported
labelAddressRecord accounts report r readOn =
  case (recordLabel r, recordTagsRead r) of
    (Nothing, []) -> return (report, [])
    (labelM, tags) -> do
      placed <- placeAddress candidates readOn (parseKeypath =<< recordKeypath r)
      case placed of
        PlacedAt accId addr -> do
          labelled <- case labelM of
            Nothing -> return (report, [])
            Just _ | dBAddressInternal addr -> return (labelAside, [])
            Just label -> do
              _ <- setAddrLabel accId (dBAddressIndex addr) label
              let replaced = replacedBy label [dBAddressLabel addr]
              return
                ( report
                    { reportAddressLabels = reportAddressLabels report + 1,
                      reportAddressLabelsReplaced = reportAddressLabelsReplaced report + genericLength replaced
                    },
                  replaced
                )
          tagged tags (chainAddressOf addr) labelled
        PlacedOnChangeChain place -> tagged tags place (if isJust labelM then labelAside else report, [])
        PlacedTooFar place -> tagged tags place (if isJust labelM then labelTooFar else report, [])
        NotPlaced -> return (notInWallet, [])
  where
    -- The accounts the record names, or every account when it names none
    -- the wallet has. An address is of one account at most on a network,
    -- so what the record says only narrows the search.
    candidates = [(accId, acc) | (_, accId, acc) <- narrowed]
    narrowed = case accountsOfRecord accounts r of
      Just named@(_ : _) -> named
      _ -> accounts
    labelAside = report {reportChangeAddresses = reportChangeAddresses report + 1}
    notInWallet = report {reportAddressesMissing = reportAddressesMissing report + 1}
    labelTooFar = notInWallet {reportAddressesTooFar = reportAddressesTooFar report + 1}
    -- What was done so far, once the address has the tags of the record
    tagged [] _ imported = return imported
    tagged tags place imported = lift $ do
      tagChainAddress place tags
      takeTagSettings r
      return (first (\counted -> counted {reportAddressTags = reportAddressTags counted + 1}) imported)

-- | Where the address of a record is in the wallet
data Placement
  = -- | The row of an account has it
    PlacedAt !DBAccountId !DBAddress
  | -- | Its key path derives it on the change chain of an account, which
    -- has not generated it. hw generates no change address on the word of
    -- a file: it takes no label, and its tags need no row.
    PlacedOnChangeChain !ChainAddress
  | -- | Its key path derives it on the receiving chain of an account,
    -- beyond 'scanLimit' past the last used address, further than hw
    -- generates on the word of a file ('withinScanLimit')
    PlacedTooFar !ChainAddress
  | -- | Nowhere hw can tell
    NotPlaced

-- | Find the address of a record among the addresses of the accounts, read
-- in each, on its network and of its kind; or derive it from its key path
-- in the account whose key gives it, which then generates its receiving
-- chain up to it, as the wallet that wrote the file had it, within
-- 'scanLimit' of its last used address ('withinScanLimit'): a file names
-- any path, and hw does not generate for days on its word.
--
-- An address that is derived and not generated is given with its
-- account, its chain and its index, which is what its tags need. Its key
-- path is checked first: the account derives one address at that path,
-- and it must be the address of the record.
placeAddress :: (MonadUnliftIO m) => [(DBAccountId, DBAccount)] -> (DBAccount -> Maybe Address) -> Maybe SoftPath -> ExceptT String (DB m) Placement
placeAddress accounts readIn keypathM = do
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
          derivedM <- liftIO $ deriveChainAddress (accountKind acc) (accountXPubKey acc) (addrDeriv addrType) index
          if derivedM /= Just addr
            then return Nothing
            else do
              text <- liftEither $ addrToTextE (accountNetwork acc) addr
              let place = ChainAddress accId (isInternal addrType) (fromIntegral index) text
              case addrType of
                AddrInternal -> return (Just (PlacedOnChangeChain place))
                AddrExternal -> do
                  within <- lift $ withinScanLimit accId AddrExternal index
                  if not within
                    then return (Just (PlacedTooFar place))
                    else do
                      extendChainTo accId AddrExternal (fromIntegral index + 1)
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
--
-- The tags of the record go where its label goes, besides the tags the
-- transaction has there. A pending transaction is tagged under its
-- nosigHash, as tag does. A record with neither a label nor a tag sets
-- nothing.
labelTxRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> TxHash -> ExceptT String (DB m) Imported
labelTxRecord accounts report r tid =
  case (recordLabel r, recordTagsRead r) of
    (Nothing, []) -> return (report, [])
    (labelM, tags) -> do
      pendingM <- findPendingTx tid
      case pendingM of
        Just held | maybe (onNetwork held.pendingTxPool) (held.pendingTxPool `elem`) named -> do
          lift $ forM_ labelM (setPendingTxLabel held)
          lift $ tagTx held.pendingTxPool (nosigHash (txSignDataTx held.pendingTxData)) tags
          lift $ takeTagSettings r
          return (set labelM tags [txSignDataLabel held.pendingTxData])
        _ -> case named of
          Just [] -> return (report {reportOtherAccounts = reportOtherAccounts report + 1}, [])
          Just ofRecord -> setIn ofRecord labelM tags
          Nothing -> do
            pools <- filter onNetwork <$> lift allPools
            holders <- filterM (\pool -> lift (isJust <$> P.get (txInfoKey pool tid))) pools
            if null holders
              then return (report {reportTxsMissing = reportTxsMissing report + 1}, [])
              else setIn holders labelM tags
  where
    -- The pools of the accounts the origin of the record names, on its
    -- network when it names one, or Nothing when it has no origin
    named = case recordOrigin r of
      Nothing -> Nothing
      Just _ -> nub . map (\(_, _, acc) -> accountPool acc) <$> accountsOfRecord accounts r
    -- Whether a pool is on the network the record names, any pool when
    -- it names none
    onNetwork pool = maybe True (== poolNetworkName pool) (recordNetwork r)
    -- The report once the label and the tags were set in the portfolios
    -- that held these labels, one for each, with those the label replaced
    set labelM tags held =
      ( report
          { reportTxLabels = reportTxLabels report + (if isJust labelM then n else 0),
            reportTxTags = reportTxTags report + (if null tags then 0 else n),
            reportTxLabelsReplaced = reportTxLabelsReplaced report + genericLength replaced
          },
        replaced
      )
      where
        n = genericLength held
        replaced = maybe [] (`replacedBy` held) labelM
    setIn pools labelM tags = do
      held <- forM pools $ \pool -> lift $ do
        old <- txLabel pool tid
        forM_ labelM (setTxLabel pool tid)
        tagTx pool tid tags
        return old
      lift $ takeTagSettings r
      return (set labelM tags held)

-- | Lock or unlock the coin of a record as its spendable flag says, when
-- the wallet has the coin, in the accounts the record names or in any. A
-- record with a label alone is left aside, whether the wallet has the
-- coin or not: hw keeps no label on a coin, so no sync would place it.
lockOutputRecord :: (MonadUnliftIO m) => [(Portfolio, DBAccountId, DBAccount)] -> ImportReport -> Record -> OutPoint -> ExceptT String (DB m) ImportReport
lockOutputRecord accounts report r outpoint = do
  rows <- lift $ coinRows outpoint
  let named = map (\(_, accId, _) -> accId) <$> accountsOfRecord accounts r
      held = [row | row <- rows, maybe True (dBCoinAccount row `elem`) named]
  case (recordSpendable r, held) of
    (Nothing, _) -> return report {reportNotKept = reportNotKept report + 1}
    (_, []) -> return report {reportCoinsMissing = reportCoinsMissing report + 1}
    (Just spendable, _) -> do
      forM_ held $ \row -> setLockedByUser (dBCoinAccount row) outpoint (not spendable)
      return $
        if spendable
          then report {reportCoinsUnlocked = reportCoinsUnlocked report + 1}
          else report {reportCoinsLocked = reportCoinsLocked report + 1}

{- The file show shows -}

-- | A file of a wallet as show shows it, before the user imports it: the
-- report importwallet would answer with, and the records of the file that
-- importwallet would do something with, laid out by account. The report
-- gives each wallet of the file as importwallet would leave it, and
-- counts what is set, what the wallet does not have yet and what is left
-- aside. show prints those counts in the words of importwallet, so the
-- user reads before the import the answer it would give.
--
-- A record goes under the account of the file whose origin and network it
-- names. This reads only what the file says. importwallet places a record
-- in the wallet itself, by the address or the transaction, and may place
-- one show shows apart.
data FileShown = FileShown
  { -- | The report of the import of the file, which was undone
    shownReport :: !ImportReport,
    -- | Each account of the file, in its order, with the records of the
    -- labels and locks hw keeps. show leaves out those of the networks it
    -- hides, which the report still counts.
    shownAccounts :: ![(AccountImport, [FileRecord])],
    -- | The records of labels and locks that name no account of the file
    shownOthers :: ![FileRecord],
    -- | The tags of the file that nothing has, which its key records
    -- name (@namingUnusedTags@), each once, in the order of the file.
    -- importwallet makes them tags of the database whatever it is
    -- narrowed to, so they show whatever the view is narrowed to.
    shownTagsNotUsed :: ![FileTag]
  }
  deriving (Eq, Show)

-- | A record of a file as show shows it: with the labels of the database
-- that importwallet would replace with its label, each once, and with
-- what importwallet would make of the record. The user reads there,
-- before the import, which label of their own the file takes the place
-- of, and which records name nothing of their wallet: the labels of a
-- file of another wallet read like any others. A transaction has a label
-- in each portfolio that holds it, so one record may replace several.
data FileRecord = FileRecord
  { fileRecord :: !Record,
    fileRecordReplaces :: ![Text],
    fileRecordFate :: !RecordFate
  }
  deriving (Eq, Show)

-- | A record of a file in JSON: the object of its line, as the file has
-- it, with the labels importwallet would replace under "replaces", and
-- what importwallet would make of it under "status"
instance Json.ToJSON FileRecord where
  toJSON shown =
    Json.object $
      recordFields (fileRecord shown)
        <> ["replaces" .= fileRecordReplaces shown, "status" .= fileRecordFate shown]

instance Json.FromJSON FileRecord where
  parseJSON value =
    Json.withObject "record" (\o -> FileRecord <$> Json.parseJSON value <*> o .: "replaces" <*> o .: "status") value

-- | What importwallet would make of a record, as --json writes it: "set",
-- "notinwallet", "otheraccount" or "ownaddress"
instance Json.ToJSON RecordFate where
  toJSON fate = Json.String $ case fate of
    FateSet -> "set"
    FateNotInWallet -> "notinwallet"
    FateOtherAccount -> "otheraccount"
    FateOwnAddress -> "ownaddress"

instance Json.FromJSON RecordFate where
  parseJSON = Json.withText "record status" $ \case
    "set" -> return FateSet
    "notinwallet" -> return FateNotInWallet
    "otheraccount" -> return FateOtherAccount
    "ownaddress" -> return FateOwnAddress
    _ -> fail "Unknown record status"

-- | A tag as a record of a file names it: with the colour the record
-- gives it, if any, and with whether the record says that it locks its
-- coins. It is what importwallet would set, on a tag that has no colour
-- yet and does not lock yet.
data FileTag = FileTag
  { fileTag :: !ShownTag,
    fileTagLocksCoins :: !Bool
  }
  deriving (Eq, Show)

-- | A tag of a file in JSON: its name, its colour and whether it locks
-- its coins, under the keys the answer of tags gives a tag
instance Json.ToJSON FileTag where
  toJSON tag =
    Json.object
      [ "name" .= shownTag (fileTag tag),
        "color" .= shownTagColor (fileTag tag),
        "lockscoins" .= fileTagLocksCoins tag
      ]

instance Json.FromJSON FileTag where
  parseJSON = Json.withObject "FileTag" $ \o ->
    FileTag <$> (ShownTag <$> o .: "name" <*> o .:? "color") <*> o .: "lockscoins"

-- | The tags a record of a file names, those that are tags, in its
-- order: what show shows of a file, before the wallet takes anything of
-- it. A colour hw does not know is no colour.
recordFileTags :: Record -> [FileTag]
recordFileTags r =
  [ FileTag (ShownTag tag (readTagColor =<< Map.lookup (tagText tag) colours)) (tagText tag `elem` locks)
    | tag <- recordTagsRead r
  ]
  where
    colours = fromMaybe Map.empty (recordTagColors r)
    locks = fromMaybe [] (recordTagLocks r)

-- | Whether a file holds anything a file of a wallet holds: a key or a
-- record, even one of a type the BIP does not define. A file of other
-- lines alone is no file of a wallet.
holdsAnything :: WalletFile -> Bool
holdsAnything file = not (null (fileKeys file) && null (fileRecords file) && fileOtherTypes file == 0)

-- | Show a file of a wallet. The file is imported into the database as
-- importwallet imports it, then that is undone ('undoneAfter'), so that
-- the database keeps nothing. Each wallet and each account shows as
-- importwallet would leave it: created, found, added or left aside, with
-- the reasons importwallet gives, each description of the file with the
-- one kept in its place, each label with those it would replace, and each
-- record with what importwallet would make of it. The report of the
-- import comes with them. A file that importwallet would refuse is
-- refused with its refusal, and so is a scope the file cannot answer
-- (@checkScopeHeld@).
--
-- Importing the file and undoing it gives the very answer importwallet
-- would give, without its rules written twice, once to do and once to
-- tell what it would do. The two could not drift apart.
fileShown :: (MonadUnliftIO m) => ImportScope -> WalletFile -> ExceptT String (DB m) FileShown
fileShown scope file = do
  (report, records) <- liftEither =<< lift (undoneAfter (runExceptT (importTelling scope file)))
  return (sortRecords file report records)

-- | Lay out the records of a file that show lists under the accounts of
-- its keys, given the report of its import, which says what became of
-- each wallet and of each account
sortRecords :: WalletFile -> ImportReport -> [FileRecord] -> FileShown
sortRecords file report records =
  FileShown
    { shownReport = report,
      shownAccounts = [(outcome, [shown | (Just at, shown) <- placed, at == i]) | (i, outcome) <- zip [0 ..] (reportAccounts report)],
      shownOthers = [shown | (Nothing, shown) <- placed],
      shownTagsNotUsed = nubBy (\a b -> shownTag (fileTag a) == shownTag (fileTag b)) (concatMap (recordFileTags . snd) (fileKeys file))
    }
  where
    placed = [(accountOf (fileRecord shown), shown) | shown <- records]
    -- What names each account of the file: its network, and its origin,
    -- from the fingerprint of its wallet
    names = accountName <$> reportAccounts report
    accountName :: AccountImport -> Maybe (Text, Maybe Origin)
    accountName outcome = do
      ref <- importedAccount outcome
      let place = accountRefPlace ref
          fingerprintM = listToMaybe [walletImportFingerprint wallet | wallet <- reportWallets report, walletImportIndex wallet == refWallet (placePortfolio place)]
      return (cs (placeNetwork place).name, (\fingerprint -> accountOrigin (accountRefKind ref) fingerprint (accountRefPath ref)) <$> fingerprintM)
    -- The place of the account of a record among the accounts of the
    -- file: the one on its network with its origin
    accountOf :: Record -> Maybe Int
    accountOf r = do
      origin <- parseOrigin =<< recordOrigin r
      let matches (netName, originM) = originM == Just origin && maybe True (== netName) (recordNetwork r)
      elemIndex True (maybe False matches <$> names)

-- | The keys of a file as the --json answer of show writes them: those of
-- the answer of importwallet ('reportPairs'), with the records of each
-- account in its object ("records"), then the records of no account of
-- the file, under "records" too, and the tags that nothing has
-- ("unusedtags")
fileShownPairs :: FileShown -> [Pair]
fileShownPairs shown =
  [pair | pair@(key, _) <- reportPairs (shownReport shown), key /= "accounts"]
    <> [ "accounts" .= [Json.object (accountImportPairs outcome <> ["records" .= records]) | (outcome, records) <- shownAccounts shown],
         "records" .= shownOthers shown,
         "unusedtags" .= shownTagsNotUsed shown
       ]

-- | What show says of a file, read from the keys 'fileShownPairs' writes
parseFileShown :: Json.Object -> Parser FileShown
parseFileShown o =
  FileShown
    <$> parseReport o
    <*> (mapM shownAccount =<< o .: "accounts")
    <*> o .: "records"
    <*> o .: "unusedtags"
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
    "addresstags" .= reportAddressTags report,
    "txtags" .= reportTxTags report,
    "contacts" .= reportContacts report,
    "contactsown" .= reportContactsOwn report,
    "addresslabelsreplaced" .= reportAddressLabelsReplaced report,
    "txlabelsreplaced" .= reportTxLabelsReplaced report,
    "contactlabelsreplaced" .= reportContactLabelsReplaced report,
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

-- | The report of an import, read from the keys 'reportPairs' writes
parseReport :: Json.Object -> Parser ImportReport
parseReport o =
  ImportReport
    <$> o .: "wallets"
    <*> (mapM parseAccountImport =<< o .: "accounts")
    <*> o .: "walletlabels"
    <*> o .: "portfoliolabels"
    <*> o .: "addresslabels"
    <*> o .: "txlabels"
    <*> o .:? "addresstags" .!= 0
    <*> o .:? "txtags" .!= 0
    <*> o .:? "contacts" .!= 0
    <*> o .:? "contactsown" .!= 0
    <*> o .: "addresslabelsreplaced"
    <*> o .: "txlabelsreplaced"
    <*> o .: "contactlabelsreplaced"
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

-- | The label a file gives a wallet or a portfolio, in the object of the
-- wallet or of the portfolio: the label under "label", and under
-- "keptdescription" the description kept in its place. Both are null
-- when the file gives no label.
labelImportPairs :: Maybe LabelImport -> [Pair]
labelImportPairs labelM =
  [ "label" .= (labelGiven <$> labelM),
    "keptdescription" .= (labelKeptInstead =<< labelM)
  ]

parseLabelImport :: Json.Object -> Parser (Maybe LabelImport)
parseLabelImport o = do
  givenM <- o .:? "label"
  kept <- o .:? "keptdescription"
  return ((`LabelImport` kept) <$> givenM)

instance Json.ToJSON PortfolioImport where
  toJSON p =
    Json.object $
      [ "portfolio" .= portfolioImportIndex p,
        "created" .= portfolioImportCreated p
      ]
        <> labelImportPairs (portfolioImportLabel p)

instance Json.FromJSON PortfolioImport where
  parseJSON = Json.withObject "portfolio import" $ \o ->
    PortfolioImport <$> o .: "portfolio" <*> o .: "created" <*> parseLabelImport o

instance Json.ToJSON WalletImport where
  toJSON w =
    Json.object $
      [ "wallet" .= walletImportIndex w,
        "fingerprint" .= walletImportFingerprint w,
        "created" .= walletImportCreated w,
        "portfolios" .= walletImportPortfolios w
      ]
        <> labelImportPairs (walletImportLabel w)

instance Json.FromJSON WalletImport where
  parseJSON = Json.withObject "wallet import" $ \o ->
    WalletImport <$> o .: "wallet" <*> o .: "fingerprint" <*> o .: "created" <*> parseLabelImport o <*> o .: "portfolios"

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
