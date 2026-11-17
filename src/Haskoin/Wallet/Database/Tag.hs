{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The tags of the database: the words the user puts on addresses and
-- on transactions, and which of them each address and each transaction
-- has. "Haskoin.Wallet.Tags" gives the rules of the word. The tables are
-- described with the others ("Haskoin.Wallet.Schema.V5").
--
-- = A tag is of the whole database
--
-- A tag has one row, with its colour, whatever it is put on. It is not
-- of a portfolio: the user who tags addresses kyc in two portfolios means
-- one group, with one colour. The row is made the first time the tag is
-- used or coloured ('ensureTag'), and stays until the user deletes the
-- tag ('deleteTag'), even when nothing has it any more. A tag that
-- vanished with its last use would lose its colour, and the user who
-- puts it back later would find it grey.
--
-- The word of a tag is the key of its row, and every row that says what
-- has the tag names it by that word. A tag has no number of its own: the
-- word is what the user types and what the files carry, and a number
-- would need a join in every listing. So a tag is renamed by writing the
-- new word in each of those rows ('renameTag'). That is rare, and the
-- listings are not.
--
-- = What has tags
--
-- A contact of the address book has tags too, which
-- "Haskoin.Wallet.Database.Contact" keeps. They are counted, renamed and
-- deleted here with the others ('tagUses', 'renameTag', 'deleteTag'),
-- since a tag is one word with one colour whatever has it.
--
-- An address of an account has tags, a receiving address or a change
-- address alike. A change address takes no label, since the label of its
-- transaction names its payment. A tag says something else: which group
-- the coins at the address belong to. The change of a payment made with
-- coins of a group belongs to that group too, and coin control by tag
-- must be able to find it. So a new change address takes the tags that
-- every coin of its payment has ('sharedAddressTags') when the user asks:
-- as the payment is made, with --tag-change, or afterwards, with tag
-- --with-change ("Haskoin.Wallet.Database.ChangeChain"). Neither is done
-- unasked, since a tag may say something of an address alone.
--
-- A transaction of a pool has tags, kept apart from its summary as its
-- label is, so that the sync never touches them. A pending transaction
-- is tagged under its nosigHash, which names it before and after it is
-- signed. The tags move to its transaction id once the network has it
-- ('moveTxTags').
--
-- = The contacts of a transaction
--
-- A contact of the address book has tags too, and txs keeps the
-- transactions at the contacts of a tag as it keeps those at the tagged
-- addresses of the portfolio: --address-tag asks for both. A contact is
-- an address of somebody else, so the rows of the addresses of the
-- portfolio do not hold it. A second table holds the addresses of others
-- each transaction shows ('writeTxOtherAddresses'), written by the same
-- one place.
--
-- = Filters
--
-- A listing keeps the rows that have any one of the tags asked for. The
-- conditions here are parts of the query of each listing, so that the
-- database does the filtering, in the order of the listing, and reads no
-- row the listing does not show.
--
-- A listing also keeps the rows without a tag when asked ('TagFilter',
-- 'TxTagFilter'). That is the other end of the work of sorting: once the
-- user has tagged what they know, the rows without a tag are those left
-- to look at. Nothing in the database names them, since a row without a
-- tag has no row in the tables of tags. So they are found by asking
-- about each row of the listing in turn, from the key of the tags.
module Haskoin.Wallet.Database.Tag
  ( -- * Tags
    ensureTag,
    heldTag,
    requireTags,
    noSuchTag,
    setTagColor,
    allTags,
    TagUse (..),
    tagUses,
    TagDeleted (..),
    deleteTag,
    renameTag,
    tagsStarting,
    shownTags,
    storedTag,

    -- * The tags of addresses
    tagAddress,
    untagAddress,
    addressTags,
    sharedAddressTags,
    setAddressTags,
    clearAddressTags,
    addressTagsAt,
    addressTagsAmong,
    tagsOfAddressRows,
    taggedAddressRows,
    taggedAddressTexts,
    accountAddressTags,
    deleteAccountTags,

    -- * The tags of transactions
    tagTx,
    untagTx,
    txTags,
    poolTxTags,
    moveTxTags,
    copyTxTags,
    deleteTxTags,
    deletePortfolioTxTags,

    -- * The addresses of transactions
    writeTxAddresses,
    writeTxOtherAddresses,
    deleteTxAddresses,
    deletePortfolioTxAddresses,

    -- * Filters
    TagFilter (..),
    noTagFilter,
    isTagFiltered,
    keepsTagged,
    addressKept,
    coinAtKept,
    TxTagFilter (..),
    noTxTagFilter,
    isTxFiltered,
    txFilterOfRows,
    txKept,
    txsKeptAmong,
    txidsKept,
    fewKeptTxids,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Either (rights)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.Esqueleto.Legacy (InnerJoin (..), SqlExpr, SqlQuery, Value (..), asc, countRows, distinct, exists, from, groupBy, in_, limit, notExists, on, orderBy, select, val, valList, where_, (&&.), (==.), (^.), (||.))
import qualified Database.Persist as P
import Database.Persist.Sql (Single (..), rawExecute, rawSql)
import Haskoin
import Haskoin.Wallet.Database
import Haskoin.Wallet.Tags
import Haskoin.Wallet.Util (rowLimit)
import Numeric.Natural (Natural)

{- Tags -}

-- | Make sure a tag has its row, without a colour when the row is new.
-- Every command that puts a tag on something calls this first, so a tag
-- that something has always has a row.
ensureTag :: (MonadUnliftIO m) => Tag -> DB m ()
ensureTag tag = do
  heldM <- P.get (DBTagKey (tagText tag))
  case heldM of
    Just _ -> return ()
    Nothing -> do
      now <- liftIO getCurrentTime
      P.insert_ (DBTag (tagText tag) "" now)

-- | A tag of the database as the answers show it, with its colour, or
-- Nothing for a word that is no tag of the database
heldTag :: (MonadUnliftIO m) => Tag -> DB m (Maybe ShownTag)
heldTag tag = fmap (ShownTag tag . readTagColor . dBTagColor) <$> P.get (DBTagKey (tagText tag))

-- | The tags as the answers show them, each with the colour it has now,
-- in the order given. Each must be a tag of the database: a listing that
-- filters by a tag, or a payment that spends from one, is refused a word
-- that is no tag. The user most likely mistyped it, and an empty answer
-- would not say so.
requireTags :: (MonadUnliftIO m) => [Tag] -> ExceptT String (DB m) [ShownTag]
requireTags tags = forM tags $ \tag -> do
  heldM <- lift (heldTag tag)
  maybe (throwError (noSuchTag tag)) return heldM

-- | The refusal of a word that is no tag of the database
noSuchTag :: Tag -> String
noSuchTag tag = "There is no tag " <> cs (tagText tag) <> ". hw tags lists the tags."

-- | Give a tag a colour, or remove its colour with Nothing. The tag gets
-- its row if it had none: a tag can be coloured before it is used.
setTagColor :: (MonadUnliftIO m) => Tag -> Maybe TagColor -> DB m ()
setTagColor tag colorM = do
  ensureTag tag
  P.update (DBTagKey (tagText tag)) [DBTagColor P.=. maybe "" colorName colorM]

-- | Every tag of the database with its colour, in the order of the
-- alphabet
allTags :: (MonadUnliftIO m) => DB m [ShownTag]
allTags = do
  rows <- P.selectList [] [P.Asc DBTagName]
  return (rights [storedTag (P.entityVal row) | row <- rows])

-- | A row of a tag as the answers show it
storedTag :: DBTag -> Either String ShownTag
storedTag row = (\tag -> ShownTag tag (readTagColor (dBTagColor row))) <$> readTag (dBTagName row)

-- | The given tags with the colours they have now, in the order given: a
-- tag without a row is shown without a colour
shownTags :: (MonadUnliftIO m) => [Tag] -> DB m [ShownTag]
shownTags tags = forM tags $ \tag -> maybe (plainTag tag) id <$> heldTag tag

-- | A tag with how many addresses, how many transactions and how many
-- contacts of the address book have it, in the whole database: what tags
-- lists. A transaction that two portfolios hold counts once for each.
data TagUse = TagUse
  { tagUseTag :: !ShownTag,
    tagUseAddresses :: !Natural,
    tagUseTxs :: !Natural,
    tagUseContacts :: !Natural
  }
  deriving (Eq, Show)

-- | A tag with its uses in JSON: its name and its colour, as every tag
-- of an answer, with the three counts beside them
instance ToJSON TagUse where
  toJSON use =
    object
      [ "name" .= shownTag use.tagUseTag,
        "color" .= shownTagColor use.tagUseTag,
        "addresses" .= use.tagUseAddresses,
        "txs" .= use.tagUseTxs,
        "contacts" .= use.tagUseContacts
      ]

instance FromJSON TagUse where
  parseJSON = withObject "TagUse" $ \o ->
    TagUse <$> (ShownTag <$> o .: "name" <*> o .:? "color") <*> o .: "addresses" <*> o .: "txs" <*> o .: "contacts"

-- | Every tag of the database with its uses, in the order of the
-- alphabet. The three counts read the index of each table by tag.
tagUses :: (MonadUnliftIO m) => DB m [TagUse]
tagUses = do
  tags <- allTags
  addresses <- select . from $ \t -> do
    groupBy (t ^. DBAddressTagTag)
    return (t ^. DBAddressTagTag, countRows)
  txs <- select . from $ \t -> do
    groupBy (t ^. DBTxTagTag)
    return (t ^. DBTxTagTag, countRows)
  contacts <- select . from $ \t -> do
    groupBy (t ^. DBContactTagTag)
    return (t ^. DBContactTagTag, countRows)
  let counts rows = Map.fromList [(name, fromIntegral (count :: Int)) | (Value name, Value count) <- rows]
      usesIn byName shown = Map.findWithDefault 0 (tagText (shownTag shown)) byName
  return [TagUse shown (usesIn (counts addresses) shown) (usesIn (counts txs) shown) (usesIn (counts contacts) shown) | shown <- tags]

-- | What had a tag that was deleted: how many addresses, how many
-- transactions, and how many contacts of the address book
data TagDeleted = TagDeleted
  { deletedFromAddresses :: !Natural,
    deletedFromTxs :: !Natural,
    deletedFromContacts :: !Natural
  }
  deriving (Eq, Show)

-- | Delete a tag from the whole database: from every address and every
-- transaction that has it, in every portfolio, and from every contact of
-- the address book, with its colour. Answers how many of each had it.
deleteTag :: (MonadUnliftIO m) => Tag -> DB m TagDeleted
deleteTag tag = do
  let name = tagText tag
  addresses <- P.count [DBAddressTagTag P.==. name]
  txs <- P.count [DBTxTagTag P.==. name]
  contacts <- P.count [DBContactTagTag P.==. name]
  P.deleteWhere [DBAddressTagTag P.==. name]
  P.deleteWhere [DBTxTagTag P.==. name]
  P.deleteWhere [DBContactTagTag P.==. name]
  P.delete (DBTagKey name)
  return (TagDeleted (fromIntegral addresses) (fromIntegral txs) (fromIntegral contacts))

-- | Give everything that has the first tag the second tag in its place,
-- in the whole database: every address and every transaction, in every
-- portfolio, and every contact of the address book. The first tag is
-- then gone. A first tag the database does not have changes nothing.
--
-- When the second tag is new, this renames the first one. The tag keeps
-- its colour, and the time it was made.
--
-- When the second tag exists, this merges the first one into it. What
-- had the first has the second, and what had both has the second once.
-- The second keeps its colour. When it has none, it takes the colour of
-- the first, as a tag without a colour takes the one a file of a wallet
-- names for it.
--
-- The caller decides whether a merge is wanted: a merge cannot be taken
-- apart afterwards, since nothing says any more what had which tag.
--
-- The rows are moved by the database, two statements for each table,
-- which read the index of the table by tag. A tag on a hundred thousand
-- transactions is renamed without reading one row here.
renameTag :: (MonadUnliftIO m) => Tag -> Tag -> DB m ()
renameTag old new = do
  let oldKey = DBTagKey (tagText old)
      newKey = DBTagKey (tagText new)
  oldRowM <- P.get oldKey
  forM_ oldRowM $ \oldRow -> unless (old == new) $ do
    newRowM <- P.get newKey
    case newRowM of
      Nothing -> P.insert_ oldRow {dBTagName = tagText new}
      Just newRow ->
        when (Text.null (dBTagColor newRow)) $
          P.update newKey [DBTagColor P.=. dBTagColor oldRow]
    forM_ tablesOfTags $ \table -> do
      -- A row whose thing has the second tag already cannot take it
      -- again. "or ignore" leaves that row as it is, and the second
      -- statement deletes it.
      rawExecute
        ("update or ignore " <> table <> " set tag = ? where tag = ?")
        [P.toPersistValue (tagText new), P.toPersistValue (tagText old)]
      rawExecute
        ("delete from " <> table <> " where tag = ?")
        [P.toPersistValue (tagText old)]
    P.delete oldKey

-- | The tables that say what has which tag, by their names in the
-- database: the tags of the addresses, of the transactions, and of the
-- contacts. Each names its tag in a column tag.
tablesOfTags :: [Text]
tablesOfTags = ["d_b_address_tag", "d_b_tx_tag", "d_b_contact_tag"]

-- | At most the given number of tags that start with the text, in the
-- order of the alphabet: what the shell offers. The text is compared as
-- it is, without LIKE, whose wildcards a typed word could hold.
tagsStarting :: (MonadUnliftIO m) => Text -> Natural -> DB m [Text]
tagsStarting start atMost = do
  rows <-
    rawSql
      "select name from d_b_tag where substr(name, 1, ?) = ? order by name limit ?"
      [P.toPersistValue (Text.length start), P.toPersistValue start, P.toPersistValue (rowLimit atMost :: Int)]
  return (unSingle <$> rows)

{- The tags of addresses -}

-- | Put tags on an address, besides those it has. A tag it has already
-- changes nothing.
tagAddress :: (MonadUnliftIO m) => DBAddress -> [Tag] -> DB m ()
tagAddress addr tags =
  forM_ tags $ \tag -> do
    ensureTag tag
    P.repsert
      (DBAddressTagKey (dBAddressAccount addr) (dBAddressInternal addr) (dBAddressIndex addr) (tagText tag))
      (DBAddressTag (dBAddressAccount addr) (dBAddressInternal addr) (dBAddressIndex addr) (dBAddressAddress addr) (tagText tag))

-- | Take tags off an address. Answers those it did not have, which the
-- caller refuses: the user most likely named another address or mistyped
-- the tag.
untagAddress :: (MonadUnliftIO m) => DBAddress -> [Tag] -> DB m [Tag]
untagAddress addr tags = do
  missing <- forM tags $ \tag -> do
    let key = DBAddressTagKey (dBAddressAccount addr) (dBAddressInternal addr) (dBAddressIndex addr) (tagText tag)
    heldM <- P.get key
    case heldM of
      Just _ -> [] <$ P.delete key
      Nothing -> return [tag]
  return (concat missing)

-- | The tags of an address, with their colours, in the order of the
-- alphabet
addressTags :: (MonadUnliftIO m) => DBAddress -> DB m [ShownTag]
addressTags addr = addressTagsAt (dBAddressAccount addr) (dBAddressAddress addr)

-- | The tags every one of the addresses has, in the order of the
-- alphabet: what the change of a payment that spends from them takes
-- ("Haskoin.Wallet.Database.PendingTx".'changeTakesTags'). A tag one of
-- them lacks is not shared, so an address without a tag leaves none, and
-- no address shares none.
sharedAddressTags :: (MonadUnliftIO m) => [DBAddress] -> DB m [Tag]
sharedAddressTags [] = return []
sharedAddressTags addrs = do
  tagsOfEach <- forM addrs (fmap (Set.fromList . map shownTag) . addressTags)
  return (Set.toAscList (foldr1 Set.intersection tagsOfEach))

-- | Give an address the tags, in place of those it has: a new change
-- address has the tags of the coins of its payment and no other. It may
-- have tags all the same before a payment takes it, as when importwallet
-- read them from the file of a wallet whose pending payments are not in
-- the database. They said something of another payment.
setAddressTags :: (MonadUnliftIO m) => DBAddress -> [Tag] -> DB m ()
setAddressTags addr tags = clearAddressTags addr >> tagAddress addr tags

-- | Take every tag off an address: a change address that a deleted
-- pending transaction gives back is new again, and says nothing of the
-- coins of the next payment that takes it
clearAddressTags :: (MonadUnliftIO m) => DBAddress -> DB m ()
clearAddressTags addr =
  P.deleteWhere
    [ DBAddressTagAccount P.==. dBAddressAccount addr,
      DBAddressTagInternal P.==. dBAddressInternal addr,
      DBAddressTagIndex P.==. dBAddressIndex addr
    ]

-- | The tags of the address of an account, by the text of the address as
-- hw stores it: what a coin shows, which names its address so
addressTagsAt :: (MonadUnliftIO m) => DBAccountId -> Text -> DB m [ShownTag]
addressTagsAt accId address = Map.findWithDefault [] address <$> addressTagsAmong [accId] [address]

-- | The tags of those of the addresses that have some, by the text of
-- each address as hw stores it, among the addresses of the accounts. The
-- accounts are those of one network, where an address is of one account
-- at most, so the text names one address. One query reads a page of a
-- listing, and a listing read whole asks 'inListBatch' addresses at a
-- time.
addressTagsAmong :: (MonadUnliftIO m) => [DBAccountId] -> [Text] -> DB m (Map Text [ShownTag])
addressTagsAmong accIds addresses = do
  found <- forM (chunksOf (fromIntegral inListBatch) addresses) $ \some ->
    select . from $ \(t `InnerJoin` g) -> do
      on (g ^. DBTagName ==. t ^. DBAddressTagTag)
      where_ $
        t ^. DBAddressTagAccount `in_` valList accIds
          &&. t ^. DBAddressTagAddress `in_` valList some
      orderBy [asc (t ^. DBAddressTagTag)]
      return (t ^. DBAddressTagAddress, g)
  return $
    Map.fromListWith
      (flip (<>))
      [(address, [shown]) | (Value address, row) <- concat found, Right shown <- [storedTag (P.entityVal row)]]

-- | The tags of each of the address rows that has some, by the account
-- and the text of each: the rows may be of several accounts, as those
-- balances lists
tagsOfAddressRows :: (MonadUnliftIO m) => [DBAddress] -> DB m (DBAddress -> [ShownTag])
tagsOfAddressRows rows = do
  let byAccount = Map.fromListWith (<>) [(dBAddressAccount row, [dBAddressAddress row]) | row <- rows]
  found <- forM (Map.toList byAccount) $ \(accId, addresses) ->
    (,) accId <$> addressTagsAmong [accId] addresses
  let tagsBy = Map.fromList found
  return $ \row -> Map.findWithDefault [] (dBAddressAddress row) (Map.findWithDefault Map.empty (dBAddressAccount row) tagsBy)

-- | The addresses of an account that have any of the tags, each once, by
-- its chain and its index, with its text: what a payment spends from, and
-- where its change may go back to
taggedAddressRows :: (MonadUnliftIO m) => DBAccountId -> [Tag] -> DB m [(Bool, Int, Text)]
taggedAddressRows accId tags = do
  rows <- select . distinct . from $ \t -> do
    where_ $
      t ^. DBAddressTagAccount ==. val accId
        &&. t ^. DBAddressTagTag `in_` valList (tagText <$> tags)
    orderBy [asc (t ^. DBAddressTagInternal), asc (t ^. DBAddressTagIndex)]
    return (t ^. DBAddressTagInternal, t ^. DBAddressTagIndex, t ^. DBAddressTagAddress)
  return [(internal, index, address) | (Value internal, Value index, Value address) <- rows]

-- | The text of every address of the accounts that has any of the tags,
-- each once
taggedAddressTexts :: (MonadUnliftIO m) => [DBAccountId] -> [Tag] -> DB m [Text]
taggedAddressTexts accIds tags =
  concat <$> forM accIds (\accId -> map (\(_, _, address) -> address) <$> taggedAddressRows accId tags)

-- | Every tagged address of an account with its tags, by its chain and
-- its index, in the order of the chains: what exportwallet writes
accountAddressTags :: (MonadUnliftIO m) => DBAccountId -> DB m [((Bool, Int, Text), [Tag])]
accountAddressTags accId = do
  rows <- P.selectList [DBAddressTagAccount P.==. accId] [P.Asc DBAddressTagInternal, P.Asc DBAddressTagIndex, P.Asc DBAddressTagTag]
  let tagged = [((dBAddressTagInternal row, dBAddressTagIndex row, dBAddressTagAddress row), [tag]) | P.Entity _ row <- rows, Right tag <- [readTag (dBAddressTagTag row)]]
  return (Map.toAscList (Map.fromListWith (flip (<>)) tagged))

-- | Delete the tags of every address of the accounts: those of a
-- portfolio that is deleted. The tags themselves stay.
deleteAccountTags :: (MonadUnliftIO m) => [DBAccountId] -> DB m ()
deleteAccountTags accIds = P.deleteWhere [DBAddressTagAccount P.<-. accIds]

{- The tags of transactions -}

-- | Put tags on a transaction of a pool, by its id, or by its nosigHash
-- while it is pending, besides those it has
tagTx :: (MonadUnliftIO m) => Pool -> TxHash -> [Tag] -> DB m ()
tagTx pool hash tags =
  forM_ tags $ \tag -> do
    ensureTag tag
    P.repsert
      (DBTxTagKey pool.poolPortfolio (poolNetworkName pool) (hashBytes hash) (tagText tag))
      (DBTxTag pool.poolPortfolio (poolNetworkName pool) (hashBytes hash) (tagText tag))

-- | Take tags off a transaction of a pool. Answers those it did not
-- have.
untagTx :: (MonadUnliftIO m) => Pool -> TxHash -> [Tag] -> DB m [Tag]
untagTx pool hash tags = do
  missing <- forM tags $ \tag -> do
    let key = DBTxTagKey pool.poolPortfolio (poolNetworkName pool) (hashBytes hash) (tagText tag)
    heldM <- P.get key
    case heldM of
      Just _ -> [] <$ P.delete key
      Nothing -> return [tag]
  return (concat missing)

-- | The tags of a transaction of a pool, with their colours, in the order
-- of the alphabet
txTags :: (MonadUnliftIO m) => Pool -> TxHash -> DB m [ShownTag]
txTags pool hash = do
  rows <- select . from $ \(t `InnerJoin` g) -> do
    on (g ^. DBTagName ==. t ^. DBTxTagTag)
    where_ $
      t ^. DBTxTagPortfolio ==. val pool.poolPortfolio
        &&. t ^. DBTxTagNetwork ==. val (poolNetworkName pool)
        &&. t ^. DBTxTagTxid ==. val (hashBytes hash)
    orderBy [asc (t ^. DBTxTagTag)]
    return g
  return (rights (storedTag . P.entityVal <$> rows))

-- | Every tagged transaction of a pool with its tags, in the order of
-- the ids: what exportwallet writes. A pending transaction is there under
-- its nosigHash.
poolTxTags :: (MonadUnliftIO m) => Pool -> DB m [(TxHash, [Tag])]
poolTxTags pool = do
  rows <- P.selectList [DBTxTagPortfolio P.==. pool.poolPortfolio, DBTxTagNetwork P.==. poolNetworkName pool] [P.Asc DBTxTagTxid, P.Asc DBTxTagTag]
  let tagged = [(hash, [tag]) | P.Entity _ row <- rows, Right hash <- [bytesToHash (dBTxTagTxid row)], Right tag <- [readTag (dBTxTagTag row)]]
  return (Map.toAscList (Map.fromListWith (flip (<>)) tagged))

-- | Give the tags of a transaction of a pool to another hash, and leave
-- none under the first: a pending transaction tagged under its nosigHash
-- gets its tags under its id once the network has it. The tags the
-- second hash has already stay.
moveTxTags :: (MonadUnliftIO m) => Pool -> TxHash -> TxHash -> DB m ()
moveTxTags pool fromHash toHash = unless (fromHash == toHash) $ do
  copyTxTags pool fromHash pool toHash
  deleteTxTags pool fromHash

-- | Give a transaction the tags of another one, besides its own: the
-- replacement bumpfee prepares gets the tags of the transaction it
-- replaces, as it gets its label
copyTxTags :: (MonadUnliftIO m) => Pool -> TxHash -> Pool -> TxHash -> DB m ()
copyTxTags fromPool fromHash toPool toHash = do
  tags <- map shownTag <$> txTags fromPool fromHash
  tagTx toPool toHash tags

-- | Take every tag off a transaction of a pool: a pending transaction
-- that is deleted leaves none behind
deleteTxTags :: (MonadUnliftIO m) => Pool -> TxHash -> DB m ()
deleteTxTags pool hash =
  P.deleteWhere
    [ DBTxTagPortfolio P.==. pool.poolPortfolio,
      DBTxTagNetwork P.==. poolNetworkName pool,
      DBTxTagTxid P.==. hashBytes hash
    ]

-- | Delete the tags of every transaction of a portfolio that is deleted
deletePortfolioTxTags :: (MonadUnliftIO m) => DBPortfolioId -> DB m ()
deletePortfolioTxTags portfolioKey = P.deleteWhere [DBTxTagPortfolio P.==. portfolioKey]

{- The addresses of transactions -}

-- | Write the addresses of the portfolio a transaction of the pool
-- spends from or pays, each by its account, its chain and its index, in
-- place of those written before. Only the write of a summary calls this,
-- with the addresses of that summary, so the rows always say what the
-- summary says.
writeTxAddresses :: (MonadUnliftIO m) => Pool -> TxHash -> [(DBAccountId, Bool, Int)] -> DB m ()
writeTxAddresses pool hash addresses = do
  let wanted = Set.fromList addresses
  held <- P.selectList (txAddressesOf pool hash) []
  let heldSet = Set.fromList [(dBTxAddressAccount row, dBTxAddressInternal row, dBTxAddressIndex row) | P.Entity _ row <- held]
  -- A summary written again most often names the same addresses: then
  -- nothing is written
  unless (heldSet == wanted) $ do
    deleteTxAddresses pool hash
    forM_ (Set.toList wanted) $ \(accId, internal, index) ->
      P.insert_ (DBTxAddress pool.poolPortfolio (poolNetworkName pool) (hashBytes hash) accId internal index)

-- | Write the addresses of others a transaction of the pool shows, by
-- the text of each as hw writes it on the network, in place of those
-- written before. Only the write of a summary calls this, with the
-- addresses of that summary, as it calls 'writeTxAddresses'.
--
-- Every such address is written, whether the address book has it or
-- not. The other choice was to write the contacts alone. A contact added
-- later would then find none of its past transactions, unless adding it
-- read the whole history, and two places would write the rows.
writeTxOtherAddresses :: (MonadUnliftIO m) => Pool -> TxHash -> [Text] -> DB m ()
writeTxOtherAddresses pool hash addresses = do
  let wanted = Set.fromList addresses
  held <- P.selectList (txOtherAddressesOf pool hash) []
  let heldSet = Set.fromList [dBTxOtherAddressAddress row | P.Entity _ row <- held]
  -- A summary written again most often names the same addresses: then
  -- nothing is written
  unless (heldSet == wanted) $ do
    P.deleteWhere (txOtherAddressesOf pool hash)
    forM_ (Set.toList wanted) $ \address ->
      P.insert_ (DBTxOtherAddress pool.poolPortfolio (poolNetworkName pool) (hashBytes hash) address)

-- | The rows of the addresses of others of a transaction of a pool
txOtherAddressesOf :: Pool -> TxHash -> [P.Filter DBTxOtherAddress]
txOtherAddressesOf pool hash =
  [ DBTxOtherAddressPortfolio P.==. pool.poolPortfolio,
    DBTxOtherAddressNetwork P.==. poolNetworkName pool,
    DBTxOtherAddressTxid P.==. hashBytes hash
  ]

-- | The rows of the addresses of a transaction of a pool
txAddressesOf :: Pool -> TxHash -> [P.Filter DBTxAddress]
txAddressesOf pool hash =
  [ DBTxAddressPortfolio P.==. pool.poolPortfolio,
    DBTxAddressNetwork P.==. poolNetworkName pool,
    DBTxAddressTxid P.==. hashBytes hash
  ]

-- | Delete the rows of the addresses of a transaction of a pool, those
-- of the portfolio and those of others, with a summary that is deleted
deleteTxAddresses :: (MonadUnliftIO m) => Pool -> TxHash -> DB m ()
deleteTxAddresses pool hash = do
  P.deleteWhere (txAddressesOf pool hash)
  P.deleteWhere (txOtherAddressesOf pool hash)

-- | Delete the rows of the addresses of every transaction of a portfolio
-- that is deleted, those of the portfolio and those of others
deletePortfolioTxAddresses :: (MonadUnliftIO m) => DBPortfolioId -> DB m ()
deletePortfolioTxAddresses portfolioKey = do
  P.deleteWhere [DBTxAddressPortfolio P.==. portfolioKey]
  P.deleteWhere [DBTxOtherAddressPortfolio P.==. portfolioKey]

{- Filters -}

-- | The rows a listing keeps by the tags of their address: the addresses
-- of addresses and of balances, and the coins of coins, which have the
-- tags of their address. contacts keeps its contacts by their own tags
-- the same way. A row stays when its address has any one of the tags, or
-- when it has no tag at all and those are asked for. The two add up, as
-- several tags do: a listing never keeps fewer rows for one more option.
data TagFilter = TagFilter
  { -- | The tags asked for (--tag)
    keptTags :: ![Tag],
    -- | Whether the rows without a tag are asked for (--untagged)
    keptUntagged :: !Bool
  }
  deriving (Eq, Show)

-- | The filter that keeps every row
noTagFilter :: TagFilter
noTagFilter = TagFilter [] False

-- | Whether the filter leaves any row out
isTagFiltered :: TagFilter -> Bool
isTagFiltered kept = kept.keptUntagged || not (null kept.keptTags)

-- | Whether the filter keeps a row that has the tags given: for a listing
-- that is read whole and filtered once read, as the address book is
keepsTagged :: TagFilter -> [Tag] -> Bool
keepsTagged kept tags =
  not (isTagFiltered kept)
    || any (`elem` kept.keptTags) tags
    || (kept.keptUntagged && null tags)

-- | Whether the filter keeps something, given which rows of the tags of
-- addresses are its own: it has one of the tags asked for, or it has no
-- tag when those are asked for. A part that is not asked for is left out
-- of the query, which would otherwise ask it of every row it walks past.
keptByAddressTags :: TagFilter -> (SqlExpr (P.Entity DBAddressTag) -> SqlExpr (Value Bool)) -> SqlExpr (Value Bool)
keptByAddressTags kept own = case (null kept.keptTags, kept.keptUntagged) of
  (True, False) -> val True
  (False, False) -> hasOne
  (True, True) -> hasNone
  (False, True) -> hasOne ||. hasNone
  where
    hasOne = exists . from $ \t -> where_ (own t &&. t ^. DBAddressTagTag `in_` valList (tagText <$> kept.keptTags))
    hasNone = notExists . from $ \t -> where_ (own t)

-- | Whether the filter keeps an address: a condition of the query of
-- addresses. The key of the tags starts with that of the address, so it
-- answers for each address without reading a row.
addressKept :: TagFilter -> SqlExpr (P.Entity DBAddress) -> SqlExpr (Value Bool)
addressKept kept a =
  keptByAddressTags kept $ \t ->
    t ^. DBAddressTagAccount ==. a ^. DBAddressAccount
      &&. t ^. DBAddressTagInternal ==. a ^. DBAddressInternal
      &&. t ^. DBAddressTagIndex ==. a ^. DBAddressIndex

-- | Whether the filter keeps a coin, by the tags of its address: a
-- condition of the query of coins and of balances. The index of the tags
-- by account and address answers it for each coin without reading a row.
coinAtKept :: TagFilter -> SqlExpr (P.Entity DBCoin) -> SqlExpr (Value Bool)
coinAtKept kept c =
  keptByAddressTags kept $ \t ->
    t ^. DBAddressTagAccount ==. c ^. DBCoinAccount
      &&. t ^. DBAddressTagAddress ==. c ^. DBCoinAddress

-- | The transactions a listing of transactions keeps. A transaction
-- stays when it has any of the tags of transactions asked for, or when
-- any address of the portfolio it spends from or pays has any of the
-- tags of addresses asked for, or when any contact of the address book
-- it pays or was paid from has any of those, or when it is left to sort
-- and those are asked for. With several of these, the listing is the
-- union: the transactions that match any one of them.
--
-- = The transactions left to sort
--
-- A transaction is left to sort when it has no tag of its own, and when
-- no contact it pays or was paid from has one. The tags of a contact
-- count because they say what its payments are for, as the journal reads
-- them ("Haskoin.Wallet.Cli.Journal"): a payment to a landlord tagged
-- rent is sorted the day the landlord is tagged, and tagging each of its
-- payments again would be the work the contact was tagged to save. With
-- the tags of the transactions alone, the list would never empty for a
-- user who sorts by contact.
--
-- The tags of the addresses of the portfolio do not count. They say
-- which group the coins belong to, such as kyc or cold, and not what a
-- payment is for: a payment from a kyc address is still to sort.
data TxTagFilter = TxTagFilter
  { -- | The tags of transactions asked for (--tag)
    keptTxTags :: ![Tag],
    -- | The tags of addresses asked for (--address-tag): those of the
    -- addresses of the portfolio, and those of the contacts of the
    -- address book
    keptAddressTags :: ![Tag],
    -- | Whether the transactions left to sort are asked for (--untagged)
    keptUntaggedTxs :: !Bool
  }
  deriving (Eq, Show)

-- | The filter that keeps every transaction
noTxTagFilter :: TxTagFilter
noTxTagFilter = TxTagFilter [] [] False

-- | Whether the filter leaves any transaction out
isTxFiltered :: TxTagFilter -> Bool
isTxFiltered kept = kept.keptUntaggedTxs || not (null kept.keptTxTags && null kept.keptAddressTags)

-- | The filter of transactions that goes with a filter of rows: the
-- tags asked for are those of the transactions, and the rows without a
-- tag are the transactions left to sort. --tag and --untagged mean this
-- in txs, as they mean the other in addresses. exportsheet takes the two
-- options once and gives them to every tab, each in the meaning of its
-- listing.
txFilterOfRows :: TagFilter -> TxTagFilter
txFilterOfRows kept = TxTagFilter kept.keptTags [] kept.keptUntagged

-- | Whether the filter keeps a summary: a condition of the query of txs.
-- Each part is answered from a primary key, that of the tags of the
-- transaction, or that of its addresses and then that of their tags,
-- for the addresses of the portfolio and for the contacts alike, so no
-- summary is decoded to tell. A part that is not asked for is left out
-- of the query, which would otherwise ask it of every summary it walks
-- past.
txKept :: TxTagFilter -> SqlExpr (P.Entity DBTxInfo) -> SqlExpr (Value Bool)
txKept kept t = case parts of
  [] -> val True
  _ -> foldr1 (||.) parts
  where
    parts =
      [exists (ownTags (Just kept.keptTxTags)) | not (null kept.keptTxTags)]
        <> [atTagged | not (null kept.keptAddressTags)]
        <> [leftToSort | kept.keptUntaggedTxs]
    -- At a tagged address of the portfolio, or at a tagged contact. The
    -- address of the portfolio is asked first: the database asks about
    -- the contacts only for a transaction the first leaves out.
    atTagged = atTaggedAddress ||. atTaggedContact
    -- Without a tag of its own, and at no contact that has one. Most
    -- transactions with a tag have their own, which is asked first.
    leftToSort = notExists (ownTags Nothing) &&. notExists atContactWithTag
    -- The rows of the tags of the transaction: those of the tags given,
    -- or of any tag
    ownTags :: Maybe [Tag] -> SqlQuery ()
    ownTags wanted =
      from $ \g -> do
        where_ $
          g ^. DBTxTagPortfolio ==. t ^. DBTxInfoPortfolio
            &&. g ^. DBTxTagNetwork ==. t ^. DBTxInfoNetwork
            &&. g ^. DBTxTagTxid ==. t ^. DBTxInfoTxid
        forM_ wanted $ \tags -> where_ (g ^. DBTxTagTag `in_` valList (tagText <$> tags))
    -- The addresses of others the transaction shows, from their primary
    -- key, each asked about in the primary key of the tags of contacts
    atTaggedContact =
      exists . from $ \(x `InnerJoin` c) -> do
        on $
          c ^. DBContactTagNetwork ==. x ^. DBTxOtherAddressNetwork
            &&. c ^. DBContactTagAddress ==. x ^. DBTxOtherAddressAddress
        where_ $
          othersOfTx x
            &&. c ^. DBContactTagTag `in_` valList (tagText <$> kept.keptAddressTags)
    -- The addresses of others the transaction shows that are a contact
    -- with any tag. Here the tags of contacts are asked inside the
    -- question about each address, not joined to it as above. With no
    -- tag to look for, the database read a join the other way round:
    -- every tagged contact of the network, for each transaction. Asked
    -- this way, it has the one order to read them in.
    atContactWithTag :: SqlQuery ()
    atContactWithTag =
      from $ \x ->
        where_ $
          othersOfTx x
            &&. exists
              ( from $ \c ->
                  where_ $
                    c ^. DBContactTagNetwork ==. x ^. DBTxOtherAddressNetwork
                      &&. c ^. DBContactTagAddress ==. x ^. DBTxOtherAddressAddress
              )
    -- Whether a row of the addresses of others is of the transaction
    othersOfTx x =
      x ^. DBTxOtherAddressPortfolio ==. t ^. DBTxInfoPortfolio
        &&. x ^. DBTxOtherAddressNetwork ==. t ^. DBTxInfoNetwork
        &&. x ^. DBTxOtherAddressTxid ==. t ^. DBTxInfoTxid
    atTaggedAddress =
      exists . from $ \(x `InnerJoin` a) -> do
        on $
          a ^. DBAddressTagAccount ==. x ^. DBTxAddressAccount
            &&. a ^. DBAddressTagInternal ==. x ^. DBTxAddressInternal
            &&. a ^. DBAddressTagIndex ==. x ^. DBTxAddressIndex
        where_ $
          x ^. DBTxAddressPortfolio ==. t ^. DBTxInfoPortfolio
            &&. x ^. DBTxAddressNetwork ==. t ^. DBTxInfoNetwork
            &&. x ^. DBTxAddressTxid ==. t ^. DBTxInfoTxid
            &&. a ^. DBAddressTagTag `in_` valList (tagText <$> kept.keptAddressTags)

-- | Those of the given transactions of a pool that the filter keeps, by
-- the bytes of their ids. The unconfirmed transactions of a pool are
-- read whole and put in order outside the database, so they are asked
-- about here, all at once. There are few of them.
txsKeptAmong :: (MonadUnliftIO m) => Pool -> TxTagFilter -> [KeyBytes] -> DB m (Set KeyBytes)
txsKeptAmong pool kept txids = do
  found <- forM (chunksOf (fromIntegral inListBatch) txids) $ \some ->
    select . from $ \t -> do
      where_ $
        t ^. DBTxInfoPortfolio ==. val pool.poolPortfolio
          &&. t ^. DBTxInfoNetwork ==. val (poolNetworkName pool)
          &&. t ^. DBTxInfoTxid `in_` valList some
          &&. txKept kept t
      return (t ^. DBTxInfoTxid)
  return (Set.fromList [txid | Value txid <- concat found])

-- | The id of every transaction of a pool the filter keeps, a dead one
-- too. A statement and a journal narrowed down by tags read the whole
-- history of a pool at once, and ask here which of its transactions to
-- keep ("Haskoin.Wallet.TxSelection"). The condition is that of txs
-- ('txKept'), so they keep what txs lists with the same options.
--
-- The database walks the summaries of the pool by their primary key,
-- which holds the id, and reads no summary.
txidsKept :: (MonadUnliftIO m) => Pool -> TxTagFilter -> ExceptT String (DB m) [TxHash]
txidsKept pool kept = do
  found <- lift . select . from $ \t -> do
    where_ $
      t ^. DBTxInfoPortfolio ==. val pool.poolPortfolio
        &&. t ^. DBTxInfoNetwork ==. val (poolNetworkName pool)
        &&. txKept kept t
    return (t ^. DBTxInfoTxid)
  either throwError return (mapM (\(Value txid) -> bytesToHash txid) found)

-- | The ids of every transaction of a pool the filter keeps, when they
-- are at most the number given, or Nothing when they are more. The
-- accounts are those of the pool.
--
-- A listing asked for a rare tag would otherwise walk the whole history
-- of the pool to find its few transactions: a million rows read for
-- twenty kept. Here the tags give the transactions directly. Those that
-- have a tag come from the rows of their tags. Those at a tagged address
-- come from the tags of the addresses, then from the index of the
-- transactions of each address. Those at a tagged contact come the same
-- way, from the tags of the contacts. Each reads the matches and nothing
-- else.
--
-- Each of the reads stops one past the number given, so that a filter
-- that keeps half a million transactions costs no more here than one
-- that keeps two thousand. The listing then walks the history instead,
-- which is quick when most rows are kept.
--
-- A filter that keeps the transactions left to sort always walks the
-- history: no row of a tag names a transaction without one, so nothing
-- gives them directly.
fewKeptTxids :: (MonadUnliftIO m) => Natural -> Pool -> [DBAccountId] -> TxTagFilter -> DB m (Maybe (Set KeyBytes))
fewKeptTxids _ _ _ kept | kept.keptUntaggedTxs = return Nothing
fewKeptTxids atMost pool accIds kept = do
  tagged <-
    if null kept.keptTxTags
      then return []
      else select . from $ \g -> do
        where_ $
          g ^. DBTxTagPortfolio ==. val pool.poolPortfolio
            &&. g ^. DBTxTagNetwork ==. val (poolNetworkName pool)
            &&. g ^. DBTxTagTag `in_` valList (tagText <$> kept.keptTxTags)
        limit (rowLimit (atMost + 1))
        return (g ^. DBTxTagTxid)
  atAddresses <-
    if null kept.keptAddressTags
      then return []
      else select . from $ \(a `InnerJoin` x) -> do
        on $
          x ^. DBTxAddressAccount ==. a ^. DBAddressTagAccount
            &&. x ^. DBTxAddressInternal ==. a ^. DBAddressTagInternal
            &&. x ^. DBTxAddressIndex ==. a ^. DBAddressTagIndex
        where_ $
          a ^. DBAddressTagAccount `in_` valList accIds
            &&. a ^. DBAddressTagTag `in_` valList (tagText <$> kept.keptAddressTags)
        limit (rowLimit (atMost + 1))
        return (x ^. DBTxAddressTxid)
  -- Those at a tagged contact, in two reads. The tags of the contacts of
  -- the network give the addresses, which are few: an address book is
  -- small. The index of the transactions of the pool at an address of
  -- others then gives the transactions of those addresses.
  --
  -- One query that joins the two tables was turned down. SQLite chose to
  -- read every address of others of the pool and to ask the tags about
  -- each, which is the walk of the history this function is there to
  -- avoid. Two reads leave it no choice.
  contactAddresses <-
    if null kept.keptAddressTags
      then return []
      else select . distinct . from $ \c -> do
        where_ $
          c ^. DBContactTagNetwork ==. val (poolNetworkName pool)
            &&. c ^. DBContactTagTag `in_` valList (tagText <$> kept.keptAddressTags)
        return (c ^. DBContactTagAddress)
  atContacts <- fmap concat . forM (chunksOf (fromIntegral inListBatch) [address | Value address <- contactAddresses]) $ \some ->
    select . from $ \x -> do
      where_ $
        x ^. DBTxOtherAddressPortfolio ==. val pool.poolPortfolio
          &&. x ^. DBTxOtherAddressNetwork ==. val (poolNetworkName pool)
          &&. x ^. DBTxOtherAddressAddress `in_` valList some
      limit (rowLimit (atMost + 1))
      return (x ^. DBTxOtherAddressTxid)
  let tooMany found = fromIntegral (length found) > atMost
      txids = Set.fromList [txid | Value txid <- tagged <> atAddresses <> atContacts]
  return $
    if tooMany tagged || tooMany atAddresses || tooMany atContacts || fromIntegral (Set.size txids) > atMost
      then Nothing
      else Just txids
