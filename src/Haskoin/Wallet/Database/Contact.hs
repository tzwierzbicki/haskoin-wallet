{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The address book in the database: the contacts, the addresses of
-- others that the user keeps, each with its label and its tags.
-- "Haskoin.Wallet.Contacts" says what a contact is and why, and the
-- tables are described with the others ("Haskoin.Wallet.Database.Schema.V5").
--
-- = A contact is of the whole database
--
-- A contact is of no portfolio. Whichever portfolio pays the landlord,
-- the address is the landlord's. So the commands that manage the address
-- book work whichever portfolio is active, and deleting a portfolio or a
-- wallet leaves the address book as it is.
--
-- = A contact is on one network
--
-- A contact is kept by its network and by its address as hw writes it on
-- that network ('contactText'). The network is needed: an address reads
-- the same on every test network, and a legacy address reads on Bitcoin
-- and on Bitcoin Cash. The same address may be a contact on two
-- networks, each with its own label.
--
-- Bitcoin Cash writes one address in two forms, the legacy one and the
-- CashAddr one. Both are kept as the CashAddr one, so that they are one
-- contact, and so that a transaction finds the contact however the user
-- typed its address.
--
-- = Few rows, read whole
--
-- An address book is typed by hand, one contact at a time, so it holds
-- tens or hundreds of rows where a history holds many thousands. The
-- contacts of a network are therefore read whole and put in order here
-- ('networkContacts'), by a rule written once
-- ("Haskoin.Wallet.Contacts".'contactOrder'). An index on the label was
-- turned down: the order ignores capital letters, in every alphabet,
-- which SQLite cannot do by itself.
--
-- A transaction asks for the contacts among its addresses by their keys
-- ('contactsAmong'), so showing a history costs one read of the primary
-- key for each transaction, however large the address book.
--
-- = A contact the wallet holds
--
-- A wallet of the database may come to hold the address of a contact
-- ("Haskoin.Wallet.Contacts"). Which contacts it holds is asked of the
-- addresses each time ('contactHolders'), with one query for each
-- account of the network, and never kept in the address book: the
-- addresses of the wallet come and go without it.
module Haskoin.Wallet.Database.Contact
  ( -- * Contacts
    contactText,
    addContact,
    addContactDated,
    contactAt,
    contactsNamed,
    setContactLabel,
    deleteContact,

    -- * The tags of contacts
    tagContact,
    untagContact,

    -- * Reading the address book
    networkContacts,
    contactsAmong,

    -- * The contacts the wallet holds
    contactHolders,
    contactHolder,
    listedContacts,
    heldContacts,

    -- * For the shell
    contactsStarting,
    contactsLabelled,
  )
where

import Conduit (MonadUnliftIO)
import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto.Legacy (InnerJoin (..), Value (..), asc, from, in_, on, orderBy, select, val, valList, where_, (&&.), (==.), (^.))
import qualified Database.Persist as P
import Database.Persist.Sql (Single (..), rawSql)
import Haskoin
import Haskoin.Wallet.Contacts
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address (addrPoolPath, heldRowsOnNetwork)
import Haskoin.Wallet.Database.Portfolio (portfolioRef)
import Haskoin.Wallet.Database.Tag (TagFilter, ensureTag, keepsTagged, noTagFilter, storedTag)
import Haskoin.Wallet.Tags
import Haskoin.Wallet.Util (addrToTextE, rowLimit)
import Numeric.Natural (Natural)

{- Contacts -}

-- | The name of a network, as the rows of the address book keep it
networkKey :: Network -> Text
networkKey net = cs net.name

-- | The address of a contact as the address book keeps it: as hw writes
-- it on the network. Whatever form the user typed, the same address
-- gives the same text, which is the key of the contact.
contactText :: Network -> Address -> Either String Text
contactText = addrToTextE

-- | Add an address to the address book of a network, with its label,
-- which may be empty. An address that is a contact already is refused,
-- with the label it has and the way to change it: adding it again most
-- likely means the user forgot it was there, and replacing its label
-- unasked would lose the one they chose.
--
-- The caller checks that the address is of no wallet of the database
-- ("Haskoin.Wallet.Commands"). That check needs the accounts, which this
-- module does not read.
addContact :: (MonadUnliftIO m) => Network -> Address -> Text -> ExceptT String (DB m) ()
addContact net addr label = addContactDated net addr label =<< liftIO getCurrentTime

-- | Add an address to the address book as 'addContact' does, dated at
-- the time given, not at this one: importwallet gives a contact it
-- creates the date its file has for it, the day the user added the
-- contact on the computer that wrote the file.
addContactDated :: (MonadUnliftIO m) => Network -> Address -> Text -> UTCTime -> ExceptT String (DB m) ()
addContactDated net addr label added = do
  address <- liftEither (contactText net addr)
  heldM <- lift $ P.get (DBContactKey (networkKey net) address)
  forM_ heldM $ \held ->
    throwError $
      cs address
        <> " is in the address book already"
        <> (if Text.null (dBContactLabel held) then "" else ", as \"" <> cs (dBContactLabel held) <> "\"")
        <> ". To change its label, run: hw label "
        <> cs address
        <> " LABEL"
  lift $ P.insert_ (DBContact (networkKey net) address label added)

-- | The contact of a network at an address, by the text the address book
-- keeps it under ('contactText'), with its tags
contactAt :: (MonadUnliftIO m) => Network -> Text -> DB m (Maybe Contact)
contactAt net address = do
  rowM <- P.get (DBContactKey (networkKey net) address)
  forM rowM $ \row -> do
    tags <- Map.findWithDefault [] address <$> contactTagsAmong net [address]
    return (shownContact tags row)

-- | The contacts an address the user typed names, each with its
-- network: the address is read on every network, and looked up on each
-- where it reads. Most addresses read on one network. A legacy address
-- reads on Bitcoin and on Bitcoin Cash, and an address of a test network
-- on the others, so the address may be a contact on several. The caller
-- picks one as it picks an address of the wallet
-- ('Haskoin.Wallet.Database.Address.chooseOnNetwork').
contactsNamed :: (MonadUnliftIO m) => Text -> DB m [(Contact, Network)]
contactsNamed typed =
  fmap catMaybes . forM readings $ \(net, address) ->
    fmap (\contact -> (contact, net)) <$> contactAt net address
  where
    readings = [(net, address) | net <- allNets, Just addr <- [textToAddr net typed], Right address <- [contactText net addr]]

-- | Set the label of a contact, or remove it with the empty text. The
-- contact must be in the address book.
setContactLabel :: (MonadUnliftIO m) => Network -> Text -> Text -> DB m ()
setContactLabel net address label =
  P.update (DBContactKey (networkKey net) address) [DBContactLabel P.=. label]

-- | Delete a contact from the address book, with its tags. The tags
-- themselves stay, as they stay when an address loses them.
deleteContact :: (MonadUnliftIO m) => Network -> Text -> DB m ()
deleteContact net address = do
  P.deleteWhere [DBContactTagNetwork P.==. networkKey net, DBContactTagAddress P.==. address]
  P.delete (DBContactKey (networkKey net) address)

{- The tags of contacts -}

-- | Put tags on a contact, besides those it has. A tag it has already
-- changes nothing. The tags are those of the database, which addresses
-- and transactions have too: each gets its row first, as for them.
tagContact :: (MonadUnliftIO m) => Network -> Text -> [Tag] -> DB m ()
tagContact net address tags =
  forM_ tags $ \tag -> do
    ensureTag tag
    P.repsert
      (DBContactTagKey (networkKey net) address (tagText tag))
      (DBContactTag (networkKey net) address (tagText tag))

-- | Take tags off a contact. Answers those it did not have, which the
-- caller refuses, as for an address of the wallet.
untagContact :: (MonadUnliftIO m) => Network -> Text -> [Tag] -> DB m [Tag]
untagContact net address tags = do
  missing <- forM tags $ \tag -> do
    let key = DBContactTagKey (networkKey net) address (tagText tag)
    heldM <- P.get key
    case heldM of
      Just _ -> [] <$ P.delete key
      Nothing -> return [tag]
  return (concat missing)

-- | The tags of those of the contacts of a network that have some, by
-- the text of each address, with their colours, in the order of the
-- alphabet. A listing read whole asks 'inListBatch' addresses at a time.
contactTagsAmong :: (MonadUnliftIO m) => Network -> [Text] -> DB m (Map Text [ShownTag])
contactTagsAmong net addresses = do
  found <- forM (chunksOf (fromIntegral inListBatch) addresses) $ \some ->
    select . from $ \(t `InnerJoin` g) -> do
      on (g ^. DBTagName ==. t ^. DBContactTagTag)
      where_ $
        t ^. DBContactTagNetwork ==. val (networkKey net)
          &&. t ^. DBContactTagAddress `in_` valList some
      orderBy [asc (t ^. DBContactTagTag)]
      return (t ^. DBContactTagAddress, g)
  return $
    Map.fromListWith
      (flip (<>))
      [(address, [shown]) | (Value address, row) <- concat found, Right shown <- [storedTag (P.entityVal row)]]

{- Reading the address book -}

-- | A row of a contact as the answers show it, with the tags given
shownContact :: [ShownTag] -> DBContact -> Contact
shownContact tags row =
  Contact
    { contactAddress = dBContactAddress row,
      contactLabel = dBContactLabel row,
      contactTags = tags,
      contactCreated = dBContactCreated row
    }

-- | The contacts of a network, in the order contacts lists them
-- ('contactOrder'): by label, then by address. With a filter of tags,
-- those that have any one of the tags, and those without a tag when
-- asked for, as the listings of the wallet keep the rows of a tag. Each
-- contact comes with all of its tags, kept or not.
networkContacts :: (MonadUnliftIO m) => Network -> TagFilter -> DB m [Contact]
networkContacts net kept = do
  rows <- P.selectList [DBContactNetwork P.==. networkKey net] []
  tagsBy <- contactTagsAmong net (dBContactAddress . P.entityVal <$> rows)
  let contacts = [shownContact (Map.findWithDefault [] (dBContactAddress row) tagsBy) row | P.Entity _ row <- rows]
      keeps contact = keepsTagged kept (shownTag <$> contactTags contact)
  return (sortOn contactOrder (filter keeps contacts))

-- | The contacts among the addresses given, on a network, by address:
-- what a transaction shows of the addresses of others it spends from or
-- pays. An address that cannot be written on the network is no contact
-- of it.
contactsAmong :: (MonadUnliftIO m) => Network -> [Address] -> DB m (Map Address Contact)
contactsAmong net addrs
  | null texts = return Map.empty
  | otherwise = do
      rows <- forM (chunksOf (fromIntegral inListBatch) (Map.keys texts)) $ \some ->
        P.selectList [DBContactNetwork P.==. networkKey net, DBContactAddress P.<-. some] []
      let found = P.entityVal <$> concat rows
      -- Most transactions pay no contact: the tags are then not read
      tagsBy <- if null found then return Map.empty else contactTagsAmong net (dBContactAddress <$> found)
      return $
        Map.fromList
          [ (addr, shownContact (Map.findWithDefault [] (dBContactAddress row) tagsBy) row)
            | row <- found,
              Just addr <- [Map.lookup (dBContactAddress row) texts]
          ]
  where
    texts = Map.fromList [(address, addr) | addr <- addrs, Right address <- [contactText net addr]]

{- The contacts the wallet holds -}

-- | The address of the wallet that each of the contacts of a network is,
-- for those a wallet of the database holds on that network, by the
-- address of the contact as the address book keeps it. The same address
-- on another network is another chain, where the wallet may not hold it:
-- a contact of Bitcoin Cash at a legacy address the wallet has on Bitcoin
-- is still an address of somebody else.
contactHolders :: (MonadUnliftIO m) => Network -> [Contact] -> DB m (Map Text Holder)
contactHolders net contacts = do
  held <- heldRowsOnNetwork (const True) net (Map.keys kept)
  return $
    Map.fromList
      [ (address, Holder (portfolioRef portfolio) path)
        | (addr, (portfolio, row)) <- Map.toList held,
          Just address <- [Map.lookup addr kept],
          Just path <- [addrPoolPath net row]
      ]
  where
    kept = Map.fromList [(addr, contactAddress contact) | contact <- contacts, Just addr <- [textToAddr net (contactAddress contact)]]

-- | The address of the wallet that a contact of a network is, if a wallet
-- of the database holds it
contactHolder :: (MonadUnliftIO m) => Network -> Contact -> DB m (Maybe Holder)
contactHolder net contact = Map.lookup (contactAddress contact) <$> contactHolders net [contact]

-- | The contacts of a network as contacts lists them ('networkContacts'),
-- each with the address of the wallet it is, if a wallet holds it
listedContacts :: (MonadUnliftIO m) => Network -> TagFilter -> DB m [(Contact, Maybe Holder)]
listedContacts net kept = do
  contacts <- networkContacts net kept
  holders <- contactHolders net contacts
  return [(contact, Map.lookup (contactAddress contact) holders) | contact <- contacts]

-- | The contacts of a network that a wallet of the database holds, each
-- with the address of the wallet it is, in the order contacts lists them
heldContacts :: (MonadUnliftIO m) => Network -> DB m [(Contact, Holder)]
heldContacts net = do
  listed <- listedContacts net noTagFilter
  return [(contact, holder) | (contact, Just holder) <- listed]

{- For the shell -}

-- | The text of at most the given number of contacts of a network whose
-- address starts with the text, in the order of the addresses, as the
-- shell offers them. A Bitcoin Cash address is typed without its prefix,
-- so the text is matched after the colon of the stored address when it
-- has one, as for the addresses of the wallet
-- ('Haskoin.Wallet.Database.Address.addressesStarting'). The text is
-- compared as it is, without LIKE, which ignores the case of letters
-- while a base58 address does not.
contactsStarting :: (MonadUnliftIO m) => Network -> Text -> Natural -> DB m [Text]
contactsStarting net start atMost = do
  rows <-
    rawSql
      "select address from d_b_contact where network = ? and substr(address, instr(address, ':') + 1, ?) = ? order by address limit ?"
      [P.toPersistValue (networkKey net), P.toPersistValue (Text.length start), P.toPersistValue start, P.toPersistValue (rowLimit atMost :: Int)]
  return (unSingle <$> rows)

-- | The addresses of the contacts of a network whose label starts with
-- the text, capital and small letters alike, in the order contacts
-- lists them: what the shell completes a name to. The text is not
-- empty: every label starts with the empty text.
--
-- The labels are compared here, not in the database, which folds the
-- case of the letters a to z alone. The address book is small, so its
-- labels are all read.
contactsLabelled :: (MonadUnliftIO m) => Network -> Text -> DB m [Text]
contactsLabelled net start
  | Text.null start = return []
  | otherwise = do
      rows <- P.selectList [DBContactNetwork P.==. networkKey net, DBContactLabel P.!=. ""] []
      let named = [row | P.Entity _ row <- rows, Text.toCaseFold start `Text.isPrefixOf` Text.toCaseFold (dBContactLabel row)]
      return (contactAddress <$> sortOn contactOrder (shownContact [] <$> named))
