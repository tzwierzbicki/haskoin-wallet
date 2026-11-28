{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The address book, as the commands keep it: newcontact, contacts and
-- deletecontact. A contact is an address of someone else that the user
-- keeps ("Haskoin.Wallet.Contacts"). The book is of the whole database, so
-- the three commands work whichever portfolio is active.
--
-- label, tag, untag and show take a contact by its address, where they
-- take an address of the wallet ('namedAddress'): the user need not
-- remember which of the two an address is. A transaction shows its
-- addresses that are contacts as such, with their labels
-- ('Haskoin.Wallet.Database.TxInfo.fillContacts').
module Haskoin.Wallet.Commands.Contact
  ( NamedAddress (..),
    namedAddress,
    notHeldOrKept,
    notHeldToShow,
    contactAnswer,
    cmdNewContact,
    readingNetworks,
    cmdDeleteContact,
    contactsListing,
  )
where

import Control.Monad (forM, forM_)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.Trans.Class (lift)
import Data.List (genericTake, intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Cli.Command
import Haskoin.Wallet.Cli.Listing
import Haskoin.Wallet.Cli.Response
import Haskoin.Wallet.Commands.Deletion (confirmDeletion, deleteContactNotice)
import Haskoin.Wallet.Commands.Run (runDBResponse)
import Haskoin.Wallet.Config
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Database.Contact
import Haskoin.Wallet.Database.Portfolio
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Keys

-- | What an address the user typed names: an address of a wallet of the
-- database, with its account, or a contact of the address book, with its
-- network
data NamedAddress
  = OwnAddress !DBAccountId !DBAddress
  | BookAddress !Network !Contact

-- | What an address the user typed names, for label, tag, untag and
-- show. The wallet is looked in first, as before the address book
-- existed. An address that no wallet of the database has is looked for
-- in the address book. So one command works on both kinds of address,
-- and the user need not say which kind an address is.
--
-- An address the wallet has is never a contact: newcontact refuses it.
-- Should the wallet come to hold a contact later, as after importwallet
-- adds the wallet that owns it, the address is the wallet's here too.
--
-- A contact is picked among the networks as an address of the wallet is
-- ('chooseOnNetwork'): the one on --network, or the only one, or else
-- the one on the default network. The first argument is the refusal of
-- an address that is in neither.
namedAddress :: String -> Config -> Maybe Network -> Text -> ExceptT String (DB IO) NamedAddress
namedAddress notFound cfg netM typed = do
  own <- lift $ findAddressByText typed
  if not (null own)
    then (\(_, accId, row) -> OwnAddress accId row) <$> addressByTextOr notFound cfg netM typed
    else uncurry (flip BookAddress) <$> contactNamed notFound cfg netM typed

-- | The one contact an address the user typed names, with its network,
-- or the refusal given when the address book has none with it
contactNamed :: String -> Config -> Maybe Network -> Text -> ExceptT String (DB IO) (Contact, Network)
contactNamed notFound cfg netM typed = do
  found <- lift $ contactsNamed typed
  case chooseOnNetwork cfg netM [(row, snd row) | row <- found] of
    Right row -> return row
    Left FoundNowhere -> throwError notFound
    Left (OnHiddenNetworks networks) -> throwError (onHiddenNetworks ("The contact " <> cs typed) networks)
    Left (NotOnGivenNetwork net networks) ->
      throwError $ "The contact " <> cs typed <> " is on " <> networksText networks <> ", not on the --network " <> net.name
    Left (OnSeveralNetworks networks) ->
      throwError $ "The contact " <> cs typed <> " is on " <> networksText (nub networks) <> ": give the --network"

-- | The refusal of label, tag and untag for an address that the wallet
-- has not generated on this computer and that is not in the address
-- book (@notGeneratedOrKept@).
notHeldOrKept :: Config -> Text -> String
notHeldOrKept cfg = notGeneratedOrKept cfg []

-- | The refusal of show for such an address. It also says how to check
-- an address that another computer of the wallet gave out: by its path,
-- which show derives from the key of the account, whether this computer
-- generated the address or not. The offline computer generates no
-- address, and checking one there is what a user pastes it for.
notHeldToShow :: Config -> Text -> String
notHeldToShow cfg =
  notGeneratedOrKept
    cfg
    ["To check an address that another computer of the wallet gave out, give its path, as receive printed it: hw show PATH"]

-- | The refusal of an address that no wallet of the database has
-- generated and that is not in the address book, with the advice of the
-- command, a sentence each, before the way to keep an address of
-- somebody else.
--
-- It says what this computer knows, and not that the address is none of
-- the wallet: it may well be one. A computer holds the addresses it
-- generated, when receive gave them out or a sync found them paid. The
-- same wallet on another computer, such as the offline one, has not
-- generated the addresses the first one gave out. "The wallet has no
-- address" was false there: a careful user took it for a sign that the
-- online computer was tampered with, and a careless one added an address
-- of the wallet to the address book.
--
-- hw does not look for the address among those the accounts would
-- derive next. Such a search has an end, a thousand addresses for
-- instance, and a chain of a computer that never synced starts at zero:
-- the search would find the address in a small wallet, and deny it again
-- in a large one, after the user learned to trust the answer. A path
-- names an address at any size, so the refusal of show sends there.
--
-- An address that only hidden networks read is refused as anything else
-- on hidden networks alone is ('onHiddenNetworks'), with the command
-- that shows one of them. Every advice here ends in a refusal for it:
-- newcontact keeps no contact on a hidden network, and a path needs the
-- --network of the address. Showing the network is the first step.
--
-- The word is an address: the commands refuse before one that reads as
-- an address on no network, as what it is (@namesNothing@), and show
-- takes such a word for a file.
notGeneratedOrKept :: Config -> [String] -> Text -> String
notGeneratedOrKept cfg advice typed
  | reading@(_ : _) <- fst <$> readingNetworks typed,
    null (onShownNetworks cfg id reading) =
      onHiddenNetworks ("The address " <> cs typed) reading
  | otherwise =
      intercalate ". " $
        ["The wallet has not generated the address " <> cs typed <> " on this computer, and it is not in the address book"]
          <> advice
          <> ["To add an address of somebody else to the address book, run: hw newcontact " <> cs typed <> " LABEL"]

-- | The answer showing a contact of a network as it is now, read again
-- by its address after a command that changed it
contactAnswer :: Network -> Text -> ExceptT String (DB IO) Response
contactAnswer net address = do
  contactM <- lift $ contactAt net address
  maybe (throwError ("The address book has no contact " <> cs address)) (return . ResponseContact net) contactM

-- | Add an address of somebody else to the address book, with its label,
-- if any, and its tags, and answer with the contact.
--
-- The network of the contact is the one --network names, where the
-- address must read. Without it, it is the only shown network the
-- address reads on, or else the default network, when the address reads
-- there. Most addresses read on one network. A legacy address reads on
-- Bitcoin and on Bitcoin Cash, and an address of a test network on the
-- others. hw does not guess past the default: the user is asked for
-- --network.
--
-- An address of a wallet of the database is refused, with where it is.
-- The address book is for the addresses of others. An address of the
-- wallet has a path, a label and tags of its own, and the views name it
-- by them. Two labels for one address would disagree sooner or later.
-- The check looks on the network of the contact alone: the same address
-- on another network is another chain, where the wallet may not hold it.
--
-- An address of the wallet that this computer has not generated yet is
-- taken: hw cannot know it (@notGeneratedOrKept@). Once the wallet holds
-- it, contacts, sync and the answer of the address say so
-- ("Haskoin.Wallet.Contacts").
cmdNewContact :: Config -> NewContact -> IO Response
cmdNewContact cfg opts =
  runDBResponse cfg $ do
    (net, addr) <- liftEither $ contactNetwork cfg opts.network opts.address
    address <- liftEither $ contactText net addr
    held <- lift $ heldRowsOnNetwork (const True) net [addr]
    forM_ (Map.elems held) $ \(portfolio, row) ->
      throwError $
        cs address
          <> " is an address of the wallet: "
          <> cs (maybe (pathText (addrPath row)) poolPathText (addrPoolPath net row))
          <> " of "
          <> cs (portfolioText portfolio)
          <> ". The address book is for the addresses of others. To label it, run: hw label "
          <> cs address
          <> " LABEL"
    addContact net addr (fromMaybe "" opts.label)
    lift $ tagContact net address opts.tags
    contactAnswer net address

-- | The network a new contact is on, and its address read there: see
-- 'cmdNewContact'. A word that reads as an address on no network is
-- refused as no address. One that reads on hidden networks alone is
-- refused with the way to show one.
contactNetwork :: Config -> Maybe Network -> Text -> Either String (Network, Address)
contactNetwork cfg netM typed =
  case (netM, shown) of
    (Just net, _) -> maybe (Left (cs typed <> " is not an address of " <> net.name)) (Right . (net,)) (textToAddr net typed)
    (Nothing, [only]) -> Right only
    (Nothing, [])
      | null everywhere -> Left (cs typed <> " is not an address")
      | otherwise -> Left (onHiddenNetworks ("The address " <> cs typed) (fst <$> everywhere))
    (Nothing, several) ->
      maybe
        (Left ("The address " <> cs typed <> " reads on " <> networksText (fst <$> several) <> ": give the --network"))
        Right
        (listToMaybe [reading | reading <- several, fst reading == configDefaultNetwork cfg])
  where
    everywhere = readingNetworks typed
    shown = onShownNetworks cfg fst everywhere

-- | The networks of a portfolio that read a text as an address, each with
-- the address it reads. The text of an address does not always name one
-- network: one that starts with 1 reads on btc and on bch, and one that
-- starts with m on every test network.
readingNetworks :: Text -> [(Network, Address)]
readingNetworks typed = [(net, addr) | net <- portfolioNetworks, Just addr <- [textToAddr net typed]]

-- | Delete a contact from the address book, by its address, and answer
-- with the contact as it was. The contact is found as label finds it:
-- on --network, or the only one, or else on the default network.
--
-- The user is asked first, unless --yes, and shown the contact with its
-- label ('confirmDeletion', 'deleteContactNotice'). The command deletes
-- no funds and no key, and the copy of the labels hw keeps after each
-- change holds the address book as it was before (@backUpLabels@).
--
-- A contact whose address a wallet of the database holds is found here
-- too, and nowhere else: label, tag, untag and show take its address for
-- that of the wallet ('namedAddress'). The question then says where the
-- wallet holds the address.
cmdDeleteContact :: Config -> Text -> Maybe Network -> Bool -> IO Response
cmdDeleteContact cfg typed netM yes =
  runDBResponse cfg $ do
    (contact, net) <- contactNamed (notInBook typed) cfg netM typed
    holderM <- lift $ contactHolder net contact
    confirmDeletion yes (deleteContactNotice contact holderM) "Delete the contact?"
    lift $ deleteContact net (contactAddress contact)
    return (ResponseContactDeleted net contact)

-- | The refusal of an address the address book does not have
notInBook :: Text -> String
notInBook typed = "The address book has no contact " <> cs typed <> ". hw contacts lists them."

-- | The address book, a section for each network hw shows that has
-- contacts, or for the one --network keeps, in the order of the labels
-- ('Haskoin.Wallet.Contacts.contactOrder'), or in its reverse with
-- --reverse. A contact names the one to go on after, by its address: the
-- listing starts in the section of its network.
--
-- With a filter of tags, the listing keeps the contacts that have any
-- one of the tags, and those without a tag when asked for. Each contact
-- comes with all of its tags, kept or not. A word that is no tag is
-- refused, as in the other listings.
--
-- A contact whose address a wallet of the database holds comes with that
-- address of the wallet ('listedContacts'), so that the listing marks it:
-- the contact is still there, and nothing else shows it any more.
--
-- An address book is typed by hand and holds few rows, so each section
-- is read whole, and its table sizes its columns to all of its contacts
-- ("Haskoin.Wallet.Database.Contact").
--
-- The listing is the same whichever portfolio is active, and works in a
-- database without any: the address book is of the whole database.
contactsListing :: Config -> Maybe Network -> TagFilter -> Paging Text -> ExceptT String (DB IO) Sections
contactsListing cfg netM kept paging = do
  _ <- requireTags kept.keptTags
  let listed = [net | net <- portfolioNetworks, isShown cfg net, maybe True (== net) netM]
      rowsOf net = do
        contacts <- lift $ listedContacts net kept
        return $ case listedOrder paging of
          UsualOrder -> contacts
          ReverseOrder -> reverse contacts
      section net startM =
        Listing
          { listingStart = startM,
            listingRows = \afterM countM -> do
              contacts <- rowsOf net
              let following = case afterM of
                    Nothing -> contacts
                    Just cursor -> drop 1 $ dropWhile ((/= cursor) . contactAddress . fst) contacts
              return $ maybe id genericTake countM following,
            listingCursor = Right . contactAddress . fst,
            listingAnswer = \contacts -> ResponseContacts [(net, contacts) | not (null contacts)],
            listingWhole = True,
            listingShownEmpty = False
          }
  (from, startM) <- case paging.pagingAfter of
    Nothing -> return (listed, Nothing)
    Just typed -> do
      held <- forM listed $ \net -> do
        contacts <- rowsOf net
        let named = [address | Just addr <- [textToAddr net typed], Right address <- [contactText net addr]]
        return (net, listToMaybe [contactAddress contact | (contact, _) <- contacts, contactAddress contact `elem` named])
      found <- liftEither $ sectionOfAddress typed held
      case found of
        Just (i, address) -> return (drop i listed, Just address)
        Nothing -> throwError ("The contact " <> cs typed <> " is not among the contacts listed")
  return $ Sections (length listed > 1) paging.pagingMax (zipWith section from (startM : repeat Nothing))
