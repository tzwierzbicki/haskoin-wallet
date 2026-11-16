{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The address book in the database: the contacts, their labels and
-- their tags, what a transaction finds of them, and what the shell is
-- offered. The contacts of these tests are addresses of other wallets:
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU and
-- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ, the first two of TestUtils, and
-- the SegWit address bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4.
module Haskoin.Wallet.Database.ContactSpec (spec) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Haskoin
import Haskoin.Wallet.Contacts
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Contact
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Tags
import Haskoin.Wallet.TestUtils
import Test.Hspec

spec :: Spec
spec = do
  contactSpec
  networkSpec
  labelSpec
  tagSpec
  listingSpec
  transactionSpec
  shellSpec

-- | The address of the landlord, of Alice, and of Bob, as the address
-- book keeps them
landlord, alice, bob :: Text
landlord = "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
alice = "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"
bob = "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"

-- | The address of Bitcoin Cash that the specification of CashAddr
-- gives in both forms, in its legacy form, and in its CashAddr form
cashLegacy, cashAddr :: Text
cashLegacy = "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu"
cashAddr = "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"

-- | Add a contact on a network by its address as typed, with its label.
-- The address must read on the network.
added :: Network -> Text -> Text -> ExceptT String (DB IO) ()
added net typed label = case textToAddr net typed of
  Just addr -> addContact net addr label
  Nothing -> error ("The address " <> show typed <> " does not read")

-- | The label and the tags of a contact, or Nothing for none
labelAndTags :: Maybe Contact -> Maybe (Text, [Text])
labelAndTags = fmap (\contact -> (contactLabel contact, tagText . shownTag <$> contactTags contact))

-- | The addresses of contacts, in their order
addressesOf :: [Contact] -> [Text]
addressesOf = map contactAddress

contactSpec :: Spec
contactSpec =
  describe "A contact of the address book" $ do
    it "is found by its address once it is added, with its label" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", [])

    it "may have no label" $
      inDatabaseE $ do
        added btc landlord ""
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("", [])

    it "is not found before it is added" $
      inDatabaseE $
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Nothing

    it "is refused a second time, with the label it has and the way to change it" $
      inDatabase $ do
        refused <- runExceptT (added btc landlord "Landlord" >> added btc landlord "Somebody else")
        liftIO $ refused `shouldBe` Left "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is in the address book already, as \"Landlord\". To change its label, run: hw label 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU LABEL"

    it "keeps its label when it is refused a second time" $
      inDatabase $ do
        _ <- runExceptT (added btc landlord "Landlord")
        _ <- runExceptT (added btc landlord "Somebody else")
        (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", [])

    it "is gone once it is deleted" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ deleteContact btc landlord
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Nothing

networkSpec :: Spec
networkSpec =
  describe "The network of a contact" $ do
    -- The legacy form and the CashAddr form are one address of Bitcoin
    -- Cash
    it "keeps a Bitcoin Cash address in its CashAddr form, however it was typed" $
      inDatabaseE $ do
        added bch cashLegacy "Bob"
        lift $ (addressesOf <$> networkContacts bch []) `dbShouldBe` ["bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"]

    it "finds a Bitcoin Cash contact by either form of its address" $
      inDatabaseE $ do
        added bch cashAddr "Bob"
        lift $ (map (contactLabel . fst) <$> contactsNamed cashLegacy) `dbShouldBe` ["Bob"]
        lift $ (map (contactLabel . fst) <$> contactsNamed "qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a") `dbShouldBe` ["Bob"]

    -- A legacy address reads on Bitcoin and on Bitcoin Cash
    it "tells apart the same address on two networks, each with its label" $
      inDatabaseE $ do
        added btc cashLegacy "On Bitcoin"
        added bch cashLegacy "On Bitcoin Cash"
        lift $ (labelAndTags <$> contactAt btc cashLegacy) `dbShouldBe` Just ("On Bitcoin", [])
        lift $ (labelAndTags <$> contactAt bch cashAddr) `dbShouldBe` Just ("On Bitcoin Cash", [])

    it "names every network an address is a contact on" $
      inDatabaseE $ do
        added btc cashLegacy "On Bitcoin"
        added bch cashLegacy "On Bitcoin Cash"
        lift $ (map ((.name) . snd) <$> contactsNamed cashLegacy) `dbShouldBe` ["btc", "bch"]

labelSpec :: Spec
labelSpec =
  describe "The label of a contact" $ do
    it "is the one the user gave it last" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ setContactLabel btc landlord "Mr Smith"
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Mr Smith", [])

    it "is removed with an empty one, and the contact stays" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ setContactLabel btc landlord ""
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("", [])

tagSpec :: Spec
tagSpec =
  describe "The tags of a contact" $ do
    it "are shown in the order of the alphabet" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag, coldTag]
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", ["cold", "rent"])

    it "are added to those it has" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag]
        lift $ tagContact btc landlord [kycTag]
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", ["kyc", "rent"])

    it "come with the colour of each tag" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag]
        lift $ setTagColor rentTag (Just TagYellow)
        lift $ (fmap contactTags <$> contactAt btc landlord) `dbShouldBe` Just [ShownTag rentTag (Just TagYellow)]

    it "are taken off one by one, and the others stay" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag, kycTag]
        lift $ untagContact btc landlord [kycTag] `dbShouldBe` []
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", ["rent"])

    it "name the tags the contact did not have, when asked to take them off" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag]
        lift $ untagContact btc landlord [rentTag, kycTag] `dbShouldBe` [kycTag]

    it "are tags of the database, listed with those of addresses and transactions" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag]
        lift $ allTags `dbShouldBe` [ShownTag rentTag Nothing]

    -- rent is on two contacts, kyc on one
    it "are counted by tags, beside the addresses and the transactions of each tag" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        lift $ tagContact btc landlord [rentTag, kycTag]
        lift $ tagContact btc alice [rentTag]
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag kycTag Nothing) 0 0 1, TagUse (ShownTag rentTag Nothing) 0 0 2]

    it "go when their tag is deleted, which says how many contacts had it" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        lift $ tagContact btc landlord [rentTag, kycTag]
        lift $ tagContact btc alice [rentTag]
        lift $ deleteTag rentTag `dbShouldBe` TagDeleted 0 0 2
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", ["kyc"])
        lift $ (labelAndTags <$> contactAt btc alice) `dbShouldBe` Just ("Alice", [])

    -- rent becomes cold
    it "take the new name of a tag that is renamed" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        lift $ tagContact btc landlord [rentTag, kycTag]
        lift $ tagContact btc alice [rentTag]
        lift $ renameTag rentTag coldTag
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", ["cold", "kyc"])
        lift $ (labelAndTags <$> contactAt btc alice) `dbShouldBe` Just ("Alice", ["cold"])

    -- The landlord has both tags, and Alice has rent alone. rent is
    -- merged into kyc.
    it "hold a tag once when another tag they had too is merged into it" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        lift $ tagContact btc landlord [rentTag, kycTag]
        lift $ tagContact btc alice [rentTag]
        lift $ renameTag rentTag kycTag
        lift $ (labelAndTags <$> contactAt btc landlord) `dbShouldBe` Just ("Landlord", ["kyc"])
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag kycTag Nothing) 0 0 2]

    it "go with their contact, and the tag itself stays" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag]
        lift $ deleteContact btc landlord
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag rentTag Nothing) 0 0 0]

listingSpec :: Spec
listingSpec =
  describe "The contacts of a network" $ do
    it "are listed in the order of their labels, capital and small letters alike" $
      inDatabaseE $ do
        added btc landlord "landlord"
        added btc alice "Alice"
        added btc bob "Bob"
        lift $ (map contactLabel <$> networkContacts btc []) `dbShouldBe` ["Alice", "Bob", "landlord"]

    it "end with those that have no label" $
      inDatabaseE $ do
        added btc landlord ""
        added btc alice "Alice"
        lift $ (addressesOf <$> networkContacts btc []) `dbShouldBe` [alice, landlord]

    it "leave out the contacts of another network" $
      inDatabaseE $ do
        added btc alice "Alice"
        added bch cashAddr "Bob"
        lift $ (map contactLabel <$> networkContacts btc []) `dbShouldBe` ["Alice"]

    -- Alice is tagged kyc, Bob rent, and the landlord has no tag
    it "are those that have any one of the tags asked for" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        added btc bob "Bob"
        lift $ tagContact btc alice [kycTag]
        lift $ tagContact btc bob [rentTag]
        lift $ (map contactLabel <$> networkContacts btc [kycTag]) `dbShouldBe` ["Alice"]
        lift $ (map contactLabel <$> networkContacts btc [kycTag, rentTag]) `dbShouldBe` ["Alice", "Bob"]

    it "show all the tags of a contact kept for one of them" $
      inDatabaseE $ do
        added btc alice "Alice"
        lift $ tagContact btc alice [kycTag, coldTag]
        lift $ (map labelAndTags . map Just <$> networkContacts btc [kycTag]) `dbShouldBe` [Just ("Alice", ["cold", "kyc"])]

transactionSpec :: Spec
transactionSpec =
  describe "The contacts among the addresses of a transaction" $ do
    it "are found by address, each with its label and its tags" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ tagContact btc landlord [rentTag]
        found <- lift $ contactsAmong btc [otherAddr 0, otherAddr 1]
        liftIO $ Map.map (\contact -> (contactLabel contact, tagText . shownTag <$> contactTags contact)) found `shouldBe` Map.fromList [(otherAddr 0, ("Landlord", ["rent"]))]

    it "are none when the address book has none of the addresses" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ (Map.keys <$> contactsAmong btc [otherAddr 1, otherAddr 2]) `dbShouldBe` []

    -- The address is a contact on Bitcoin, and the transaction is on
    -- Bitcoin Cash
    it "are none on another network than that of the contact" $
      inDatabaseE $ do
        added btc cashLegacy "On Bitcoin"
        lift $ (Map.keys <$> contactsAmong bch [testAddress cashLegacy]) `dbShouldBe` []

shellSpec :: Spec
shellSpec =
  describe "The contacts the shell is offered" $ do
    it "are those whose address starts with the word typed" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        added btc bob "Bob"
        lift $ contactsStarting btc "1J" 21 `dbShouldBe` [landlord]
        lift $ contactsStarting btc "1" 21 `dbShouldBe` [landlord, alice]

    -- A base58 address tells capital letters from small ones
    it "are none for the start of an address typed in the wrong case" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        lift $ contactsStarting btc "1j" 21 `dbShouldBe` []

    it "are matched after the prefix of a Bitcoin Cash address" $
      inDatabaseE $ do
        added bch cashAddr "Bob"
        lift $ contactsStarting bch "qpm2" 21 `dbShouldBe` ["bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"]

    it "are no more than the number asked for" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        lift $ (length <$> contactsStarting btc "1" 1) `dbShouldBe` 1

    it "are those whose label starts with the word typed, capital and small letters alike" $
      inDatabaseE $ do
        added btc landlord "Landlord"
        added btc alice "Alice"
        lift $ contactsLabelled btc "ali" `dbShouldBe` [alice]
        lift $ contactsLabelled btc "LAND" `dbShouldBe` [landlord]

    it "are none by label for an empty word, which starts every label" $
      inDatabaseE $ do
        added btc alice "Alice"
        lift $ contactsLabelled btc "" `dbShouldBe` []

    it "are none by label for a word in the middle of a label" $
      inDatabaseE $ do
        added btc landlord "The landlord"
        lift $ contactsLabelled btc "land" `dbShouldBe` []
