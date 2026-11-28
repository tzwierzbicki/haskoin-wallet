{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The address book, through the commands of hw: newcontact, contacts
-- and deletecontact, the label, the tags and the view of a contact, the
-- contacts in the views of a transaction, the Tab key, and the address
-- book in the file of a wallet and in a spreadsheet.
--
-- The contacts are addresses of other wallets. The landlord is at
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU: the address the payment of the
-- shared story was paid from, and the one the tests pay 20000 to. Alice
-- is at bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4. The home holds
-- portfolio0 of the test wallet, whose first receiving address is
-- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj.
module Haskoin.Wallet.Workflow.ContactSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import Haskoin
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Portfolio (Place (..))
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.WalletFile (ImportReport (..))
import Haskoin.Wallet.Tags (ShownTag (..), colorName, tagText)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Util (addrToString)
import Haskoin.Wallet.Workflow.TestUtils
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  newContactSpec
  changeSpec
  deleteSpec
  listingSpec
  transactionSpec
  completionSpec
  walletFileSpec
  sheetSpec

{- The contacts of these tests -}

-- | The address of the landlord, of Alice, and the address of Bitcoin
-- Cash that the specification of CashAddr gives in both forms
landlord, alice, cashLegacy, cashAddr :: String
landlord = "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
alice = "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"
cashLegacy = "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu"
cashAddr = "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"

{- What an answer shows of the address book -}

-- | The contact of an answer about one: its network, its address, its
-- label and its tags
contactOf :: Response -> Either String (String, Text, Text, [Text])
contactOf = \case
  ResponseContact net contact -> Right (shown net contact)
  ResponseContactDeleted net contact -> Right (shown net contact)
  other -> Left (show other)
  where
    shown :: Network -> Contact -> (String, Text, Text, [Text])
    shown net contact = (net.name, contactAddress contact, contactLabel contact, namesOf (contactTags contact))

-- | The contacts contacts lists: the network of each group, with the
-- label of each of its contacts, in their order
listedOf :: Response -> Either String [(String, [Text])]
listedOf (ResponseContacts groups) = Right [(net.name, contactLabel <$> contacts) | (net, contacts) <- groups]
listedOf other = Left (show other)

-- | When each contact contacts lists was added, by its label
addedOf :: Response -> Either String [(Text, UTCTime)]
addedOf (ResponseContacts groups) = Right [(contactLabel contact, contactCreated contact) | (_, contacts) <- groups, contact <- contacts]
addedOf other = Left (show other)

-- | The contacts of the transactions of an answer: the label of each
-- contact, by its address, for each transaction
txContactsOf :: Response -> Either String [[(String, Text)]]
txContactsOf = \case
  ResponseTxs groups -> Right [contactsIn (placeNetwork (groupPlace g)) (listedTxInfo listed) | g <- groups, listed <- groupRows g]
  ResponsePendingTxs _ held -> Right [contactsIn (placeNetwork place) info | (place, info) <- held]
  ResponsePrepared place info _ _ -> Right [contactsIn (placeNetwork place) info]
  ResponseTx place info _ -> Right [contactsIn (placeNetwork place) info]
  other -> Left (show other)
  where
    contactsIn :: Network -> TxInfo -> [(String, Text)]
    contactsIn net info = [(addrToString net addr, contactLabel contact) | (addr, contact) <- Map.toList (txInfoContacts info)]

-- | Three contacts of btc in a home: the landlord, Alice, tagged
-- friends, and Zoe at the second address of other wallets of TestUtils,
-- tagged friends too
threeContacts :: FilePath -> IO ()
threeContacts home = do
  _ <- answer home ["newcontact", landlord, "landlord"] []
  _ <- answer home ["newcontact", alice, "Alice", "--tag", "friends"] []
  _ <- answer home ["newcontact", "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ", "Zoe", "--tag", "friends"] []
  return ()

newContactSpec :: Spec
newContactSpec =
  describe "newcontact" $ do
    it "adds an address of somebody else to the address book, with its label, which the answer shows" $
      withHome $ \home ->
        (contactOf <$> hwJson home ["newcontact", landlord, "Landlord"] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", [])

    it "adds an address without a label" $
      withHome $ \home ->
        (contactOf <$> hwJson home ["newcontact", landlord] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "", [])

    it "gives the contact the tags of --tag" $
      withHome $ \home ->
        (contactOf <$> hwJson home ["newcontact", alice, "Alice", "--tag", "friends", "--tag", "kyc"] [])
          `shouldReturn` Right ("btc", "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4", "Alice", ["friends", "kyc"])

    -- The home has no portfolio: the address book is of the database
    it "works in a database without any portfolio" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right [("btc", ["Landlord"])]

    -- A CashAddr address reads on Bitcoin Cash alone, among btc and bch
    it "takes the only shown network the address reads on, whatever the default network" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        (contactOf <$> hwJson home ["newcontact", cashAddr, "Bob"] [])
          `shouldReturn` Right ("bch", "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a", "Bob", [])

    -- A legacy address reads on btc and on bch, and btc is the default
    it "takes the default network for an address that reads on several" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        (contactOf <$> hwJson home ["newcontact", cashLegacy, "Bob"] [])
          `shouldReturn` Right ("btc", "1BpEi6DfDAUFd7GtittLSdBeYJvcoaVggu", "Bob", [])

    it "takes the network of --network, and keeps a Bitcoin Cash address in its CashAddr form" $
      withHome $ \home -> do
        setShownNetworks home ["btc", "bch"]
        (contactOf <$> hwJson home ["newcontact", cashLegacy, "Bob", "--network", "bch"] [])
          `shouldReturn` Right ("bch", "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a", "Bob", [])

    -- A SegWit address of Bitcoin has no form on Bitcoin Cash
    it "refuses an address that does not read on the network of --network" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newcontact", alice, "Alice", "--network", "bch"] [])
          `shouldReturn` Right "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4 is not an address of bch"

    it "refuses a word that is no address" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["newcontact", "landlord"] [])
          `shouldReturn` Right "landlord is not an address"

    it "refuses an address that is in the address book already, with the label it has" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        (errorOf <$> hwJson home ["newcontact", landlord, "Somebody else"] [])
          `shouldReturn` Right "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is in the address book already, as \"Landlord\". To change its label, run: hw label 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU LABEL"

    -- receive gives out the first receiving address of the portfolio
    it "refuses an address of the wallet, with where it is" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["receive"] []
        (errorOf <$> hwJson home ["newcontact", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Me"] [])
          `shouldReturn` Right "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is an address of the wallet: bip44/0/0 of portfolio0 of wallet0. The address book is for the addresses of others. To label it, run: hw label 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj LABEL"

    it "shows the address in one piece, then in fours, with its network" $
      withHome $ \home -> do
        shown <- Text.lines . runOut <$> hw home ["newcontact", alice, "Alice", "--tag", "friends"] []
        take 3 shown
          `shouldBe` [ "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  \"Alice\"  [friends]",
                       "  In fours: bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4",
                       "  Network : btc"
                     ]

changeSpec :: Spec
changeSpec =
  describe "A contact of the address book" $ do
    it "takes the label label gives it, by its address" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        (contactOf <$> hwJson home ["label", landlord, "Mr Smith"] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Mr Smith", [])

    it "loses its label with an empty one" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        (contactOf <$> hwJson home ["label", landlord, ""] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "", [])

    it "takes the tags tag puts on it, besides those it has" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        (contactOf <$> hwJson home ["tag", landlord, "kyc"] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["kyc", "rent"])

    it "loses the tags untag takes off it" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent", "--tag", "kyc"] []
        (contactOf <$> hwJson home ["untag", landlord, "kyc"] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["rent"])

    it "is refused the untag of a tag it does not have" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        (errorOf <$> hwJson home ["untag", landlord, "kyc"] [])
          `shouldReturn` Right "The contact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU does not have the tag kyc"

    it "is shown by show, by its address" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        (contactOf <$> hwJson home ["show", landlord] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["rent"])

    -- The contact is on bch: its address is found without its prefix,
    -- and in its legacy form
    it "is found by either form of a Bitcoin Cash address" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", cashAddr, "Bob"] []
        (contactOf <$> hwJson home ["show", "qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"] [])
          `shouldReturn` Right ("bch", "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a", "Bob", [])
        (contactOf <$> hwJson home ["label", cashLegacy, "Robert", "--network", "bch"] [])
          `shouldReturn` Right ("bch", "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a", "Robert", [])

    it "is counted by tags, among what has each tag" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "rent"] []
        listed <- hwJson home ["tags"] []
        case listed of
          ResponseTags [use] -> (tagText (shownTag use.tagUseTag), use.tagUseAddresses, use.tagUseTxs, use.tagUseContacts) `shouldBe` ("rent", 0, 0, 2)
          other -> expectationFailure (show other)

    it "takes the new name of a tag that renametag renames" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["renametag", "rent", "housing"] []
        (contactOf <$> hwJson home ["show", landlord] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["housing"])

    it "loses a tag that deletetag deletes, which says how many contacts had it" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "rent"] []
        deleted <- hwJson home ["deletetag", "rent", "--yes"] []
        case deleted of
          ResponseTagDeleted tag had -> (tagText tag, had) `shouldBe` ("rent", TagDeleted 0 0 2)
          other -> expectationFailure (show other)
        (contactOf <$> hwJson home ["show", landlord] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", [])

    -- The address is neither of the wallet nor of the address book
    it "must be in the address book to be labelled, and the refusal says how to add it" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["label", landlord, "Landlord"] [])
          `shouldReturn` Right "The wallet has not generated the address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU on this computer, and it is not in the address book. To add an address of somebody else to the address book, run: hw newcontact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU LABEL"

    it "must be in the address book to be tagged, and the refusal says how to add it" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["tag", landlord, "rent"] [])
          `shouldReturn` Right "The wallet has not generated the address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU on this computer, and it is not in the address book. To add an address of somebody else to the address book, run: hw newcontact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU LABEL"

    it "must be in the address book for untag to take a tag off it, and the refusal says how to add it" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["untag", landlord, "rent"] [])
          `shouldReturn` Right "The wallet has not generated the address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU on this computer, and it is not in the address book. To add an address of somebody else to the address book, run: hw newcontact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU LABEL"

deleteSpec :: Spec
deleteSpec =
  describe "deletecontact" $ do
    it "deletes a contact, and answers with the contact as it was" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        (contactOf <$> hwJson home ["deletecontact", landlord, "--yes"] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["rent"])
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right []

    it "leaves the other contacts as they are" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["newcontact", alice, "Alice"] []
        _ <- answer home ["deletecontact", landlord, "--yes"] []
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right [("btc", ["Alice"])]

    it "keeps the tags the contact had as tags of the database" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["deletecontact", landlord, "--yes"] []
        listed <- hwJson home ["tags"] []
        case listed of
          ResponseTags [use] -> (tagText (shownTag use.tagUseTag), colorName <$> shownTagColor use.tagUseTag, use.tagUseContacts) `shouldBe` ("rent", Nothing, 0)
          other -> expectationFailure (show other)

    it "refuses an address the address book does not have" $
      withHome $ \home ->
        (errorOf <$> hwJson home ["deletecontact", landlord] [])
          `shouldReturn` Right "The address book has no contact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU. hw contacts lists them."

    it "shows the contact it deleted" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        shown <- Text.lines . runOut <$> hw home ["deletecontact", landlord, "--yes"] []
        shown
          `shouldBe` [ "Deleted from the address book",
                       "  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\""
                     ]

    it "asks first, with the contact and its label, and what becomes of the transactions that paid it" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        run <- hw home ["deletecontact", landlord] ["y"]
        Text.lines (runErr run)
          `shouldBe` [ "The contact 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" will be deleted from",
                       "the address book.",
                       "The transactions that paid the address stay in the history, and show it as the",
                       "address of a stranger.",
                       "Delete the contact? [y/N] "
                     ]

    -- The tag rent stays in the database, for what else has it
    it "asks with a word that the tags stay, when the contact has tags" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        run <- hw home ["deletecontact", landlord] ["n"]
        take 1 (drop 2 (Text.lines (runErr run))) `shouldBe` ["Its tags come off it, and the tags themselves stay."]

    it "deletes the contact once the user answers yes" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        run <- hw home ["deletecontact", landlord] ["y"]
        runExit run `shouldBe` ExitSuccess
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right []

    it "keeps the contact when the user answers no, and hw fails" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        run <- hw home ["deletecontact", landlord] ["n"]
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "Nothing was deleted"
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right [("btc", ["Landlord"])]

    it "asks nothing with --yes" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        run <- hw home ["deletecontact", landlord, "--yes"] []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

listingSpec :: Spec
listingSpec =
  describe "contacts" $ do
    it "lists the contacts in the order of their labels, capital and small letters alike" $
      withHome $ \home -> do
        threeContacts home
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right [("btc", ["Alice", "landlord", "Zoe"])]

    it "lists them in the reverse order with --reverse" $
      withHome $ \home -> do
        threeContacts home
        (listedOf <$> hwJson home ["contacts", "--reverse"] []) `shouldReturn` Right [("btc", ["Zoe", "landlord", "Alice"])]

    it "puts a contact without a label last" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", landlord] []
        _ <- answer home ["newcontact", alice, "Alice"] []
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right [("btc", ["Alice", ""])]

    it "lists the contacts of each network in a group of its own, btc first" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", cashAddr, "Bob"] []
        _ <- answer home ["newcontact", alice, "Alice"] []
        (listedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right [("btc", ["Alice"]), ("bch", ["Bob"])]

    it "keeps the contacts of one network with --network" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", cashAddr, "Bob"] []
        _ <- answer home ["newcontact", alice, "Alice"] []
        (listedOf <$> hwJson home ["contacts", "--network", "bch"] []) `shouldReturn` Right [("bch", ["Bob"])]

    it "keeps the contacts that have the tag of --tag" $
      withHome $ \home -> do
        threeContacts home
        (listedOf <$> hwJson home ["contacts", "--tag", "friends"] []) `shouldReturn` Right [("btc", ["Alice", "Zoe"])]

    it "refuses a word that is no tag" $
      withHome $ \home -> do
        threeContacts home
        (errorOf <$> hwJson home ["contacts", "--tag", "nope"] [])
          `shouldReturn` Right "There is no tag nope. hw tags lists the tags."

    it "goes on after the contact --after names, for as many rows as --max says" $
      withHome $ \home -> do
        threeContacts home
        (listedOf <$> hwJson home ["contacts", "--after", alice, "--max", "1"] []) `shouldReturn` Right [("btc", ["landlord"])]

    it "refuses to go on after an address that is no contact" $
      withHome $ \home -> do
        threeContacts home
        (errorOf <$> hwJson home ["contacts", "--after", "191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q"] [])
          `shouldReturn` Right "The contact 191J7K3FaXXyM7C9ceSMRsJNF6aWCvvf1Q is not among the contacts listed"

    -- The home shows every network, so the section says which one it is
    it "shows each contact in two lines on the screen, under the name of its network" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "friends"] []
        shown <- Text.lines . runOut <$> hw home ["contacts"] []
        shown
          `shouldBe` [ "btc",
                       "",
                       "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  \"Alice\"  [friends]",
                       "  bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4"
                     ]

    it "names no network on the screen when hw shows one network only" $
      withHome $ \home -> do
        setShownNetworks home ["btc"]
        _ <- answer home ["newcontact", alice, "Alice"] []
        shown <- Text.lines . runOut <$> hw home ["contacts"] []
        take 1 shown `shouldBe` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4  \"Alice\""]

    it "starts with a line that names the tags it keeps" $
      withHome $ \home -> do
        threeContacts home
        shown <- Text.lines . runOut <$> hw home ["contacts", "--tag", "friends"] []
        take 2 shown `shouldBe` ["Contacts tagged friends", ""]

    it "lists the contacts that have no tag, under a line that says so, with --untagged" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "friends"] []
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        shown <- Text.lines . runOut <$> hw home ["contacts", "--network", "btc", "--untagged"] []
        take 3 shown
          `shouldBe` [ "Contacts without a tag",
                       "",
                       "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\""
                     ]

    it "says how to add a contact when the address book is empty" $
      withHome $ \home -> do
        shown <- Text.lines . runOut <$> hw home ["contacts"] []
        shown
          `shouldBe` [ "There are no contacts to display",
                       "To add an address of somebody else to the address book, run: hw newcontact ADDRESS LABEL"
                     ]

transactionSpec :: Spec
transactionSpec =
  describe "A transaction at an address of the address book" $ do
    -- The payment of the paid chain was paid from the address of the
    -- landlord
    it "names the contact a payment received was paid from, in the answer of txs" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        (txContactsOf <$> hwJson home ["txs"] [])
          `shouldReturn` Right [[("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord")]]

    it "names no contact for the address of a stranger" $
      withChain paidChain $ \home _ -> do
        synced home
        (txContactsOf <$> hwJson home ["txs"] []) `shouldReturn` Right [[]]

    it "names the contact as it is now: one added after the sync, or deleted since" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["label", landlord, "Mr Smith"] []
        (txContactsOf <$> hwJson home ["txs"] [])
          `shouldReturn` Right [[("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Mr Smith")]]
        _ <- answer home ["deletecontact", landlord, "--yes"] []
        (txContactsOf <$> hwJson home ["txs"] []) `shouldReturn` Right [[]]

    -- The payment prepared pays 20000 to the address of the landlord
    it "names the contact a payment pays in the answer of preparetx, before it is signed" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        (txContactsOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] [])
          `shouldReturn` Right [[("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord")]]

    it "follows the address a pending payment pays with the label of its contact, in pendingtxs" $
      withChain paidChain $ \home _ -> do
        _ <- paymentSigned home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        shown <- Text.lines . runOut <$> hw home ["pendingtxs"] []
        drop 1 (take 2 shown) `shouldBe` ["  portfolio0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\""]

    -- The payment of the paid chain was paid from the address of the
    -- landlord, at the first receiving address of the wallet. The
    -- listing has no option of tags: the contact is named all the same,
    -- without its tag.
    it "names the contact a payment received was paid from on the screen of txs, on a line of its own" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        listed <- Text.lines . runOut <$> hw home ["txs", "--network", "btc"] []
        dropWhile (not . Text.isPrefixOf "  ") listed
          `shouldBe` [ "  balance 0.00050000 btc, at bip44/0/0",
                       "  from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\""
                     ]

    -- The payment pays 10000 to the landlord and 20000 to a stranger, at
    -- 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "names the contact a pending payment pays before a stranger that it pays more, in pendingtxs" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0001", fixedTo (otherAddr 1) "0.0002", "--fee-rate", "10"] []
        shown <- Text.lines . runOut <$> hw home ["pendingtxs"] []
        drop 1 (take 2 shown) `shouldBe` ["  portfolio0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" and 1 more"]

    -- The payment pays 20000 to Alice, 10000 to the landlord, and 5000
    -- to a stranger, at 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ. The count is
    -- of the stranger alone, after the last contact.
    it "names each contact a pending payment pays, the one it pays the most first, then counts the addresses it does not name, in pendingtxs" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["newcontact", alice, "Alice"] []
        _ <- answer home ["preparetx", alice <> ":0.0002", fixedTo (otherAddr 0) "0.0001", fixedTo (otherAddr 1) "0.00005", "--fee-rate", "10"] []
        shown <- Text.lines . runOut <$> hw home ["pendingtxs"] []
        drop 1 (take 3 shown)
          `shouldBe` [ "  portfolio0 btc, to bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4 \"Alice\"",
                       "  to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" and 1 more"
                     ]

    it "puts the label of the contact below its address in the verbose view of txs" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        shown <- Text.lines . runOut <$> hw home ["txs", "--network", "btc", "--verbose"] []
        take 2 (dropWhile (not . Text.isPrefixOf "  From") shown)
          `shouldBe` [ "  From     : 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU",
                       "             \"Landlord\"  [rent]"
                     ]

    -- The payment of the paid chain was paid from the address of the
    -- landlord, who has the tag rent. Alice has the tag friends, and
    -- paid nothing.
    it "is kept by txs --address-tag when its contact has the tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "friends"] []
        (txContactsOf <$> hwJson home ["txs", "--address-tag", "rent"] [])
          `shouldReturn` Right [[("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord")]]
        (txContactsOf <$> hwJson home ["txs", "--address-tag", "friends"] []) `shouldReturn` Right []

    -- The contact is added and tagged after the sync that brought the
    -- payment
    it "is kept by txs --address-tag for a contact tagged after the transaction was synced" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["tag", landlord, "rent"] []
        (fmap length . txContactsOf <$> hwJson home ["txs", "--address-tag", "rent"] []) `shouldReturn` Right 1
        _ <- answer home ["untag", landlord, "rent"] []
        (fmap length . txContactsOf <$> hwJson home ["txs", "--address-tag", "rent"] []) `shouldReturn` Right 0

    -- The pending payment pays 20000 to the address of the landlord
    it "is kept by pendingtxs --address-tag when the contact it pays has the tag" $
      withChain paidChain $ \home _ -> do
        _ <- paymentSigned home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "friends"] []
        (txContactsOf <$> hwJson home ["pendingtxs", "--address-tag", "rent"] [])
          `shouldReturn` Right [[("1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord")]]
        (txContactsOf <$> hwJson home ["pendingtxs", "--address-tag", "friends"] []) `shouldReturn` Right []

    -- The payment received names the address it was paid at. The contact
    -- that paid it follows on a line of its own, with the tag that put
    -- the payment there.
    it "names the contact and the tag asked for on the screen of txs --address-tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent", "--tag", "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["txs", "--network", "btc", "--address-tag", "rent"] []
        (take 2 listed, drop 3 listed)
          `shouldBe` ( ["Transactions at addresses tagged rent", ""],
                       [ "  balance 0.00050000 btc, at bip44/0/0",
                         "  from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"Landlord\" [rent]"
                       ]
                     )

    -- The payment and the contact it pays both have the tag rent. The
    -- first line is about the payment, so the tag there is its own. The
    -- tag beside the address is that of the contact. The payment takes
    -- 20000 and a fee of 2260 from the portfolio: 22260. Its time is in
    -- the zone of the computer, so the test reads what follows it.
    it "shows the tag of a payment on its first line and the same tag of the contact it pays beside the contact, in pendingtxs --address-tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--label", "rent of November", "--tag", "rent"] []
        listed <- Text.lines . runOut <$> hw home ["pendingtxs", "--address-tag", "rent"] []
        (Text.takeEnd 51 <$> take 1 (drop 2 listed), drop 3 listed)
          `shouldBe` ( ["     -0.00022260 btc  not signed, 10 sat/vB  [rent]"],
                       ["  portfolio0 btc, to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU \"rent of November\" [rent]"]
                     )

    -- ESC[4m underlines, and ESC[0m ends it
    it "underlines the address of a contact on a terminal" $
      withChain paidChain $ \home _ -> do
        _ <- paymentSigned home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        run <- hwOnScreen [] home ["pendingtxs", "--no-pager"]
        runOut run `shouldSatisfy` Text.isInfixOf "to \ESC[0;4m1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\ESC[0m"

    it "does not underline the address of a stranger" $
      withChain paidChain $ \home _ -> do
        _ <- paymentSigned home
        run <- hwOnScreen [] home ["pendingtxs", "--no-pager"]
        runOut run `shouldSatisfy` (not . Text.isInfixOf "\ESC[0;4m")

completionSpec :: Spec
completionSpec =
  describe "The Tab key" $ do
    -- The colon is escaped, as bash takes every value hw offers
    it "completes the address of a contact for a recipient of send, up to the colon before its amount" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["send", "1J"] `shouldReturn` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\\:"]

    it "writes the address of the one contact whose label starts with the word typed" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["send", "ali"] `shouldReturn` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4\\:"]

    it "does the same for a recipient of preparetx" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["preparetx", "LAND"] `shouldReturn` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\\:"]

    -- A recipient of sendall takes what remains, without an amount
    it "completes the address alone for a recipient of sendall and of preparesendall" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["sendall", "zo"] `shouldReturn` ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]
        bashCompletion home ["preparesendall", "1P"] `shouldReturn` ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]

    -- Two contacts are labelled Alice
    it "offers nothing for a name that several contacts start with" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", alice, "Alice"] []
        _ <- answer home ["newcontact", landlord, "Alice, at home"] []
        bashCompletion home ["send", "ali"] `shouldReturn` []

    -- Both addresses start with 1
    it "offers every contact whose address starts with the word typed" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["send", "1"] `shouldReturn` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\\:", "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ\\:"]

    it "offers nothing once the amount of a recipient is started" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["send", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\\:0.0"] `shouldReturn` []

    it "keeps to the contacts of the network of --network" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", cashAddr, "Bob"] []
        bashCompletion home ["sendall", "--network", "bch", "bo"] `shouldReturn` ["qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a"]
        bashCompletion home ["sendall", "--network", "btc", "bo"] `shouldReturn` []

    it "completes the contact to delete after deletecontact" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["deletecontact", "bc1"] `shouldReturn` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]

    it "completes the address of a contact for label, tag, untag and show" $
      withHome $ \home -> do
        threeContacts home
        bashCompletion home ["label", "bc1"] `shouldReturn` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]
        bashCompletion home ["tag", "bc1"] `shouldReturn` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]
        bashCompletion home ["untag", "bc1"] `shouldReturn` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]
        bashCompletion home ["show", "bc1"] `shouldReturn` ["bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]

walletFileSpec :: Spec
walletFileSpec =
  describe "The address book in the file of a wallet" $ do
    it "is written by exportwallet and put back by importwallet, with the labels, the tags and their colours" $
      withHome $ \home -> withHome $ \fresh -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["tag", "rent", "--color", "yellow"] []
        _ <- answer home ["newcontact", cashAddr] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        imported <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        case imported of
          ResponseImport report _ -> reportContacts report `shouldBe` 2
          other -> expectationFailure (show other)
        (contactOf <$> hwJson fresh ["show", landlord] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["rent"])
        (contactOf <$> hwJson fresh ["show", cashAddr] [])
          `shouldReturn` Right ("bch", "bitcoincash:qpm2qsznhks23z7629mms6s4cwef74vcwvy22gdx6a", "", [])
        listed <- hwJson fresh ["tags"] []
        case listed of
          ResponseTags [use] -> (tagText (shownTag use.tagUseTag), colorName <$> shownTagColor use.tagUseTag, use.tagUseContacts) `shouldBe` ("rent", Just "yellow", 1)
          other -> expectationFailure (show other)

    -- One line of the file: an addr record without an origin, marked as
    -- a contact, which ends with the date the contact was added, the
    -- time of this run
    it "is written as addr records that name their network, say they are contacts and give the date they were added" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        written <- Text.lines . decodeUtf8 <$> BS.readFile (home </> "wallet.jsonl")
        map (fst . Text.breakOn "\"added\":\"") (filter (Text.isInfixOf "\"contact\"") written)
          `shouldBe` ["{\"type\":\"addr\",\"ref\":\"1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU\",\"label\":\"Landlord\",\"network\":\"btc\",\"contact\":true,"]

    -- The second computer imports the file later, and shows the contact
    -- as added when the first computer added it
    it "keeps the date it was added on the computer that imports the file" $
      withHome $ \home -> withHome $ \fresh -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        added <- either fail return . addedOf =<< hwJson home ["contacts"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        (addedOf <$> hwJson fresh ["contacts"] []) `shouldReturn` Right added

    -- The copy of the labels hw made before the deletion has the date
    it "comes back with the date it was added, when a deleted contact is imported from the file written before" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        added <- either fail return . addedOf =<< hwJson home ["contacts"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer home ["deletecontact", landlord, "--yes"] []
        _ <- answer home ["importwallet", home </> "wallet.jsonl"] []
        (addedOf <$> hwJson home ["contacts"] []) `shouldReturn` Right added

    -- A file narrowed to one portfolio is made to show less
    it "is left out of a file of one portfolio" $
      withHome $ \home -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl", "--portfolio", "0"] []
        written <- Text.lines . decodeUtf8 <$> BS.readFile (home </> "wallet.jsonl")
        filter (Text.isInfixOf "\"contact\"") written `shouldBe` []

    -- The contact here is labelled Mr Smith, and Landlord in the file
    it "gives a contact the address book has the label of the file, and adds its tags" $
      withHome $ \home -> withHome $ \other -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer other ["newcontact", landlord, "Mr Smith", "--tag", "kyc"] []
        _ <- answer other ["importwallet", home </> "wallet.jsonl"] []
        (contactOf <$> hwJson other ["show", landlord] [])
          `shouldReturn` Right ("btc", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Landlord", ["kyc", "rent"])

    it "is named on the screen of importwallet, after the labels" $
      withHome $ \home -> withHome $ \fresh -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        shown <- Text.lines . runOut <$> hw fresh ["importwallet", home </> "wallet.jsonl"] []
        filter (Text.isPrefixOf "Address book") shown `shouldBe` ["Address book: 1 contact"]

    it "is shown by show before the file is imported, under a heading of its own" $
      withHome $ \home -> withHome $ \fresh -> do
        newTestPortfolio home
        _ <- answer home ["newcontact", landlord, "Landlord", "--tag", "rent"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        shown <- Text.lines . runOut <$> hw fresh ["show", home </> "wallet.jsonl"] []
        take 2 (dropWhile (/= "Address book") shown)
          `shouldBe` [ "Address book",
                       "  btc  1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU  \"Landlord\"  [rent]"
                     ]

sheetSpec :: Spec
sheetSpec =
  describe "The address book in a spreadsheet" $ do
    let -- The lines of a CSV file, without the mark it starts with
        linesOf file = filter (not . Text.null) . Text.splitOn "\r\n" . Text.drop 1 . decodeUtf8 <$> BS.readFile file
        -- A line without its last three fields: when the contact was
        -- added, which is the time of the test
        withoutCreated = Text.intercalate "," . reverse . drop 3 . reverse . Text.splitOn ","

    it "is written by contacts --csv, a row for each contact, with its address in one piece and in fours" $
      withHome $ \home -> do
        _ <- answer home ["newcontact", alice, "Alice", "--tag", "friends", "--tag", "kyc"] []
        let file = home </> "contacts.csv"
        run <- hw home ["contacts", "--csv", file] []
        runOut run `shouldBe` "Written to " <> Text.pack file <> "\n"
        rows <- linesOf file
        (take 1 rows, withoutCreated <$> drop 1 rows)
          `shouldBe` ( ["Network,Address,Address in fours,Contact label,Contact tags,Created (UTC),Created (local),Created (offset)"],
                       ["btc,bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4,bc1q w508 d6qe jxtd g4y5 r3za rvar y0c5 xw7k v8f3 t4,Alice,friends kyc"]
                     )
