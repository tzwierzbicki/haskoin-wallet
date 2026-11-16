{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tags, through the commands of hw: tag, untag, tags, renametag and
-- deletetag, the
-- listings that keep the rows of a tag, the payments that spend the
-- coins of a tag and keep their change in it, and the tags in the
-- file of a wallet. The home holds portfolio0 of the test wallet on btc.
-- Most tests use two coins of its bip44 account: 50000 satoshis at its
-- first receiving address, bip44/0/0, 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj,
-- and 30000 at its second one, bip44/0/1,
-- 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg.
module Haskoin.Wallet.Workflow.TagSpec (spec) where

import Control.Monad (forM_)
import Data.IORef (modifyIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (Group (..), Response (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (JsonCoin (..), LeftReason (..))
import Haskoin.Wallet.Database.Tag (TagDeleted (..), TagUse (..))
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.HwTestUtils
import Haskoin.Wallet.Labels (ImportReport (..))
import Haskoin.Wallet.Tags (ShownTag (..), colorName, tagText)
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Workflow.TestUtils
import Numeric.Natural (Natural)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec = do
  tagCommandSpec
  colourSpec
  untagSpec
  renameTagSpec
  deleteTagSpec
  txTagSpec
  filterSpec
  screenSpec
  coinControlSpec
  changeTagSpec
  walletFileSpec
  completionSpec

{- What an answer shows of tags -}

-- | The addresses of an answer, each with its tags
addressTagsOf :: Response -> Either String [(Text, [Text])]
addressTagsOf (ResponseAddress _ addr _ tags _) = Right [(dBAddressAddress addr, namesOf tags)]
addressTagsOf (ResponseAddresses groups) = Right [(dBAddressAddress addr, namesOf tags) | g <- groups, (addr, _, tags) <- groupRows g]
addressTagsOf other = Left (show other)

-- | The addresses balances lists, each with the value of its coins, then
-- the total of each network
balancesOf :: Response -> Either String ([(Text, Word64)], [Word64])
balancesOf (ResponseBalances groups) =
  Right
    ( [(dBAddressAddress addr, coinBalanceTotal balance) | (g, _) <- groups, (addr, balance, _) <- groupRows g],
      [coinBalanceTotal total | (_, total) <- groups]
    )
balancesOf other = Left (show other)

-- | The coins of an answer, each with the tags of its address
coinTagsOf :: Response -> Either String [(Word64, [Text])]
coinTagsOf (ResponseCoins groups) = Right [(jsonCoinValue coin, namesOf (jsonCoinAddressTags coin)) | g <- groups, coin <- groupRows g]
coinTagsOf other = Left (show other)

-- | The amount of each transaction txs lists, with the balance after it
amountsOf :: Response -> Either String [(Integer, Maybe Integer)]
amountsOf (ResponseTxs groups) =
  Right [(summaryAmount (txInfoSummary (listedTxInfo listed)), listedTxBalance listed) | g <- groups, listed <- groupRows g]
amountsOf other = Left (show other)

-- | The first receiving address of the home, and the second one
firstAddress, secondAddress :: String
firstAddress = "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
secondAddress = "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"

tagCommandSpec :: Spec
tagCommandSpec =
  describe "tag" $ do
    it "puts tags on an address, which the answer shows" $
      withChain paidChain $ \home _ -> do
        synced home
        (addressTagsOf <$> hwJson home ["tag", firstAddress, "kyc", "cold"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["cold", "kyc"])]

    it "adds to the tags the address has" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        (addressTagsOf <$> hwJson home ["tag", firstAddress, "cold"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["cold", "kyc"])]

    it "takes capital letters as small ones" $
      withChain paidChain $ \home _ -> do
        synced home
        (addressTagsOf <$> hwJson home ["tag", firstAddress, "KYC"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["kyc"])]

    it "tags the address at a path of the active portfolio" $
      withChain paidChain $ \home _ -> do
        synced home
        (addressTagsOf <$> hwJson home ["tag", "bip44/0/0", "kyc"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["kyc"])]

    -- The payment sent left its change at bip44/1/0,
    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn. A change address takes no
    -- label, and takes tags.
    it "tags a change address" $
      withChain paidChain $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (addressTagsOf <$> hwJson home ["tag", "bip44/1/0", "kyc"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"])]

    it "is refused a word alone, with how to give the tags" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", "kyc"] [])
          `shouldReturn` Right "tag needs what to tag, then the tags: hw tag ADDRESS|TXID|PATH TAG. To colour the tag kyc, add --color COLOR"

    it "is refused an address alone, whose tags were left out" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", firstAddress] [])
          `shouldReturn` Right "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj reads as an address. To tag it, give the tags after it: hw tag 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj TAG"

    it "is refused a first word that names nothing to tag" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", "kyc", "cold"] [])
          `shouldReturn` Right "kyc is not an address, a path or the id of a transaction. tag takes what to tag first, then the tags: hw tag ADDRESS|TXID|PATH TAG"

    it "is given to a new address by receive --tag" $
      withChain paidChain $ \home _ -> do
        synced home
        (addressTagsOf <$> hwJson home ["receive", "--tag", "kyc", "--tag", "cold"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", ["cold", "kyc"])]

    it "is listed by tags with how many addresses have it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (tagUsesOf <$> hwJson home ["tags"] [])
          `shouldReturn` Right [("cold", Nothing, 1, 0), ("kyc", Nothing, 2, 0)]

colourSpec :: Spec
colourSpec =
  describe "The colour of a tag" $ do
    it "is given to a tag typed alone with --color" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        (tagUsesOf <$> hwJson home ["tag", "kyc", "--color", "red"] [])
          `shouldReturn` Right [("kyc", Just "red", 1, 0)]

    it "is given to each tag put on an address with --color" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold", "--color", "blue"] []
        (tagUsesOf <$> hwJson home ["tags"] [])
          `shouldReturn` Right [("cold", Just "blue", 1, 0), ("kyc", Just "blue", 1, 0)]

    it "is removed with --color none, and the tag stays" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "--color", "red"] []
        (tagUsesOf <$> hwJson home ["tag", "kyc", "--color", "none"] [])
          `shouldReturn` Right [("kyc", Nothing, 1, 0)]

    it "can be given to a tag nothing has yet" $
      withChain paidChain $ \home _ -> do
        synced home
        (tagUsesOf <$> hwJson home ["tag", "savings", "--color", "green"] [])
          `shouldReturn` Right [("savings", Just "green", 0, 0)]

    -- 61ecd188 has the shape of a short id: the tags of a transaction
    -- were most likely left out
    it "is not given to a new tag that reads as the id of a transaction" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", "61ecd188", "--color", "red"] [])
          `shouldReturn` Right "61ecd188 reads as the id of a transaction, and is no tag yet. To tag it, give the tags after it: hw tag 61ecd188 TAG"

untagSpec :: Spec
untagSpec =
  describe "untag" $ do
    it "takes a tag off an address, and leaves its other tags" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        (addressTagsOf <$> hwJson home ["untag", firstAddress, "kyc"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["cold"])]

    it "is refused a tag the address does not have, and changes nothing" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        (errorOf <$> hwJson home ["untag", firstAddress, "kyc", "cold"] [])
          `shouldReturn` Right "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj does not have the tag cold"
        (addressTagsOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["kyc"])]

    it "leaves the tag in tags, not used, once nothing has it" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 0, 0)]

-- | The tag an answer of renametag shows, as it is now: the name it had,
-- then its name, the name of its colour, and how many addresses and
-- transactions have it
renamedOf :: Response -> Either String (Text, (Text, Maybe Text, Natural, Natural))
renamedOf (ResponseTagRenamed old use) =
  Right (tagText old, (tagText (shownTag use.tagUseTag), colorName <$> shownTagColor use.tagUseTag, use.tagUseAddresses, use.tagUseTxs))
renamedOf other = Left (show other)

-- | The tag an answer of renametag --merge shows, as 'renamedOf' gives it
mergedOf :: Response -> Either String (Text, (Text, Maybe Text, Natural, Natural))
mergedOf (ResponseTagMerged old use) =
  Right (tagText old, (tagText (shownTag use.tagUseTag), colorName <$> shownTagColor use.tagUseTag, use.tagUseAddresses, use.tagUseTxs))
mergedOf other = Left (show other)

renameTagSpec :: Spec
renameTagSpec =
  describe "renametag" $ do
    -- kyc is on both addresses, cold on the first
    it "gives a tag a new name on every address that has it, and says what has it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (renamedOf <$> hwJson home ["renametag", "kyc", "verified"] [])
          `shouldReturn` Right ("kyc", ("verified", Nothing, 2, 0))
        (addressTagsOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", ["verified"]), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["cold", "verified"])]
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("cold", Nothing, 1, 0), ("verified", Nothing, 2, 0)]

    it "gives a tag a new name on a transaction that has it" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        _ <- answer home ["renametag", "salary", "wages"] []
        (txTagsOf <$> hwJson home ["txs", "--tag", "wages"] []) `shouldReturn` Right [["wages"]]

    it "leaves the tag its colour" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "--color", "red"] []
        (renamedOf <$> hwJson home ["renametag", "kyc", "verified"] [])
          `shouldReturn` Right ("kyc", ("verified", Just "red", 1, 0))

    it "takes the capital letters of the new name as small ones" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["renametag", "kyc", "Verified"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("verified", Nothing, 1, 0)]

    it "is refused a word that is no tag" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["renametag", "kyc", "verified"] [])
          `shouldReturn` Right "There is no tag kyc. hw tags lists the tags."

    it "is refused a new name that only differs by its capital letters" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        (errorOf <$> hwJson home ["renametag", "kyc", "KYC"] [])
          `shouldReturn` Right "The tag is named kyc already. Capital letters are taken as small ones."

    it "is refused the name of another tag, with how to merge the two, and changes nothing" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "cold"] []
        (errorOf <$> hwJson home ["renametag", "kyc", "cold"] [])
          `shouldReturn` Right "There is a tag cold already. To merge kyc into it, add --merge: hw renametag kyc cold --merge"
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("cold", Nothing, 1, 0), ("kyc", Nothing, 1, 0)]

    -- The first address has both tags, the second has saving alone:
    -- savings is then on the 2 addresses
    it "with --merge, puts everything that has the tag under another tag, and says what has that one" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "saving", "savings"] []
        _ <- answer home ["tag", secondAddress, "saving"] []
        (mergedOf <$> hwJson home ["renametag", "saving", "savings", "--merge"] [])
          `shouldReturn` Right ("saving", ("savings", Nothing, 2, 0))
        (addressTagsOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", ["savings"]), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["savings"])]
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("savings", Nothing, 2, 0)]

    -- savings was typed savngs
    it "with --merge, is refused a second word that is no tag, and renames nothing" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "saving"] []
        (errorOf <$> hwJson home ["renametag", "saving", "savngs", "--merge"] [])
          `shouldReturn` Right "There is no tag savngs to merge saving into. hw tags lists the tags. To rename saving, leave out --merge."
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("saving", Nothing, 1, 0)]

deleteTagSpec :: Spec
deleteTagSpec =
  describe "deletetag" $ do
    -- kyc is on both addresses
    it "deletes a tag from every address that has it, and says how many had it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        deleted <- hwJson home ["deletetag", "kyc", "--yes"] []
        case deleted of
          ResponseTagDeleted tag had -> (tagText tag, had) `shouldBe` ("kyc", TagDeleted 2 0 0)
          other -> expectationFailure (show other)
        (addressTagsOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", []), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["cold"])]
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("cold", Nothing, 1, 0)]

    it "is refused a word that is no tag" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["deletetag", "kyc"] [])
          `shouldReturn` Right "There is no tag kyc. hw tags lists the tags."

    -- kyc is on both addresses. untag is the command for one of them.
    it "asks first, with how many addresses lose the tag, and names untag for one of them only" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        run <- hw home ["deletetag", "kyc"] ["y"]
        Text.lines (runErr run)
          `shouldBe` [ "The tag kyc will be deleted from the whole database.",
                       "It comes off 2 addresses, and no label or other tag changes.",
                       "To take a tag off one address, transaction or contact and keep the tag, use untag instead.",
                       "Delete the tag kyc? [y/N] "
                     ]

    -- salary is on the first address, on the payment, and on a contact
    it "asks with the addresses, the transactions and the contacts that lose the tag, each counted" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "salary"] []
        _ <- answer home ["tag", shortOf (txidArg payment), "salary"] []
        _ <- answer home ["newcontact", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Employer", "--tag", "salary"] []
        run <- hw home ["deletetag", "salary"] ["n"]
        take 1 (drop 1 (Text.lines (runErr run)))
          `shouldBe` ["It comes off 1 address, 1 transaction and 1 contact, and no label or other tag changes."]

    -- untag took the tag off its only address, and the tag stays
    it "asks with a word that nothing has the tag, when nothing has it" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        run <- hw home ["deletetag", "kyc"] ["n"]
        take 3 (Text.lines (runErr run))
          `shouldBe` [ "The tag kyc will be deleted from the whole database.",
                       "No address, no transaction and no contact has it.",
                       "Delete the tag kyc? [y/N] "
                     ]

    it "deletes the tag once the user answers yes" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        run <- hw home ["deletetag", "kyc"] ["y"]
        runExit run `shouldBe` ExitSuccess
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right []

    it "keeps the tag where it is when the user answers no, and hw fails" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        run <- hw home ["deletetag", "kyc"] ["n"]
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "Nothing was deleted"
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 1, 0)]

    it "asks nothing with --yes" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        run <- hw home ["deletetag", "kyc", "--yes"] []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

txTagSpec :: Spec
txTagSpec =
  describe "The tags of a transaction" $ do
    it "are put on a transaction of the history by its short id" $
      withChain paidChain $ \home _ -> do
        synced home
        (txTagsOf <$> hwJson home ["tag", shortOf (txidArg payment), "salary"] [])
          `shouldReturn` Right [["salary"]]
        (txTagsOf <$> hwJson home ["txs"] []) `shouldReturn` Right [["salary"]]

    it "are counted by tags apart from the addresses" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("salary", Nothing, 0, 1)]

    it "are taken off by untag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary", "bonus"] []
        (txTagsOf <$> hwJson home ["untag", txidArg payment, "bonus"] [])
          `shouldReturn` Right [["salary"]]

    it "are put on a pending transaction by its nosigHash, and shown by pendingtxs" $
      withChain paidChain $ \home _ -> do
        nosig <- paymentSigned home
        _ <- answer home ["tag", nosig, "rent"] []
        (txTagsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right [["rent"]]

    -- The payment is tagged while it is pending. Once mined and synced,
    -- txs lists it first, above the payment it spends from, with its
    -- tag.
    it "stay with a pending transaction once the network has it" $
      withChain paidChain $ \home chain -> do
        nosig <- paymentSigned home
        _ <- answer home ["tag", nosig, "rent"] []
        _ <- answer home ["broadcast", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (txTagsOf <$> hwJson home ["txs"] []) `shouldReturn` Right [["rent"], []]

    -- The payment sent is tagged while the wallet still holds it as
    -- pending. Its replacement is the same payment at a higher fee.
    it "are given to the replacement bumpfee prepares of a payment still pending" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["tag", stuck, "rent"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        ownTagsOf bumped `shouldBe` ["rent"]

    -- The sync finds the payment on the network, so it is tagged as a
    -- transaction of the history
    it "are given to the replacement bumpfee prepares of a payment the sync has found" $
      withChain paidChain $ \home _ -> do
        stuck <- sentPayment home
        _ <- answer home ["sync"] []
        _ <- answer home ["tag", stuck, "rent"] []
        bumped <- txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        ownTagsOf bumped `shouldBe` ["rent"]

    it "are put on a payment as preparetx prepares it, with --tag, and its answer shows them" $
      withChain paidChain $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--tag", "rent"] []
        ownTagsOf prepared `shouldBe` ["rent"]

    -- The coin of 50000 is sent whole
    it "are put on the transaction preparesendall prepares, with --tag" $
      withChain paidChain $ \home _ -> do
        synced home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10", "--tag", "cold"] []
        ownTagsOf prepared `shouldBe` ["cold"]

    -- tags lists the new tag, without a colour, on no address and on one
    -- transaction
    it "are made by --tag of a payment when they do not exist yet, as tag makes them" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--tag", "rent"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("rent", Nothing, 0, 1)]

    it "keep the colour they have when --tag of a payment names them" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", "rent", "--color", "red"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10", "--tag", "rent"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("rent", Just "red", 0, 1)]

    -- 100000 satoshis from a portfolio that holds 50000: nothing is
    -- prepared, so nothing has the tag, and the tag is not made
    it "are not made by --tag of a payment that is refused" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- hwJson home ["preparetx", fixedTo (otherAddr 0) "0.001", "--fee-rate", "10", "--tag", "rent"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right []

    -- --tag says what the payment is for. It chooses no coin: the address
    -- of the coin of 30000 has the tag, and smallest first would spend
    -- that coin anyway, so the coin of 50000 is named with --coin.
    it "choose no coin with --tag of a payment, even when an address has the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "rent"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg twoAddressesTx 0, "--tag", "rent", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    it "go with a pending transaction that is deleted" $
      withChain paidChain $ \home _ -> do
        nosig <- paymentSigned home
        _ <- answer home ["tag", nosig, "rent"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("rent", Nothing, 0, 0)]

filterSpec :: Spec
filterSpec =
  describe "A listing that keeps the rows of a tag" $ do
    it "lists the addresses that have the tag, with --tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (addressTagsOf <$> hwJson home ["addresses", "--tag", "kyc"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", ["kyc"])]

    -- The first address has kyc and the second one cold
    it "lists the addresses that have any one of several tags" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "cold"] []
        (addressTagsOf <$> hwJson home ["addresses", "--tag", "kyc", "--tag", "cold"] [])
          `shouldReturn` Right [("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", ["cold"]), ("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["kyc"])]

    it "is refused a word that is no tag, rather than listing nothing" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["addresses", "--tag", "kyc"] [])
          `shouldReturn` Right "There is no tag kyc. hw tags lists the tags."

    -- The coin of 30000 is at the second address, which has the tag
    it "lists the coins at the addresses that have the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (coinTagsOf <$> hwJson home ["coins", "--tag", "kyc"] []) `shouldReturn` Right [(30000, ["kyc"])]

    -- Only the address of the coin of 30000 has the tag: the total of the
    -- network is what the tag holds
    it "lists the balances of the addresses that have the tag, and adds them up alone" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (balancesOf <$> hwJson home ["balances", "--tag", "kyc"] [])
          `shouldReturn` Right ([("1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", 30000)], [30000])

    -- The chain pays 50000 to the first address in block 800000, then
    -- 30000 to the second one in block 800002. The portfolio held 50000
    -- after the first payment, and 80000 after the second.
    it "lists the transactions that have the tag, each with the balance of the portfolio after it" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        (amountsOf <$> hwJson home ["txs", "--tag", "salary"] []) `shouldReturn` Right [(50000, Just 50000)]

    it "lists the transactions at an address that has the tag, with --address-tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (amountsOf <$> hwJson home ["txs", "--address-tag", "kyc"] []) `shouldReturn` Right [(30000, Just 80000)]

    -- The older payment has the tag salary, and the newer one pays the
    -- address that has the tag kyc
    it "lists the transactions that match --tag or --address-tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (amountsOf <$> hwJson home ["txs", "--tag", "salary", "--address-tag", "kyc"] [])
          `shouldReturn` Right [(30000, Just 80000), (50000, Just 50000)]

    -- The pending payment spends the coin at the first address
    it "lists the pending transactions at an address that has the tag" $
      withChain paidChain $ \home _ -> do
        _ <- paymentSigned home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", "other", "--color", "red"] []
        (fmap length . txTagsOf <$> hwJson home ["pendingtxs", "--address-tag", "kyc"] []) `shouldReturn` Right 1
        (fmap length . txTagsOf <$> hwJson home ["pendingtxs", "--address-tag", "other"] []) `shouldReturn` Right 0

screenSpec :: Spec
screenSpec =
  describe "The screen of a listing that keeps the rows of a tag" $ do
    it "starts with a line that names the tag, then a blank line" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44", "--tag", "kyc"] []
        take 4 listed
          `shouldBe` [ "Addresses tagged kyc",
                       "",
                       "bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  [kyc]",
                       "  spendable 0.00030000 btc, received 0.00030000 btc in 1 transaction"
                     ]

    it "names several tags with or between them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "cold"] []
        listed <- Text.lines . runOut <$> hw home ["coins", "--tag", "kyc", "--tag", "cold"] []
        take 1 listed `shouldBe` ["Coins at addresses tagged kyc or cold"]

    it "names the tags of transactions and those of addresses apart" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["txs", "--tag", "salary", "--address-tag", "kyc"] []
        take 1 listed `shouldBe` ["Transactions tagged salary, or at addresses tagged kyc"]

    it "has no such line without a tag asked for" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44"] []
        take 1 listed `shouldBe` ["bip44/0/1  1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  [kyc]"]

    -- The tags are the last column but one, before the label
    it "gives the tags of an address a column of the table, before its label" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["label", firstAddress, "Wages"] []
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44", "--tabular"] []
        listed
          `shouldBe` [ "Path      │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Tags         │ Label",
                       "──────────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼──────────────┼──────",
                       "bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ cold kyc     │ \"Wages\""
                     ]

    -- The second line of the compact view of a transaction ends with its
    -- tags, each between brackets
    it "ends the lines of a transaction with its tags" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        listed <- Text.lines . runOut <$> hw home ["txs", "--network", "btc"] []
        drop 1 (take 2 listed) `shouldBe` ["  balance 0.00050000 btc, at bip44/0/0, [salary]"]

    -- The short views of txs show the tags of an address only when the
    -- listing was asked to keep them
    it "shows the tag asked for beside the address that has it, with --address-tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        kept <- Text.lines . runOut <$> hw home ["txs", "--network", "btc", "--address-tag", "kyc"] []
        drop 3 (take 4 kept) `shouldBe` ["  balance 0.00050000 btc, at bip44/0/0 [kyc]"]
        whole <- Text.lines . runOut <$> hw home ["txs", "--network", "btc"] []
        drop 1 (take 2 whole) `shouldBe` ["  balance 0.00050000 btc, at bip44/0/0"]

    it "lists the tags by name, with their colours and what has them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "--color", "red"] []
        _ <- answer home ["tag", secondAddress, "kyc", "cold"] []
        _ <- answer home ["tag", txidArg twoAddressesTx, "salary"] []
        _ <- answer home ["tag", "spare", "--color", "blue"] []
        listed <- Text.lines . runOut <$> hw home ["tags"] []
        listed
          `shouldBe` [ "cold          1 address",
                       "kyc     red   2 addresses",
                       "salary        1 transaction",
                       "spare   blue  not used"
                     ]

coinControlSpec :: Spec
coinControlSpec =
  describe "Coin control by tag" $ do
    -- The coin of 30000 at the second address pays 20000: a transaction
    -- of one input and two outputs is 226 bytes, a fee of 2260, and
    -- 30000 - 20000 - 2260 = 7740 of change
    it "spends the coins at the addresses that have the tag of --spend-from-tag, and only them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 7740)]

    -- Each address has a tag of its own. Either coin would cover 20000,
    -- yet both are spent: two inputs and two outputs make 374 bytes, a
    -- fee of 3740, and 80000 - 20000 - 3740 = 56260 of change.
    it "spends the coins of every tag given" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "cold"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--spend-from-tag", "cold", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (externalAddr 1, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 56260)]

    -- --coin names the coin of 50000 at the first address, and the tag
    -- the address of the coin of 30000
    it "adds the coins of the tag to those of --coin" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg twoAddressesTx 0, "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (externalAddr 1, 30000)]

    -- The tag is on a bip44 address, which holds 50000, and on a bip84
    -- one, which holds 30000. --coin-type narrows the group to its bip44
    -- coin: a fee of 2260, and 50000 - 20000 - 2260 = 27740 of change.
    it "spends the coins of the tag that are of the type of --coin-type, and leaves its others" $
      withChain (confirmedChain bothTypesTx) $ \home _ -> do
        syncedBothTypes home
        _ <- answer home ["tag", "bip44/0/0", "kyc"] []
        _ <- answer home ["tag", "bip84/0/0", "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--coin-type", "bip44", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    it "is refused a word that is no tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] [])
          `shouldReturn` Right "There is no tag kyc. hw tags lists the tags."

    it "is refused a tag that no address of the portfolio has" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", "kyc", "--color", "red"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] [])
          `shouldReturn` Right "No address of portfolio0 of wallet0 has the tag kyc: hw addresses --tag kyc lists those of the active portfolio"

    -- The coin of 30000 is sent whole: one input and one output make 192
    -- bytes, a fee of 1920, and 30000 - 1920 = 28080 for the recipient.
    -- The coin of 50000 stays, since no chooser chose it.
    it "sends only the coins of the tag with preparesendall" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        answered <- answer home ["preparesendall", textOf (otherAddr 0), "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        prepared <- txOf answered
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 30000)]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 28080)]
        leftOf answered `shouldBe` Right [(OutPoint (txHash twoAddressesTx) 0, 50000, NotGiven)]

changeTagSpec :: Spec
changeTagSpec =
  describe "The change of a payment" $ do
    -- The coin of 30000 at /0/1, tagged kyc, pays 20000: the change goes
    -- to the new change address /1/0
    it "takes the tag of the coins --spend-from-tag spends, at its new change address" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` ["kyc"]

    -- The portfolio holds one coin, at /0/0. No option names a tag.
    it "takes the tag of the coins the payment selects, without coin control" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` ["kyc"]

    -- 60000 takes both coins. /0/0 has kyc and cold, /0/1 has kyc alone.
    it "takes the tags every coin spent has, and not a tag one of them lacks" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` ["kyc"]

    -- 60000 takes both coins, and the one at /0/1 has no tag
    it "takes no tag when one of the coins spent has none" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- The change address is of the group from then on: coins --tag lists
    -- the coin of the group, and once the payment is mined, its change of
    -- 30000 - 20000 - 2260 = 7740
    it "is a coin of the group once the payment is mined" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (coinTagsOf <$> hwJson home ["coins", "--tag", "kyc"] []) `shouldReturn` Right [(7740, ["kyc"])]

    -- The coin at /0/1 is tagged kyc, and the change goes back to /0/0,
    -- which has no tag
    it "puts no tag on the address --change-back names" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--change-back", firstAddress, "--fee-rate", "10"] []
        addressTagsIn prepared (externalAddr 0) `shouldBe` []

    -- The change goes back to /0/0, tagged cold, and the coin spent is
    -- tagged kyc
    it "leaves the address --change-back names the tags it has" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "cold"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--change-back", firstAddress, "--fee-rate", "10"] []
        addressTagsIn prepared (externalAddr 0) `shouldBe` ["cold"]

    -- Once the payment is deleted, kyc is on the address of the coin
    -- alone again
    it "loses its tag when deletetx gives its change address back" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 1, 0)]

    -- The first payment tags the change address /1/0 kyc, and is deleted.
    -- The tag is then taken off the coin, so the second payment, which
    -- takes /1/0 again, has no tag to give it.
    it "has no tag left of a deleted payment when the next payment takes the same change address" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- A first payment makes the change address /1/0, and is deleted. The
    -- address is free, and is tagged cold by hand. The coin of the second
    -- payment has no tag, so its change, at /1/0 again, has none.
    it "has the tags of its coins and no other, even one put on its address while it was free" $
      withChain paidChain $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        _ <- answer home ["tag", "bip44/1/0", "cold"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- The two ways to pay keep the transaction through the same step, so
    -- both tag its change
    forM_ [(WithSend, "send"), (WithThreeCommands, "preparetx, signtx and broadcast")] $ \(way, name) ->
      it ("takes the tag of the coin spent when the payment is made with " <> name) $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- answer home ["tag", firstAddress, "kyc"] []
          sent <- paidWith way home [fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
          addressTagsIn sent (internalAddr 0) `shouldBe` ["kyc"]

    -- No mnemonic is typed, so send keeps nothing of the payment: kyc is
    -- on the address of the coin alone, as before
    it "is not tagged by a send stopped at the mnemonic" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- hwJson home ["send", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 1, 0)]

    -- The change of 27740 goes to the first change address of the
    -- account, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, and the line under it
    -- shows its tags, as under the address the payment spends from
    it "shows its tag on the screen, under its address" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        shown <- Text.lines . runOut <$> hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        take 2 (dropWhile (not . Text.isPrefixOf "  Change") shown)
          `shouldBe` [ "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0",
                       "             [kyc]"
                     ]

walletFileSpec :: Spec
walletFileSpec =
  describe "The tags in the file of a wallet" $ do
    -- A wallet that never synced: importwallet creates the portfolio,
    -- derives the address of the record, and tags the transaction by the
    -- origin of its record
    it "are written by exportwallet and put back by importwallet, with their colours" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "--color", "red"] []
        _ <- answer home ["tag", txidArg payment, "salary"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        imported <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        case imported of
          ResponseImport report _ -> (reportAddressTags report, reportTxTags report) `shouldBe` (1, 1)
          other -> expectationFailure (show other)
        (tagUsesOf <$> hwJson fresh ["tags"] [])
          `shouldReturn` Right [("kyc", Just "red", 1, 0), ("salary", Nothing, 0, 1)]
        (addressTagsOf <$> hwJson fresh ["addresses"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["kyc"])]

    -- The tag kyc is blue here, and red in the file
    it "keep the colour a tag has already, whatever the file says" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "--color", "red"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer fresh ["tag", "kyc", "--color", "blue"] []
        _ <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        (tagUsesOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right [("kyc", Just "blue", 1, 0)]

    it "are named on the screen of importwallet, after the labels" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        shown <- Text.lines . runOut <$> hw fresh ["importwallet", home </> "wallet.jsonl"] []
        filter (Text.isPrefixOf "Tags set") shown `shouldBe` ["Tags set: 1 address"]

completionSpec :: Spec
completionSpec =
  describe "The Tab key" $ do
    it "completes a tag after --tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        bashCompletion home ["addresses", "--tag", "k"] `shouldReturn` ["kyc"]

    it "completes a tag after --tag of send, which tags the payment" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        bashCompletion home ["send", "--tag", "k"] `shouldReturn` ["kyc"]

    it "offers every tag after --spend-from-tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        bashCompletion home ["preparetx", "--spend-from-tag", ""] `shouldReturn` ["cold", "kyc"]

    it "completes the tag to rename after renametag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        bashCompletion home ["renametag", "c"] `shouldReturn` ["cold"]

    -- The second word of renametag may be the tag to merge into
    it "completes a tag for the new name of renametag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        bashCompletion home ["renametag", "kyc", "c"] `shouldReturn` ["cold"]

    it "completes the tag to delete after deletetag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        bashCompletion home ["deletetag", "c"] `shouldReturn` ["cold"]

    it "completes a tag typed with a capital letter" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        bashCompletion home ["untag", firstAddress, "K"] `shouldReturn` ["kyc"]
