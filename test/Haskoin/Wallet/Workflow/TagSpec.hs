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

import Control.Monad (forM_, void)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json (parseEither)
import Data.IORef (IORef, modifyIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Haskoin
import Haskoin.Wallet.Cli.Response (ChangeFollowed (..), ChangeStop (..), Group (..), Response (..))
import Haskoin.Wallet.Database (DBAddress (..))
import Haskoin.Wallet.Database.Address (CoinBalance (..))
import Haskoin.Wallet.Database.Coin (CoinLock (..), JsonCoin (..), LeftReason (..))
import Haskoin.Wallet.Database.PendingTx (Retag (..))
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
  untaggedSpec
  screenSpec
  coinControlSpec
  lockingTagSpec
  changeTagSpec
  mixingSpec
  changeChainSpec
  walletFileSpec
  completionSpec

{- What an answer shows of tags -}

-- | The addresses of an answer, each with its tags
addressTagsOf :: Response -> Either String [(Text, [Text])]
addressTagsOf (ResponseAddress _ addr _ tags _ _) = Right [(dBAddressAddress addr, namesOf tags)]
addressTagsOf (ResponseAddresses groups) = Right [(dBAddressAddress addr, namesOf tags) | g <- groups, (addr, _, tags) <- groupRows g]
addressTagsOf other = Left (show other)

-- | The change addresses an answer of tag says took tags with its
-- address, each with its tags as they are now: none when it names none
changeTaggedOf :: Response -> Either String [(Text, [Text])]
changeTaggedOf = changeFollowedOf AddTags

-- | The change addresses an answer of untag says lost tags with its
-- address, each with the tags it has left
changeUntaggedOf :: Response -> Either String [(Text, [Text])]
changeUntaggedOf = changeFollowedOf RemoveTags

-- | The change addresses an answer of tag says the tags stopped at, each
-- with the tags it did not take, the hash of the payment that stopped
-- them, and the address that payment also spends from
changeStopsOf :: Response -> Either String [(Text, [Text], String, Maybe Text)]
changeStopsOf (ResponseAddress _ _ _ _ followedM _) =
  Right
    [ (dBAddressAddress addr, namesOf stop.changeStopTags, Text.unpack (txHashToHex stop.changeStopPayment), (\(source, _, _) -> dBAddressAddress source) <$> stop.changeStopSource)
      | Just followed <- [followedM],
        stop@ChangeStop {changeStopAt = (addr, _, _)} <- followed.followedStops
    ]
changeStopsOf other = Left (show other)

changeFollowedOf :: Retag -> Response -> Either String [(Text, [Text])]
changeFollowedOf retag (ResponseAddress _ _ _ _ followedM _) =
  Right [(dBAddressAddress addr, namesOf tags) | Just (ChangeFollowed did rows _) <- [followedM], did == retag, (addr, _, tags) <- rows]
changeFollowedOf _ other = Left (show other)

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

-- | The label of each transaction txs lists
labelsOf :: Response -> Either String [Text]
labelsOf (ResponseTxs groups) = Right [txInfoLabel (listedTxInfo listed) | g <- groups, listed <- groupRows g]
labelsOf other = Left (show other)

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

    -- The tag is typed twice by a slip: the address has it once
    it "takes a tag named twice off once" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        (addressTagsOf <$> hwJson home ["untag", firstAddress, "kyc", "kyc"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", ["cold"])]

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

    -- The payment pays portfolio0 and portfolio1, and has the tag in both
    it "says that one transaction has the tag, of a transaction that two portfolios hold" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["tag", txidArg twoPortfoliosTx, "transfer"] []
        (renamedOf <$> hwJson home ["renametag", "transfer", "moved"] [])
          `shouldReturn` Right ("transfer", ("moved", Nothing, 0, 1))

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
                       "To take a tag off one address, transaction or contact and keep the tag, use",
                       "untag instead.",
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
        take 2 (drop 1 (Text.lines (runErr run)))
          `shouldBe` ["It comes off 1 address, 1 transaction and 1 contact, and no label or other tag", "changes."]

    -- The payment pays portfolio0 and portfolio1, and has the tag in both
    it "asks with one transaction, and says that one had the tag, of a transaction that two portfolios hold" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        syncedTwoPortfolios home
        _ <- answer home ["tag", txidArg twoPortfoliosTx, "transfer"] []
        run <- hw home ["deletetag", "transfer"] ["y"]
        take 1 (drop 1 (Text.lines (runErr run)))
          `shouldBe` ["It comes off 1 transaction, and no label or other tag changes."]
        Text.lines (runOut run) `shouldBe` ["Deleted the tag transfer", "  It was on 1 transaction"]

    -- kyc is on the first address, and locks its coins
    it "says before its question that a tag that locks its coins will free them" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", "kyc", "--lock-coins"] []
        run <- hw home ["deletetag", "kyc"] ["n"]
        run `shouldSatisfy` printed "It locks its coins. Once it is deleted, any payment may spend the coins at its addresses. Delete the tag kyc? [y/N] "

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

    -- The payment pays portfolio0 and portfolio1, and tag puts the tag on
    -- it in both: the user tagged one transaction
    it "are counted once by tags on a transaction that two portfolios hold" $
      withChain (confirmedChain twoPortfoliosTx) $ \home _ -> do
        syncedTwoPortfolios home
        (txTagsOf <$> hwJson home ["tag", txidArg twoPortfoliosTx, "transfer"] [])
          `shouldReturn` Right [["transfer"], ["transfer"]]
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("transfer", Nothing, 0, 1)]

    -- portfolio0 pays 20000 to the address of portfolio1, with a tag and
    -- a label. Once the sync has found the payment, portfolio1 shows it
    -- with both, as if tag and label had been given its id afterwards.
    it "are put by --tag of a payment on the transaction in every portfolio that it pays, with its label" $
      withChain (confirmedChain twoPortfoliosTx) $ \home chain -> do
        syncedTwoPortfolios home
        _ <- paidWith WithSend home [fixedTo account1Addr "0.0002", "--fee-rate", "10", "--tag", "transfer", "--label", "to the other portfolio"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        switchTo home 0 1
        listed <- hwJson home ["txs", "--tag", "transfer"] []
        txTagsOf listed `shouldBe` Right [["transfer"]]
        labelsOf listed `shouldBe` Right ["to the other portfolio"]

    it "are counted once by tags on such a payment" $
      withChain (confirmedChain twoPortfoliosTx) $ \home chain -> do
        syncedTwoPortfolios home
        _ <- paidWith WithSend home [fixedTo account1Addr "0.0002", "--fee-rate", "10", "--tag", "transfer"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("transfer", Nothing, 0, 1)]

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

untaggedSpec :: Spec
untaggedSpec =
  describe "A listing that keeps the rows without a tag, with --untagged" $ do
    -- The second address has a tag: the coin of 50000 at the first one is
    -- at an address without a tag
    it "lists the coins at the addresses that have no tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (coinTagsOf <$> hwJson home ["coins", "--untagged"] []) `shouldReturn` Right [(50000, [])]

    it "lists the balances of the addresses that have no tag, and adds them up alone" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (balancesOf <$> hwJson home ["balances", "--untagged"] [])
          `shouldReturn` Right ([("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", 50000)], [50000])

    it "leaves the tagged address out of addresses" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        listed <- fmap (map fst) . addressTagsOf <$> hwJson home ["addresses", "--network", "btc", "--type", "bip44", "--untagged"] []
        fmap (filter (`elem` ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"])) listed
          `shouldBe` Right ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    -- The older payment of 50000 has the tag salary. The newer one, of
    -- 30000, after which the portfolio holds 80000, is left to sort.
    it "lists the transactions left to sort, each with the balance of the portfolio after it" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        (amountsOf <$> hwJson home ["txs", "--untagged"] []) `shouldReturn` Right [(30000, Just 80000)]

    -- Sorting is tagging: the listing empties as the transactions get
    -- their tags
    it "lists no transaction once each has a tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        _ <- answer home ["tag", txidArg newerPayment, "salary"] []
        (amountsOf <$> hwJson home ["txs", "--untagged"] []) `shouldReturn` Right []

    -- Both payments were paid from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU.
    -- Once that address is a contact with a tag, its tag says what they
    -- are, and none is left to sort.
    it "leaves out the transactions paid from a contact that has a tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "Employer"] []
        (fmap length . amountsOf <$> hwJson home ["txs", "--untagged"] []) `shouldReturn` Right 2
        _ <- answer home ["tag", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", "salary"] []
        (fmap length . amountsOf <$> hwJson home ["txs", "--untagged"] []) `shouldReturn` Right 0

    -- The tag of an address of the portfolio groups its coins. It does
    -- not say what a payment to it is for.
    it "still lists a transaction that pays an address of the portfolio that has a tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        (amountsOf <$> hwJson home ["txs", "--untagged"] [])
          `shouldReturn` Right [(30000, Just 80000), (50000, Just 50000)]

    -- The older payment has the tag salary, and the newer one has none
    it "lists the transactions of a tag as well, with --tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        (amountsOf <$> hwJson home ["txs", "--untagged", "--tag", "salary"] [])
          `shouldReturn` Right [(30000, Just 80000), (50000, Just 50000)]

    it "lists a pending transaction until it has a tag" $
      withChain paidChain $ \home _ -> do
        nosig <- paymentSigned home
        (fmap length . txTagsOf <$> hwJson home ["pendingtxs", "--untagged"] []) `shouldReturn` Right 1
        _ <- answer home ["tag", nosig, "rent"] []
        (fmap length . txTagsOf <$> hwJson home ["pendingtxs", "--untagged"] []) `shouldReturn` Right 0

    it "starts the screen of addresses with a line that says so" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44", "--untagged", "--reverse"] []
        take 3 listed
          `shouldBe` [ "Addresses without a tag",
                       "",
                       "bip44/0/0  1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"
                     ]

    it "says on the screen of coins that the addresses have no tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        listed <- Text.lines . runOut <$> hw home ["coins", "--untagged"] []
        take 2 listed `shouldBe` ["Coins at addresses without a tag", ""]

    -- The older payment has a tag. The newer one, of 30000 to the second
    -- address, is listed with the 80000 the portfolio holds after it.
    it "says on the screen of txs that the contacts count, above the transactions left to sort" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        listed <- Text.lines . runOut <$> hw home ["txs", "--network", "btc", "--untagged"] []
        (take 2 listed, drop 3 listed)
          `shouldBe` ( ["Transactions without a tag or a tagged contact", ""],
                       ["  balance 0.00080000 btc, at bip44/0/1"]
                     )

    it "names the tags kept first on the screen, then the rows without a tag" $
      withChain olderAndNewerChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        txs <- Text.lines . runOut <$> hw home ["txs", "--tag", "salary", "--address-tag", "kyc", "--untagged"] []
        take 1 txs `shouldBe` ["Transactions tagged salary, or at addresses tagged kyc, or without a tag or a tagged contact"]
        balances <- Text.lines . runOut <$> hw home ["balances", "--tag", "kyc", "--untagged"] []
        take 1 balances `shouldBe` ["Addresses tagged kyc, or without a tag"]

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
                       "  spendable 0.00030000 btc, received 0.00030000 btc, 1 transaction"
                     ]

    it "names several tags with or between them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "cold"] []
        listed <- Text.lines . runOut <$> hw home ["coins", "--tag", "kyc", "--tag", "cold"] []
        take 1 listed `shouldBe` ["Coins at addresses tagged kyc or cold"]

    it "names once a tag asked for twice" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["coins", "--tag", "kyc", "--tag", "kyc"] []
        take 2 listed `shouldBe` ["Coins at addresses tagged kyc", ""]

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
          `shouldBe` [ "Path      │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Tags     │ Label",
                       "──────────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼──────────┼──────",
                       "bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │ cold kyc │ \"Wages\""
                     ]

    -- "donations kyc" takes 13 characters, one more than the column had
    -- when its width was fixed. The label of the address without a tag
    -- starts under the heading, as that of the address with two tags.
    it "makes the column of the tags as wide as the widest tags of the database, so that every label is in line" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["label", firstAddress, "Wages"] []
        _ <- answer home ["label", secondAddress, "Gifts"] []
        _ <- answer home ["tag", secondAddress, "donations", "kyc"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44", "--tabular"] []
        listed
          `shouldBe` [ "Path      │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Tags          │ Label",
                       "──────────┼────────────────────────────────────┼─────────────────┼────────────────┼───────┼───────────────┼──────",
                       "bip44/0/1 │ 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg │      0.00030000 │     0.00030000 │     1 │ donations kyc │ \"Gifts\"",
                       "bip44/0/0 │ 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj │      0.00050000 │     0.00050000 │     1 │               │ \"Wages\""
                     ]

    -- A tag of a transaction does not widen the tables of addresses
    it "leaves the column of the tags of addresses as wide as its heading when only a transaction has tags" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg payment, "salary", "bonus"] []
        listed <- Text.lines . runOut <$> hw home ["addresses", "--network", "btc", "--type", "bip44", "--tabular"] []
        take 1 listed
          `shouldBe` ["Path      │ Address                            │ Spendable (btc) │ Received (btc) │   Txs │ Tags │ Label"]

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
    -- fee of 3740, and 80000 - 20000 - 3740 = 56260 of change. --yes
    -- answers the question on coins of different tags.
    it "spends the coins of every tag given" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "cold"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--spend-from-tag", "cold", "--fee-rate", "10", "--yes"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (externalAddr 1, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 56260)]

    -- --coin names the coin of 50000 at the first address, and the tag
    -- the address of the coin of 30000. --yes answers the question on
    -- coins of different tags.
    it "adds the coins of the tag to those of --coin" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg twoAddressesTx 0, "--spend-from-tag", "kyc", "--fee-rate", "10", "--yes"] []
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

-- The portfolio holds 50000 at its first receiving address and 30000 at
-- its second one. In most tests the second address has the tag savings,
-- which locks its coins.
lockingTagSpec :: Spec
lockingTagSpec =
  describe "A tag that locks its coins" $ do
    let savingsLocksSecond home = do
          synced home
          _ <- answer home ["tag", secondAddress, "savings"] []
          answer home ["tag", "savings", "--lock-coins"] []
        firstCoin = OutPoint (txHash twoAddressesTx) 0
        secondCoin = OutPoint (txHash twoAddressesTx) 1
        lockOf = fmap (fmap (\(outpoint, _, _, lock, spendable) -> (outpoint, lock, spendable)))
        -- A payment of 20000 from the coin of 30000 at the second address,
        -- prepared while no tag locks the coin, with the options given
        -- besides: its nosigHash
        secondSpent home options = do
          synced home
          nosigOf =<< txOf =<< answer home (["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", secondAddress, "--fee-rate", "10"] <> options) []
        -- A payment of 20000 that names the tag, once savings locks the
        -- coin of 30000: its nosigHash
        namingSavings = namingSavingsWith []
        namingSavingsWith options home = do
          _ <- savingsLocksSecond home
          nosigOf =<< txOf =<< answer home (["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "savings", "--fee-rate", "10"] <> options) []
        coin = shortCoinArg twoAddressesTx 1
        lockedBySavings =
          Text.pack ("The transaction spends the coin " <> coin <> ", which the tag savings locks. A payment made with --spend-from-tag savings spends it, and hw tag savings --unlock-coins unlocks it")
        -- What a command that locks the coin says of the pending
        -- transaction that spends it, named by its short id
        stopped short =
          Text.pack ("The pending transaction " <> short <> " spends the coin " <> coin <> ", which is locked now. It can no longer be signed or sent: deletetx " <> short <> " deletes it")
        -- The savings at the second address, a withdrawal of 20000 from
        -- them with --tag-change, mined and synced, and the file of the
        -- wallet written after it
        withdrawnAndExported home chain = do
          nosig <- namingSavingsWith ["--tag-change"] home
          _ <- answer home ["signtx", nosig] seedOnce
          _ <- answer home ["broadcast", nosig, "--yes"] []
          modifyIORef chain mine
          _ <- answer home ["sync"] []
          void $ answer home ["exportwallet", home </> "wallet.jsonl"] []

    it "locks them once tag is given --lock-coins, which tags then says" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "savings"] []
        (tagLocksOf <$> hwJson home ["tag", "savings", "--lock-coins"] []) `shouldReturn` Right [("savings", True)]
        (tagLocksOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("savings", True)]

    it "does not lock them until it is told to" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "savings"] []
        (tagLocksOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("savings", False)]

    -- savings is on the transaction that paid both coins, and on no
    -- address
    it "locks no coin by being on a transaction: the coins it locks are at its addresses" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", txidArg twoAddressesTx, "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        (lockOf . coinsShown <$> hwJson home ["coins"] [])
          `shouldReturn` Right [(firstCoin, Nothing, True), (secondCoin, Nothing, True)]

    it "can be made to lock its coins before any address has it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        (tagUsesOf <$> hwJson home ["tag", "savings", "--lock-coins"] []) `shouldReturn` Right [("savings", Nothing, 0, 0)]
        (tagLocksOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("savings", True)]

    it "stops locking them once tag is given --unlock-coins" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (tagLocksOf <$> hwJson home ["tag", "savings", "--unlock-coins"] []) `shouldReturn` Right [("savings", False)]

    it "takes a colour in the same command" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        answered <- hwJson home ["tag", "savings", "--lock-coins", "--color", "blue"] []
        tagUsesOf answered `shouldBe` Right [("savings", Just "blue", 0, 0)]
        tagLocksOf answered `shouldBe` Right [("savings", True)]

    it "is refused --unlock-coins when it is no tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", "savings", "--unlock-coins"] [])
          `shouldReturn` Right "There is no tag savings. hw tags lists the tags."

    it "is refused --lock-coins and --unlock-coins together" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", "savings", "--lock-coins", "--unlock-coins"] [])
          `shouldReturn` Right "--lock-coins and --unlock-coins say opposite things: give one of them"

    -- With an address before the tag, the option would read as locking
    -- the coins of that address alone
    it "is not made to lock its coins by a command that tags an address" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", secondAddress, "savings", "--lock-coins"] [])
          `shouldReturn` Right "--lock-coins goes with a tag alone, and is about the coins at every address that has the tag: hw tag savings --lock-coins. To lock one coin, use lockcoin."
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right []

    it "is not unlocked by a command that names an address, which says what unlocks one coin" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (errorOf <$> hwJson home ["tag", secondAddress, "savings", "--unlock-coins"] [])
          `shouldReturn` Right "--unlock-coins goes with a tag alone, and is about the coins at every address that has the tag: hw tag savings --unlock-coins. To unlock one coin, use unlockcoin."

    -- 20000 is paid from the coin of 50000, the only one a payment may
    -- spend: one input and two outputs are 226 bytes, a fee of 2260, and
    -- 50000 - 20000 - 2260 = 27740 of change
    it "keeps its coins out of a payment that does not name it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 27740)]

    -- 60000 needs both coins, and the coin of 30000 is locked
    it "has a payment that needs its coins refused, with what the portfolio cannot spend now" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] [])
          `shouldReturn` Right "The coins do not cover the amount and the fee. The portfolio holds 0.00030000 btc more on btc, in 1 coin it cannot spend now: hw coins says why"

    -- The coin of 30000 pays 20000: a fee of 2260, and 30000 - 20000 -
    -- 2260 = 7740 of change
    it "has its coins spent by a payment that names it with --spend-from-tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "savings", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 7740)]

    it "is not opened by --coin, which names the coin and not the tag: the refusal names the tag and the option that opens it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg twoAddressesTx 1, "--fee-rate", "10"] [])
          `shouldReturn` Right (Text.pack ("None of the coins chosen can be spent now: " <> txidArg twoAddressesTx <> ":1, the tag savings locks it. Only a payment that names the tag spends its coins: --spend-from-tag savings"))

    it "is not opened by --spend-from, which names the address and not the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", secondAddress, "--fee-rate", "10"] [])
          `shouldReturn` Right (Text.pack ("None of the coins chosen can be spent now: " <> txidArg twoAddressesTx <> ":1, the tag savings locks it. Only a payment that names the tag spends its coins: --spend-from-tag savings"))

    -- The second address has two tags that lock, savings and cold. The
    -- payment names cold: the refusal must not read as a refusal of cold.
    it "is named by the refusal of a payment that names another tag of the address, which locks too" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["tag", secondAddress, "cold"] []
        _ <- answer home ["tag", "cold", "--lock-coins"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "cold", "--fee-rate", "10"] [])
          `shouldReturn` Right (Text.pack ("None of the coins chosen can be spent now: " <> txidArg twoAddressesTx <> ":1, the tag savings locks it. Only a payment that names the tag spends its coins: --spend-from-tag savings"))

    it "is named with the other tag that locks the coin, by the refusal of a payment that names neither" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["tag", secondAddress, "cold"] []
        _ <- answer home ["tag", "cold", "--lock-coins"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--coin", shortCoinArg twoAddressesTx 1, "--fee-rate", "10"] [])
          `shouldReturn` Right (Text.pack ("None of the coins chosen can be spent now: " <> txidArg twoAddressesTx <> ":1, the tags cold and savings lock it. Only a payment that names these tags spends their coins: --spend-from-tag cold --spend-from-tag savings"))

    -- Both tags are named: the coin of 30000 pays 20000 and a fee of
    -- 2260, and leaves 7740
    it "has its coins spent by a payment that names both tags that lock them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["tag", secondAddress, "cold"] []
        _ <- answer home ["tag", "cold", "--lock-coins"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "cold", "--spend-from-tag", "savings", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 30000)]

    -- Both addresses have the tag kyc, and the second one savings too.
    -- The payment names kyc alone: it spends the coin of 50000, and lists
    -- the coin of 30000 among those it leaves.
    it "is not opened by another tag of the address, and the answer lists the coin as left for its lock" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        answered <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        prepared <- txOf answered
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        leftOf answered `shouldBe` Right [(secondCoin, 30000, Locked LockedByTag)]

    -- The same payment on the screen: under the coin it leaves, the tag
    -- that locks it, and not kyc, which the payment named
    it "is named under a coin that a payment leaves for its lock" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        shown <- Text.lines . runOut <$> hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        take 2 (dropWhile (not . Text.isPrefixOf "  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg") shown)
          `shouldBe` ["  at 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  bip44/0/1  [kyc] [savings]", "  the tag savings locks it"]

    -- The coin of 50000 is sent whole: one input and one output are 192
    -- bytes, a fee of 1920, and 50000 - 1920 = 48080 for the recipient
    it "keeps its coins out of preparesendall, which lists them as left for their lock" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        answered <- answer home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        prepared <- txOf answered
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 48080)]
        leftOf answered `shouldBe` Right [(secondCoin, 30000, Locked LockedByTag)]

    -- The coin of 30000 is sent whole: 30000 - 1920 = 28080
    it "has its coins sent by preparesendall with --spend-from-tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        prepared <- txOf =<< answer home ["preparesendall", textOf (otherAddr 0), "--spend-from-tag", "savings", "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 1, 30000)]
        paidToOthers prepared `shouldBe` Map.fromList [(otherAddr 0, 28080)]

    -- 60000 needs both coins: two inputs and two outputs are 374 bytes, a
    -- fee of 3740, and 80000 - 60000 - 3740 = 16260 of change. --yes
    -- answers the question on coins of different tags.
    it "frees its coins for every payment once it is told to unlock them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["tag", "savings", "--unlock-coins"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10", "--yes"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (externalAddr 1, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 16260)]

    -- The two coins are outputs of one transaction: coins lists them in
    -- the order of their outputs
    it "has its coins shown by coins as locked by a tag and not spendable" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (lockOf . coinsShown <$> hwJson home ["coins"] [])
          `shouldReturn` Right [(firstCoin, Nothing, True), (secondCoin, Just LockedByTag, False)]

    it "has its coins listed by coins --locked" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (lockOf . coinsShown <$> hwJson home ["coins", "--locked"] [])
          `shouldReturn` Right [(secondCoin, Just LockedByTag, False)]

    -- 80000 in all: 50000 spendable, and 30000 locked by a tag
    it "has the value of its coins counted by balances as locked by a tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        answered <- hwJson home ["balances"] []
        case answered of
          ResponseBalances groups -> map snd groups `shouldBe` [CoinBalance 80000 50000 0 0 30000 0]
          other -> expectationFailure (show other)

    -- lockcoin holds the coin for a payment that names the tag too
    it "does not open a coin that lockcoin locks as well, for a payment that names it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["lockcoin", shortCoinArg twoAddressesTx 1] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "savings", "--fee-rate", "10"] [])
          `shouldReturn` Right (Text.pack ("None of the coins chosen can be spent now: " <> txidArg twoAddressesTx <> ":1, you locked it"))

    it "is not what unlockcoin unlocks, which says what does" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        (errorOf <$> hwJson home ["unlockcoin", shortCoinArg twoAddressesTx 1] [])
          `shouldReturn` Right "The coin is locked by a tag of its address, not by lockcoin. hw tags shows the tags that lock their coins: hw tag TAG --unlock-coins unlocks them, and --spend-from-tag TAG spends them"

    it "has a payment that named it signed and sent, with send as with preparetx, signtx and broadcast" $
      forM_ [WithSend, WithThreeCommands] $ \way ->
        withChain (confirmedChain twoAddressesTx) $ \home _ -> do
          _ <- savingsLocksSecond home
          sent <- paidWith way home [fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "savings", "--fee-rate", "10"] []
          spentFromWallet sent `shouldBe` Map.fromList [(externalAddr 1, 30000)]
          pendingState sent `shouldBe` Just (True, True)

    it "still opens for a payment that named it once renametag gives it a new name" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- namingSavings home
        _ <- answer home ["renametag", "savings", "vault"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The payment is sent at 10 satoshis a byte, and bumpfee replaces it
    -- at 20: the replacement spends the same coin of 30000
    it "has the replacement bumpfee prepares of a payment that named it signed" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- namingSavings home
        _ <- answer home ["signtx", nosig] seedOnce
        stuck <- txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []
        replacement <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        signed <- txOf =<< answer home ["signtx", replacement] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- TAG-14 of the usability review: the payment is prepared, and the
    -- coin it spends comes under the lock of a tag afterwards
    it "has a payment prepared before it locked refused by signtx, before the mnemonic, with the coin and the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["tag", secondAddress, "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        run <- hw home ["signtx", nosig] seedOnce
        run `shouldSatisfy` printedSentence lockedBySavings
        run `shouldNotSatisfy` askedMnemonic
        onePendingState home `shouldReturn` Just (False, False)

    it "has a payment signed before it locked refused by broadcast, which sends nothing" $
      withChain (confirmedChain twoAddressesTx) $ \home published -> do
        nosig <- secondSpent home []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["tag", secondAddress, "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        (errorOf <$> hwJson home ["broadcast", nosig, "--yes"] []) `shouldReturn` Right lockedBySavings
        (inMempool <$> readIORef published) `shouldReturn` []

    it "has a payment it stopped signed again once it is told to unlock its coins" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["tag", secondAddress, "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        _ <- answer home ["tag", "savings", "--unlock-coins"] []
        signed <- txOf =<< answer home ["signtx", nosig] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- The wallet no longer holds the payment: its files are as those of
    -- another computer, and name no tag
    it "has the files of a payment the wallet no longer holds refused by signtx and by broadcast" $
      withChain (confirmedChain twoAddressesTx) $ \home published -> do
        nosig <- secondSpent home ["--output", home </> "unsigned.psbt"]
        _ <- answer home ["signtx", nosig, "--output", home </> "signed.psbt"] seedOnce
        _ <- answer home ["deletetx", nosig, "--yes"] []
        _ <- answer home ["tag", secondAddress, "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        run <- hw home ["signtx", "--input", home </> "unsigned.psbt", "--output", home </> "again.psbt"] seedOnce
        run `shouldSatisfy` printedSentence lockedBySavings
        run `shouldNotSatisfy` askedMnemonic
        (errorOf <$> hwJson home ["broadcast", "--input", home </> "signed.psbt", "--yes"] []) `shouldReturn` Right lockedBySavings
        (inMempool <$> readIORef published) `shouldReturn` []

    it "says, when it is told to lock its coins, that a pending payment which spends one can no longer be signed or sent, and locks them" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["tag", secondAddress, "savings"] []
        run <- hw home ["tag", "savings", "--lock-coins"] []
        run `shouldSatisfy` printedSentence (stopped (shortOf nosig))
        (tagLocksOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("savings", True)]

    -- The payment is named by its txid once signed
    it "says of a payment sent already that the lock does not stop it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["signtx", nosig] seedOnce
        txid <- txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []
        _ <- answer home ["tag", secondAddress, "savings"] []
        run <- hw home ["tag", "savings", "--lock-coins"] []
        run
          `shouldSatisfy` printedSentence
            (Text.pack ("The pending transaction " <> shortOf txid <> " spends the coin " <> coin <> ", which is locked now. It was sent already, and the lock does not stop it"))

    -- The lock does not stop the payment, so it does not stop its
    -- replacement at a higher fee, which spends the same coin
    it "has the replacement bumpfee prepares of a payment sent before it locked signed" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["signtx", nosig] seedOnce
        stuck <- txidOf =<< txOf =<< answer home ["broadcast", nosig, "--yes"] []
        _ <- answer home ["tag", secondAddress, "savings"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        replacement <- nosigOf =<< txOf =<< answer home ["bumpfee", stuck, "--fee-rate", "20"] []
        signed <- txOf =<< answer home ["signtx", replacement] seedOnce
        pendingState signed `shouldBe` Just (True, False)

    -- savings locks its coins before the second address has it
    it "says so when tag puts it on an address whose coin a pending payment spends" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        run <- hw home ["tag", secondAddress, "savings"] []
        run `shouldSatisfy` printedSentence (stopped (shortOf nosig))

    -- The second address has kyc, which locks nothing, and savings locks
    -- its coins: kyc merged into savings puts the address under the lock
    it "says so when renametag --merge puts its lock on an address whose coin a pending payment spends" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        nosig <- secondSpent home []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        run <- hw home ["renametag", "kyc", "savings", "--merge"] []
        run `shouldSatisfy` printedSentence (stopped (shortOf nosig))

    -- The file of the wallet is written while savings locks the coin, and
    -- read once savings is deleted and the payment prepared
    it "says so when importwallet brings its lock back on an address whose coin a pending payment spends" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer home ["deletetag", "savings", "--yes"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from", secondAddress, "--fee-rate", "10"] []
        run <- hw home ["importwallet", home </> "wallet.jsonl"] []
        run `shouldSatisfy` printedSentence (stopped (shortOf nosig))

    -- The payment named savings, which is unlocked and locked again
    it "says nothing of a pending payment that named it" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        _ <- namingSavings home
        _ <- answer home ["tag", "savings", "--unlock-coins"] []
        run <- hw home ["tag", "savings", "--lock-coins"] []
        run `shouldNotSatisfy` printedSentence "which is locked now"

    it "is written by exportwallet as locking its coins, and locks them in the wallet importwallet makes" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> withHome $ \fresh -> do
        _ <- savingsLocksSecond home
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        (tagLocksOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right [("savings", True)]

    -- The file was written before the tag was made to lock its coins
    it "is not unlocked by a file of a wallet that does not say it locks" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "savings"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        _ <- answer home ["importwallet", home </> "wallet.jsonl"] []
        (tagLocksOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("savings", True)]

    -- The savings are withdrawn from once: 20000 of the coin of 30000 at
    -- the second address are paid with --tag-change, and the rest, 30000
    -- - 20000 - 2260 of fee = 7740, is at the first change address,
    -- bip44/1/0, under the tag. The second computer has the wallet from
    -- its file alone. It reads the file before its first sync, when it
    -- has not generated the change address.
    it "locks the change of a withdrawal on a second computer, which read the file of the wallet before its first sync" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> withHomeOnStoreOf home $ \second -> do
        withdrawnAndExported home chain
        _ <- answer second ["importwallet", home </> "wallet.jsonl"] []
        _ <- answer second ["sync"] []
        (fmap (map (\(_, value, _, lock, spendable) -> (value, lock, spendable))) . coinsShown <$> hwJson second ["coins", "--locked"] [])
          `shouldReturn` Right [(7740, Just LockedByTag, False)]

    -- 50000 at the first address, which has no tag: the savings stay
    it "keeps that change out of a payment of everything on the second computer" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> withHomeOnStoreOf home $ \second -> do
        withdrawnAndExported home chain
        _ <- answer second ["importwallet", home </> "wallet.jsonl"] []
        _ <- answer second ["sync"] []
        prepared <- txOf =<< answer second ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] []
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000)]

    -- The second address and the change address have the tag
    it "is on a change address from the import on, though the wallet has not generated the address: importwallet counts it, and asks for no second import" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> withHomeOnStoreOf home $ \second -> do
        withdrawnAndExported home chain
        run <- hw second ["importwallet", home </> "wallet.jsonl"] []
        filter (Text.isPrefixOf "Tags set") (Text.lines (runOut run)) `shouldBe` ["Tags set: 2 addresses"]
        run `shouldNotSatisfy` printed "Not in the wallet yet"
        (tagUsesOf <$> hwJson second ["tags"] []) `shouldReturn` Right [("savings", Nothing, 2, 0)]

changeTagSpec :: Spec
changeTagSpec =
  describe "The change of a payment" $ do
    -- The coin at /0/0, the only one, is tagged kyc, and the payment
    -- does not ask for the tags
    it "takes no tag at its new change address without --tag-change" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- The coin of 30000 at /0/1, tagged kyc, pays 20000: the change goes
    -- to the new change address /1/0
    it "takes the tag of the coins --spend-from-tag spends, at its new change address, with --tag-change" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--tag-change", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` ["kyc"]

    -- The portfolio holds one coin, at /0/0. No option names a tag.
    it "takes the tag of the coins the payment selects, without coin control, with --tag-change" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` ["kyc"]

    -- 60000 takes both coins. /0/0 has kyc and cold, /0/1 has kyc alone.
    -- --yes answers the question on coins of different tags.
    it "takes the tags every coin spent has, and not a tag one of them lacks" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc", "cold"] []
        _ <- answer home ["tag", secondAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--tag-change", "--fee-rate", "10", "--yes"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` ["kyc"]

    -- 60000 takes both coins, and the one at /0/1 has no tag. --yes
    -- answers the question on coins of different tags.
    it "takes no tag when one of the coins spent has none" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--tag-change", "--fee-rate", "10", "--yes"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- The user asked for a tagged change and gets none: hw says why
    it "is said to take no tag when the coins spent share none, though --tag-change asked for one" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--tag-change", "--fee-rate", "10", "--yes"] []
        Text.strip (unwrapped (runErr run)) `shouldBe` "--tag-change put no tag on the change address: the coins the payment spends share no tag"

    it "is said nothing of when --tag-change gives it a tag" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        runErr run `shouldBe` ""

    -- 50000 pays 48080 and the fee of 1920 for one input and one output:
    -- nothing is left, so there is no change address to tag
    it "is said nothing of when the payment has no change" $
      withChain paidChain $ \home _ -> do
        synced home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0004808", "--tag-change", "--fee-rate", "10"] []
        runErr run `shouldBe` ""

    -- The change address is of the group from then on: coins --tag lists
    -- the coin of the group, and once the payment is mined, its change of
    -- 30000 - 20000 - 2260 = 7740
    it "is a coin of the group once the payment is mined" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--tag-change", "--fee-rate", "10"] []
        _ <- answer home ["signtx", nosig] seedOnce
        _ <- answer home ["broadcast", nosig, "--yes"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (coinTagsOf <$> hwJson home ["coins", "--tag", "kyc"] []) `shouldReturn` Right [(7740, ["kyc"])]

    -- --change-back sends the change to an address that keeps its tags,
    -- so the two options ask for opposite things
    it "is refused --tag-change with --change-back" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["tag", secondAddress, "kyc"] []
        (errorOf <$> hwJson home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--change-back", firstAddress, "--tag-change", "--fee-rate", "10"] [])
          `shouldReturn` Right "--tag-change tags a new change address, and --change-back sends the change to an address that keeps its tags: give one of them"

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
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 1, 0)]

    -- The first payment tags the change address /1/0 kyc, and is deleted.
    -- The tag is then taken off the coin, so the second payment, which
    -- takes /1/0 again and asks for the tags too, has no tag to give it.
    it "has no tag left of a deleted payment when the next payment takes the same change address" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- A first payment makes the change address /1/0, and is deleted. The
    -- address is free, and is tagged cold by hand. The coin of the second
    -- payment has no tag, so its change, at /1/0 again, has none.
    it "has the tags of its coins and no other with --tag-change, even one put on its address while it was free" $
      withChain paidChain $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        _ <- answer home ["deletetx", nosig, "--yes"] []
        _ <- answer home ["tag", "bip44/1/0", "cold"] []
        prepared <- txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        addressTagsIn prepared (internalAddr 0) `shouldBe` []

    -- The two ways to pay keep the transaction through the same step, so
    -- both tag its change
    forM_ [(WithSend, "send"), (WithThreeCommands, "preparetx, signtx and broadcast")] $ \(way, name) ->
      it ("takes the tag of the coin spent with --tag-change when the payment is made with " <> name) $
        withChain paidChain $ \home _ -> do
          synced home
          _ <- answer home ["tag", firstAddress, "kyc"] []
          sent <- paidWith way home [fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
          addressTagsIn sent (internalAddr 0) `shouldBe` ["kyc"]

    -- No mnemonic is typed, so send keeps nothing of the payment: kyc is
    -- on the address of the coin alone, as before
    it "is not tagged by a send stopped at the mnemonic" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- hwJson home ["send", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        (tagUsesOf <$> hwJson home ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 1, 0)]

    -- The change of 27740 goes to the first change address of the
    -- account, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn, and the line under it
    -- shows its tags, as under the address the payment spends from
    it "shows its tag on the screen, under its address" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        shown <- Text.lines . runOut <$> hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--tag-change", "--fee-rate", "10"] []
        take 2 (dropWhile (not . Text.isPrefixOf "  Change") shown)
          `shouldBe` [ "  Change   : 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  0.00027740 btc  bip44/1/0",
                       "             [kyc]"
                     ]

-- The portfolio holds 50000 at its first receiving address and 30000 at
-- its second one. A payment of 60000 needs both coins: two inputs and two
-- outputs are 374 bytes, a fee of 3740 at 10 satoshis a byte, and
-- 80000 - 60000 - 3740 = 16260 of change.
mixingSpec :: Spec
mixingSpec =
  describe "A payment that spends coins of different tags" $ do
    let chain = confirmedChain twoAddressesTx
        payment60000 = ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"]

    -- The coin of 50000 has kyc, and the coin of 30000 has p2p
    it "is asked about first, with the coins of each tag and what they are worth" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        run <- hw home payment60000 ["n"]
        Text.lines (runErr run)
          `shouldBe` [ "The transaction spends coins that do not all have the same tags:",
                       "  [kyc]  1 coin  0.00050000 btc",
                       "  [p2p]  1 coin  0.00030000 btc",
                       "A transaction shows that its coins have one owner, so it joins these groups.",
                       "Prepare the transaction anyway? [y/N] "
                     ]

    it "is prepared once the user answers yes" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        prepared <- txOf =<< answer home payment60000 ["y"]
        spentFromWallet prepared `shouldBe` Map.fromList [(externalAddr 0, 50000), (externalAddr 1, 30000)]
        paidToWallet prepared `shouldBe` Map.fromList [(internalAddr 0, 16260)]

    it "is not prepared when the user answers no, and hw says which options choose the coins" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        run <- hw home payment60000 ["n"]
        runExit run `shouldBe` ExitFailure 1
        run `shouldSatisfy` printed "The transaction was not prepared: --spend-from-tag, --spend-from and --coin choose the coins it spends"

    it "leaves no pending transaction after a no" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        _ <- hw home payment60000 ["n"]
        (txsOf <$> hwJson home ["pendingtxs"] []) `shouldReturn` Right []

    it "is prepared without a question, with --yes" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        run <- hw home (payment60000 <> ["--yes"]) []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

    -- The coin of 30000 is at an address without a tag
    it "is asked about when one of its coins has no tag, a group named no tag" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] [] home
        run <- hw home payment60000 ["n"]
        take 2 (drop 1 (Text.lines (runErr run)))
          `shouldBe` [ "  [kyc]   1 coin  0.00050000 btc",
                       "  no tag  1 coin  0.00030000 btc"
                     ]

    -- Both coins have kyc, and the coin of 50000 has cold too
    it "is asked about when its coins share a tag and one of them has another" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc", "cold"] ["kyc"] home
        run <- hw home payment60000 ["n"]
        take 2 (drop 1 (Text.lines (runErr run)))
          `shouldBe` [ "  [cold] [kyc]  1 coin  0.00050000 btc",
                       "  [kyc]         1 coin  0.00030000 btc"
                     ]

    it "is not asked about when its coins all have the same tags" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc", "cold"] ["cold", "kyc"] home
        run <- hw home payment60000 []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

    -- --spend-from-tag kyc spends the coin of 50000 alone: 226 bytes, a
    -- fee of 2260, and 50000 - 20000 - 2260 = 27740 of change
    it "is not asked about when coin control keeps it to the coins of one tag" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        run <- hw home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

    -- Both coins have kyc, and the coin of 50000 has cold too. The
    -- payment of 60000 needs both. The user named kyc: the coins of kyc
    -- are the group asked for.
    it "is not asked about when --spend-from-tag names a tag that all its coins have, though one has another tag too" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc", "cold"] ["kyc"] home
        run <- hw home (payment60000 <> ["--spend-from-tag", "kyc"]) []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

    it "is not asked about by preparesendall either, when --spend-from-tag names a tag that all its coins have" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc", "cold"] ["kyc"] home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--spend-from-tag", "kyc", "--fee-rate", "10"] []
        (runExit run, runErr run) `shouldBe` (ExitSuccess, "")

    -- Each coin has one of the two tags named, and lacks the other
    it "is asked about when --spend-from-tag names two tags and its coins do not have both" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        run <- hw home (payment60000 <> ["--spend-from-tag", "kyc", "--spend-from-tag", "p2p"]) ["n"]
        take 2 (drop 1 (Text.lines (runErr run)))
          `shouldBe` [ "  [kyc]  1 coin  0.00050000 btc",
                       "  [p2p]  1 coin  0.00030000 btc"
                     ]

    -- --coin adds the coin of 30000, which has no tag, to the coin of kyc
    it "is asked about when --coin adds a coin without the tag that --spend-from-tag names" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] [] home
        run <- hw home (payment60000 <> ["--spend-from-tag", "kyc", "--coin", shortCoinArg twoAddressesTx 1]) ["n"]
        take 2 (drop 1 (Text.lines (runErr run)))
          `shouldBe` [ "  [kyc]   1 coin  0.00050000 btc",
                       "  no tag  1 coin  0.00030000 btc"
                     ]

    -- preparesendall sends both coins
    it "is asked about by preparesendall as well" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        run <- hw home ["preparesendall", textOf (otherAddr 0), "--fee-rate", "10"] ["n"]
        Text.lines (runErr run)
          `shouldBe` [ "The transaction spends coins that do not all have the same tags:",
                       "  [kyc]  1 coin  0.00050000 btc",
                       "  [p2p]  1 coin  0.00030000 btc",
                       "A transaction shows that its coins have one owner, so it joins these groups.",
                       "Prepare the transaction anyway? [y/N] "
                     ]

    -- The lines under an address the payment spends from show its tags
    it "shows the tags of each address it spends from under the address, once prepared" $
      withChain chain $ \home _ -> do
        syncedTagged ["kyc"] ["p2p"] home
        shown <- Text.lines . runOut <$> hw home (payment60000 <> ["--yes"]) []
        take 4 (dropWhile (not . Text.isPrefixOf "  From") shown)
          `shouldBe` [ "  From     : 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg  0.00030000 btc  bip44/0/1",
                       "             [p2p]",
                       "             1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj  0.00050000 btc  bip44/0/0",
                       "             [kyc]"
                     ]

-- | A home whose coin of 50000 at /0/0 went down a chain of two
-- payments, both mined. The first pays 20000 and leaves 27740 at the
-- change address /1/0, 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn
-- ('paymentSent'). The second spends that change: it pays 10000 at 10
-- satoshis a byte, 2260 for its 226 bytes, and leaves
-- 27740 - 10000 - 2260 = 15480 at /1/1,
-- 1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6. No address has a tag.
twoPaymentsDown :: FilePath -> IORef Chain -> IO ()
twoPaymentsDown home chain = do
  _ <- sentPayment home
  modifyIORef chain mine
  _ <- answer home ["sync"] []
  nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--fee-rate", "10"] []
  _ <- answer home ["signtx", nosig] seedOnce
  _ <- answer home ["broadcast", nosig, "--yes"] []
  modifyIORef chain mine
  void $ answer home ["sync"] []

changeChainSpec :: Spec
changeChainSpec =
  describe "The chain of change of an address" $ do
    -- The coin of 15480 at the end of the chain stays out of the group
    it "takes no tag when tag is given no --with-change" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc"] []
        (coinTagsOf <$> hwJson home ["coins"] []) `shouldReturn` Right [(15480, [])]

    it "keeps its tag when untag is given no --with-change" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        (coinTagsOf <$> hwJson home ["coins"] []) `shouldReturn` Right [(15480, ["kyc"])]

    -- The address has received its coin, and no payment has spent it
    it "is said to be empty on the screen when no change address took the tag" $
      withChain paidChain $ \home _ -> do
        synced home
        shown <- Text.lines . runOut <$> hw home ["tag", firstAddress, "kyc", "--with-change"] []
        dropWhile (/= "") shown
          `shouldBe` ["", "No change address down the chain of its payments was tagged with it"]

    it "is refused for a transaction, which has no chain of change" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", txidArg payment, "salary", "--with-change"] [])
          `shouldReturn` Right "tag takes no --with-change with a transaction: the tags that go down the chain of change are those of an address"

    it "is refused for a contact of the address book" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["newcontact", textOf (otherAddr 0), "Alice", "--tag", "friends"] []
        (errorOf <$> hwJson home ["untag", textOf (otherAddr 0), "friends", "--with-change"] [])
          `shouldReturn` Right "untag takes no --with-change with a contact: its payments are not of the wallet, so hw knows no change of them"

    it "is refused with a tag alone to colour, since nothing is tagged" $
      withChain paidChain $ \home _ -> do
        synced home
        (errorOf <$> hwJson home ["tag", "kyc", "--color", "red", "--with-change"] [])
          `shouldReturn` Right "tag takes no --with-change with a tag alone: nothing is tagged"

    -- The payment was made before the address had a tag, so its change
    -- at /1/0 has none
    it "takes the tag put on the address a payment spent from, at the change of that payment" $
      withChain paidChain $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"])]

    it "takes it at the change of the payment that spent that change too" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"]), ("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", ["kyc"])]

    -- The portfolio holds one coin, the 15480 at the end of the chain
    it "makes the coin at its end a coin of the group" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        (coinTagsOf <$> hwJson home ["coins", "--tag", "kyc"] []) `shouldReturn` Right [(15480, ["kyc"])]

    it "goes through a payment that waits to be signed" $
      withChain paidChain $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0002", "--fee-rate", "10"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"])]

    -- 60000 takes the coin at /0/0 and the one at /0/1, which has no tag
    it "ends at a payment that also spends a coin without the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] []) `shouldReturn` Right []

    -- The same payment: its change at /1/0 is where the tag stops, and
    -- /0/1 is the address to tag for it to go further
    it "names the change address where it ends, with the payment and the address whose coin has no tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        (changeStopsOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"], nosig, Just "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg")]

    it "names it on the screen, below the change addresses that took the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        nosig <- nosigOf =<< txOf =<< answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        shown <- Text.lines . runOut <$> hw home ["tag", firstAddress, "kyc", "--with-change"] []
        dropWhile (/= "") shown
          `shouldBe` [ "",
                       "No change address down the chain of its payments was tagged with it",
                       "",
                       "Not tagged, where the chain stops: 1 change address",
                       "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn",
                       "  unused",
                       "  The payment " <> Text.pack (shortOf nosig) <> " also spends a coin at bip44/0/1, without the tag kyc"
                     ]

    -- The same chain as below a change address that could not take the
    -- tag: /1/0 is the stop, and /1/1 and /1/2 below it are not, since no
    -- coin of the group reached them as far as the tags tell
    it "names the first change address that could not take the tag, and none below it" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> do
        synced home
        mixed <- txidOf =<< paidWith WithThreeCommands home [fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        _ <- paidWith WithThreeCommands home [fixedTo (otherAddr 1) "0.00005", "--fee-rate", "10"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        (changeStopsOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"], mixed, Just "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg")]

    it "names no stop when every change address below took the tag" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        (changeStopsOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] []) `shouldReturn` Right []

    -- The same payment, once the second of its two coins is tagged too
    it "goes on through that payment once every coin it spends has the tag" $
      withChain (confirmedChain twoAddressesTx) $ \home _ -> do
        synced home
        _ <- answer home ["preparetx", fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        (changeTaggedOf <$> hwJson home ["tag", secondAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"])]

    it "starts at a change address that is tagged, and goes down from it" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        (changeTaggedOf <$> hwJson home ["tag", "bip44/1/0", "kyc", "--with-change"] [])
          `shouldReturn` Right [("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", ["kyc"])]

    -- /1/0 holds what was left of the coin at /0/0
    it "never goes up, to the address the coins came from" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", "bip44/1/0", "kyc", "--with-change"] []
        (addressTagsOf <$> hwJson home ["addresses"] [])
          `shouldReturn` Right [("1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", [])]

    -- /1/0 is tagged by hand, and /1/1 below it is not: a chain tagged
    -- in part. Only /1/1 takes the tag, so the answer lists it alone.
    it "goes on below a change address that has the tag already" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", "bip44/1/0", "kyc"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", ["kyc"])]

    -- The whole chain is tagged, then the tag is taken off /1/1 by hand
    it "takes back a tag taken off by hand below, when the address is tagged again" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        _ <- answer home ["untag", "bip44/1/1", "kyc"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", ["kyc"])]

    -- The address has coins of 50000 and 30000. A payment of 20000 spends
    -- the first and leaves 50000 - 20000 - 2260 = 27740 at /1/0. A payment
    -- of 10000 spends the second and leaves 30000 - 10000 - 2260 = 17740
    -- at /1/1. A third spends both of those: it pays 40000 and 3740 for
    -- its 374 bytes, and leaves 27740 + 17740 - 40000 - 3740 = 1740 at
    -- /1/2, 1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr. Every coin the third spends
    -- came from the address, by two ways.
    it "takes the tag at the change of a payment that spends two change addresses of the chain" $
      withChain (confirmedChain twoCoinsTx) $ \home chain -> do
        synced home
        _ <- paidWith WithThreeCommands home [fixedTo (otherAddr 0) "0.0002", "--coin", coinArg twoCoinsTx 0, "--fee-rate", "10"] []
        _ <- paidWith WithThreeCommands home [fixedTo (otherAddr 1) "0.0001", "--coin", coinArg twoCoinsTx 1, "--fee-rate", "10"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 2) "0.0004", "--fee-rate", "10"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right
            [ ("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"]),
              ("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", ["kyc"]),
              ("1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr", ["kyc"])
            ]

    -- The coins of 50000 at /0/0 and of 30000 at /0/1 pay 60000 and 3740
    -- for 374 bytes, and leave 80000 - 60000 - 3740 = 16260 at /1/0. That
    -- change cannot take the tag of /0/0: /0/1 has none. A payment of
    -- 5000 spends it and leaves 16260 - 5000 - 2260 = 9000 at /1/1, which
    -- is tagged by hand. A payment of 3000 spends that and leaves
    -- 9000 - 3000 - 2260 = 3740 at /1/2, 1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr,
    -- whose one coin spent has the tag.
    it "takes the tag where every coin spent has it, below a change address that could not take it" $
      withChain (confirmedChain twoAddressesTx) $ \home chain -> do
        synced home
        _ <- paidWith WithThreeCommands home [fixedTo (otherAddr 0) "0.0006", "--fee-rate", "10"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        _ <- paidWith WithThreeCommands home [fixedTo (otherAddr 1) "0.00005", "--fee-rate", "10"] []
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 2) "0.00003", "--fee-rate", "10"] []
        _ <- answer home ["tag", "bip44/1/1", "kyc"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("1MZNPWwFwy2CqVgWBq6unPWBWrZTQ7WTnr", ["kyc"])]

    -- The change of 27740 at /1/0 is spent by a payment that sends its
    -- change back to /1/0. The address is below itself, and one of its
    -- coins came from itself: that coin does not keep the tag from it.
    it "takes the tag at a change address that got its own change back" $
      withChain paidChain $ \home chain -> do
        _ <- sentPayment home
        modifyIORef chain mine
        _ <- answer home ["sync"] []
        _ <- answer home ["preparetx", fixedTo (otherAddr 1) "0.0001", "--change-back", "largest", "--fee-rate", "10"] []
        (changeTaggedOf <$> hwJson home ["tag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["kyc"])]

    it "loses the tag untag takes off the address, all the way down" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        (changeUntaggedOf <$> hwJson home ["untag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", []), ("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", [])]

    -- The tag is taken off /1/0 by hand, and /1/1 below it still has it
    it "loses the tag below a change address that does not have it" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        _ <- answer home ["untag", "bip44/1/0", "kyc"] []
        (changeUntaggedOf <$> hwJson home ["untag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", [])]

    -- The option was forgotten the first time: the address has lost the
    -- tag, and the change addresses below have it still
    it "loses the tag with untag --with-change though the address lost it before" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        (changeUntaggedOf <$> hwJson home ["untag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", []), ("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", [])]

    it "is said then to have lost a tag that the address did not have" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "--with-change"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        run <- hw home ["untag", firstAddress, "kyc", "--with-change"] []
        run `shouldSatisfy` printedSentence "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj does not have the tag kyc: it came off the change addresses alone"

    -- kyc is a tag, and nothing below the address has it
    it "has untag --with-change refused when neither the address nor a change address below has the tag" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", "kyc", "--color", "red"] []
        (errorOf <$> hwJson home ["untag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right "The address 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj does not have the tag kyc, and no change address down the chain of its payments has it"

    it "keeps its other tags when untag takes one off" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        _ <- answer home ["tag", firstAddress, "kyc", "cold", "--with-change"] []
        (changeUntaggedOf <$> hwJson home ["untag", firstAddress, "kyc", "--with-change"] [])
          `shouldReturn` Right [("17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", ["cold"]), ("1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6", ["cold"])]

    -- Both change addresses have received their coin, and the one at
    -- /1/0 is spent
    it "is listed on the screen under the address, each change address with what it can spend" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        shown <- Text.lines . runOut <$> hw home ["tag", firstAddress, "kyc", "--with-change"] []
        dropWhile (/= "") shown
          `shouldBe` [ "",
                       "Tagged with it, down the chain of its payments: 2 change addresses",
                       "bip44/1/0  17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn  [kyc]",
                       "  spendable 0.00000000 btc, received 0.00027740 btc, 2 transactions",
                       "bip44/1/1  1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6  [kyc]",
                       "  spendable 0.00015480 btc, received 0.00015480 btc, 1 transaction"
                     ]

    it "is named under changetagged in the JSON of tag" $
      withChain paidChain $ \home chain -> do
        twoPaymentsDown home chain
        tagged <- hwJsonValue home ["tag", firstAddress, "kyc", "--with-change"] []
        Json.parseEither (Json.withObject "answer" (\o -> mapM (Json..: "address") =<< o Json..: "changetagged")) tagged
          `shouldBe` Right ["17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", "1NqNFsuS7K3dfF8RnAVr9YYCMvJuF9GCn6" :: Text]

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

    -- The tag is made to lock its coins before any address has it, as
    -- the README of tag invites to
    it "bring back a tag that nothing has yet, with its colour and its lock" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", "savings", "--lock-coins", "--color", "blue"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        (tagUsesOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right [("savings", Just "blue", 0, 0)]
        (tagLocksOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right [("savings", True)]

    -- kyc was on the first address, and has no colour and no lock
    it "bring back a tag that nothing has any more, without a colour and without a lock" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["untag", firstAddress, "kyc"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        (tagUsesOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 0, 0)]

    it "are shown by show before the file is imported, those that nothing has under a heading of their own" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", "savings", "--lock-coins", "--color", "blue"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        shown <- Text.lines . runOut <$> hw fresh ["show", home </> "wallet.jsonl"] []
        take 2 (dropWhile (/= "Tags not used") shown)
          `shouldBe` [ "Tags not used",
                       "  savings  blue  locks its coins"
                     ]

    -- A file of one portfolio is made to show less than the database: a
    -- tag that nothing has is of no portfolio
    it "leave a tag that nothing has out of a file of one portfolio" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        _ <- answer home ["exportwallet", "--portfolio", "0", home </> "portfolio.jsonl"] []
        _ <- answer fresh ["importwallet", home </> "portfolio.jsonl"] []
        (tagUsesOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right []

    -- kyc is on the first address, and savings on nothing
    it "name a tag that nothing has on no address and no transaction" $
      withChain paidChain $ \home _ -> withHome $ \fresh -> do
        synced home
        _ <- answer home ["tag", firstAddress, "kyc"] []
        _ <- answer home ["tag", "savings", "--lock-coins"] []
        _ <- answer home ["exportwallet", home </> "wallet.jsonl"] []
        _ <- answer fresh ["importwallet", home </> "wallet.jsonl"] []
        (tagUsesOf <$> hwJson fresh ["tags"] []) `shouldReturn` Right [("kyc", Nothing, 1, 0), ("savings", Nothing, 0, 0)]

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
