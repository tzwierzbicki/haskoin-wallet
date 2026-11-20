{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The tags of the database: the tags themselves and their colours, the
-- tags that lock their coins, the tags of addresses and of transactions,
-- the listings that keep the rows of some tags, and the ids of the
-- transactions a filter keeps. The pool is what portfolio 0 of the test wallet
-- holds on Bitcoin. The story of the listings is two payments received
-- by its bip44 account: 50000 at its first receiving address, in block
-- 800000, then 30000 at its second one, in block 800001.
module Haskoin.Wallet.Database.TagSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Default (def)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (deletePortfolio)
import Haskoin.Wallet.Database.Address (AddrType (..), CoinBalance (..), genExtAddress, keptAddressesAfter, nextChangeAddress, taggedAddressesAfter)
import Haskoin.Wallet.Database.Coin (CoinFilter (..), CoinLock (..), JsonCoin (..), coinBalancesAfter, listedCoinsAfter, lockCoin, refreshCoins, spendableCoins, spendableCoinsOpening, unlockCoin, unspendableCoins, walletCoin)
import Haskoin.Wallet.Contacts (Contact (..))
import Haskoin.Wallet.Database.Contact (addContact, contactAt, tagContact)
import Haskoin.Wallet.Database.Portfolio (Portfolio (..), portfolioPool)
import Haskoin.Wallet.Database.Tag
import Haskoin.Wallet.Database.TxInfo
import Haskoin.Wallet.Keys (legacyPath)
import Haskoin.Wallet.Tags
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Test.Hspec

spec :: Spec
spec = do
  tagsSpec
  lockingTagSpec
  addressTagsSpec
  txTagsSpec
  otherAddressSpec
  addressListingSpec
  untaggedAddressListingSpec
  coinListingSpec
  -- txs finds the transactions of a filter by their ids when they are
  -- few, and by a walk of the history when they are many. Both ways must
  -- list the same rows, so every rule is checked both ways: up to 2000
  -- transactions looked up, as hw does, and none, which always walks.
  txListingSpec "found by their ids" 2000
  txListingSpec "found by a walk of the history" 0
  keptIdsSpec

{- The tags of these tests are those of TestUtils: kyc, cold and rent -}

-- | The words of tags as an answer shows them
namesOf :: [ShownTag] -> [Text]
namesOf = map (tagText . shownTag)

-- | The first receiving addresses of the bip44 account of the pool, as
-- many as asked, generated in the database: /0/0, /0/1 and so on
receivingAddresses :: Int -> ExceptT String (DB IO) (DBAccountId, [DBAddress])
receivingAddresses count = do
  (accId, _) <- testNewAcc
  rows <- mapM (const (genExtAddress (def :: Config) accId "")) [1 .. count]
  return (accId, rows)

{- The payments of the story -}

-- | The coins of the story in an account: 50000 at /0/0, of transaction
-- 1, and 30000 at /0/1, of transaction 2
twoCoins :: DBAccountId -> ExceptT String (DB IO) ()
twoCoins accId =
  void $
    refreshCoins
      btc
      accId
      (const seenTime)
      [externalAddr 0, externalAddr 1]
      [storeCoin (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000, storeCoin (fakeTxHash 2, 0) (Just 800001) (externalAddr 1) 30000]

-- | The coin of 50000 at /0/0, and the coin of 30000 at /0/1
fiftyCoin, thirtyCoin :: OutPoint
fiftyCoin = OutPoint (fakeTxHash 1) 0
thirtyCoin = OutPoint (fakeTxHash 2) 0

-- | The second payment as the portfolio sees it: 30000 received at its
-- second receiving address, /0/1
secondPaymentSummary :: TxSummary
secondPaymentSummary =
  paymentSummary
    { summaryAmount = 30000,
      summaryMyOutputs = Map.fromList [(externalAddr 1, MyOutputs 30000 (legacyPath (Deriv :/ 0 :/ 1)) "")]
    }

-- | The two payments of the story in the history of the pool, and what
-- the account holds after them: 50000 + 30000 = 80000 confirmed
twoPayments :: ExceptT String (DB IO) (Pool, DBAccountId, [DBAddress])
twoPayments = do
  pool <- testPool
  (accId, rows) <- receivingAddresses 2
  lift $ do
    void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
    void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime secondPaymentSummary
    P.update accId [DBAccountBalanceConfirmed P.=. 80000]
  return (pool, accId, rows)

-- | The two payments of the story, the second one paid from another
-- wallet than the first: the first from
-- 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, as the shared story has it, and
-- the second from 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ. The tests of the
-- contacts tell the two payers apart.
paidFromTwo :: ExceptT String (DB IO) Pool
paidFromTwo = do
  pool <- testPool
  (accId, _) <- receivingAddresses 2
  lift $ do
    void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
    void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime secondPaymentSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 31000)]}
    P.update accId [DBAccountBalanceConfirmed P.=. 80000]
  return pool

-- | The filter of txs --untagged: the transactions left to sort, and no
-- tag
leftToSort :: TxTagFilter
leftToSort = TxTagFilter [] [] True

-- | The id and the balance of each transaction txs lists with a filter of
-- tags, in the order given, looking up at most that many transactions by
-- their ids
keptIn :: Natural -> ListingOrder -> Pool -> TxTagFilter -> Maybe TxsCursor -> ExceptT String (DB IO) [(TxHash, Maybe Integer)]
keptIn lookedUp order pool kept afterM = do
  rows <- keptTxsAfterBy lookedUp order pool kept afterM Nothing
  return [(chainTxid (chainOf (listedTxInfo listed)), listedTxBalance listed) | (listed, _) <- rows]

tagsSpec :: Spec
tagsSpec =
  describe "A tag of the database" $ do
    it "exists from the first time it is put on something, without a colour" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ allTags `dbShouldBe` [ShownTag kycTag Nothing]

    it "has the colour the user gave it last" $
      inDatabaseE $ do
        lift $ setTagColor kycTag (Just TagRed)
        lift $ setTagColor kycTag (Just TagBlue)
        lift $ heldTag kycTag `dbShouldBe` Just (ShownTag kycTag (Just TagBlue))

    it "can be coloured before anything has it" $
      inDatabaseE $ do
        lift $ setTagColor coldTag (Just TagCyan)
        lift $ allTags `dbShouldBe` [ShownTag coldTag (Just TagCyan)]

    it "loses its colour when the user removes it, and stays a tag" $
      inDatabaseE $ do
        lift $ setTagColor kycTag (Just TagRed)
        lift $ setTagColor kycTag Nothing
        lift $ allTags `dbShouldBe` [ShownTag kycTag Nothing]

    it "is listed with the others in the order of the alphabet" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [rentTag, coldTag, kycTag]
        lift $ (namesOf <$> allTags) `dbShouldBe` ["cold", "kyc", "rent"]

    it "stays, with its colour, when nothing has it any more" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ setTagColor kycTag (Just TagRed)
        _ <- lift $ untagAddress addr [kycTag]
        lift $ allTags `dbShouldBe` [ShownTag kycTag (Just TagRed)]

    -- kyc is on two addresses and one transaction, cold on one address
    it "is counted on the addresses and the transactions that have it" $
      inDatabaseE $ do
        pool <- testPool
        (_, [first, second]) <- receivingAddresses 2
        lift $ tagAddress first [kycTag, coldTag]
        lift $ tagAddress second [kycTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag]
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag coldTag Nothing) 1 0 0 False, TagUse (ShownTag kycTag Nothing) 2 1 0 False]

    -- A payment from portfolio 0 to portfolio 1 is a transaction of both,
    -- and each has rent on it. Portfolio 0 has rent on a second
    -- transaction as well: 2 transactions have the tag, not 3.
    it "is counted once on a transaction that two portfolios hold" $
      inDatabaseE $ do
        payer <- testPool
        paid <- (`portfolioPool` btc) <$> nextPortfolioOf testMnemonic
        lift $ tagTx payer (fakeTxHash 1) [rentTag]
        lift $ tagTx paid (fakeTxHash 1) [rentTag]
        lift $ tagTx payer (fakeTxHash 2) [rentTag]
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag rentTag Nothing) 0 2 0 False]

    -- A payment made before Bitcoin Cash left Bitcoin is on both chains
    -- under one id. To hw these are two transactions, as one key on two
    -- networks is two addresses.
    it "is counted on a transaction of each network, when two networks have one of the same id" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        lift $ tagTx (portfolioPool portfolio btc) (fakeTxHash 1) [rentTag]
        lift $ tagTx (portfolioPool portfolio bch) (fakeTxHash 1) [rentTag]
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag rentTag Nothing) 0 2 0 False]

    it "is deleted from every address and transaction that has it, and says how many had it" $
      inDatabaseE $ do
        pool <- testPool
        (_, [first, second]) <- receivingAddresses 2
        lift $ tagAddress first [kycTag, coldTag]
        lift $ tagAddress second [kycTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag]
        lift $ deleteTag kycTag `dbShouldBe` TagDeleted 2 1 0
        lift $ (namesOf <$> allTags) `dbShouldBe` ["cold"]
        lift $ (namesOf <$> addressTags first) `dbShouldBe` ["cold"]
        lift $ txTags pool (fakeTxHash 1) `dbShouldBe` []

    -- A payment from portfolio 0 to portfolio 1 has rent in both: one
    -- transaction had the tag, and neither portfolio has it left
    it "says that one transaction had it, when it is deleted from a transaction that two portfolios hold" $
      inDatabaseE $ do
        payer <- testPool
        paid <- (`portfolioPool` btc) <$> nextPortfolioOf testMnemonic
        lift $ tagTx payer (fakeTxHash 1) [rentTag]
        lift $ tagTx paid (fakeTxHash 1) [rentTag]
        lift $ deleteTag rentTag `dbShouldBe` TagDeleted 0 1 0
        lift $ txTags payer (fakeTxHash 1) `dbShouldBe` []
        lift $ txTags paid (fakeTxHash 1) `dbShouldBe` []

    -- kyc is on two addresses and one transaction, cold on one address.
    -- kyc becomes rent.
    it "is renamed on every address and transaction that has it, and its old name is no tag any more" $
      inDatabaseE $ do
        pool <- testPool
        (_, [first, second]) <- receivingAddresses 2
        lift $ tagAddress first [kycTag, coldTag]
        lift $ tagAddress second [kycTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag]
        lift $ renameTag kycTag rentTag
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag coldTag Nothing) 1 0 0 False, TagUse (ShownTag rentTag Nothing) 2 1 0 False]
        lift $ (namesOf <$> addressTags first) `dbShouldBe` ["cold", "rent"]
        lift $ (namesOf <$> txTags pool (fakeTxHash 1)) `dbShouldBe` ["rent"]

    it "keeps its colour when it is renamed" $
      inDatabaseE $ do
        lift $ setTagColor kycTag (Just TagRed)
        lift $ renameTag kycTag rentTag
        lift $ allTags `dbShouldBe` [ShownTag rentTag (Just TagRed)]

    -- The first address and the transaction have both tags, the second
    -- address has kyc alone. kyc is merged into cold: 2 addresses and 1
    -- transaction have cold, each once.
    it "is merged into another tag, which is then once on everything that had either" $
      inDatabaseE $ do
        pool <- testPool
        (_, [first, second]) <- receivingAddresses 2
        lift $ tagAddress first [kycTag, coldTag]
        lift $ tagAddress second [kycTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag, coldTag]
        lift $ renameTag kycTag coldTag
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag coldTag Nothing) 2 1 0 False]
        lift $ (namesOf <$> addressTags first) `dbShouldBe` ["cold"]
        lift $ (namesOf <$> addressTags second) `dbShouldBe` ["cold"]

    it "leaves the tag it is merged into the colour that tag has" $
      inDatabaseE $ do
        lift $ setTagColor kycTag (Just TagRed)
        lift $ setTagColor coldTag (Just TagBlue)
        lift $ renameTag kycTag coldTag
        lift $ allTags `dbShouldBe` [ShownTag coldTag (Just TagBlue)]

    it "gives its colour to the tag it is merged into, when that tag has none" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [coldTag]
        lift $ setTagColor kycTag (Just TagRed)
        lift $ renameTag kycTag coldTag
        lift $ allTags `dbShouldBe` [ShownTag coldTag (Just TagRed)]

    it "must exist for a command that filters by it or spends from it" $
      inDatabaseE $ do
        lift $ dbShouldFailWith "There is no tag kyc. hw tags lists the tags." (requireTags [kycTag])

    it "is offered to the shell when it starts with the word typed" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag, coldTag, forceRight (readTag "kitchen")]
        lift $ tagsStarting "k" 21 `dbShouldBe` ["kitchen", "kyc"]

{- A tag that locks its coins -}

-- In the tests of the coins, /0/0 holds 50000 and has the tag kyc, which
-- locks its coins, and /0/1 holds 30000 and has no tag.

-- | The pool and the account of the story with its two coins and its
-- two receiving addresses, the first one tagged kyc, and kyc locking its
-- coins
kycLocksFirst :: ExceptT String (DB IO) (Pool, DBAccountId, [DBAddress])
kycLocksFirst = do
  pool <- testPool
  (accId, rows) <- receivingAddresses 2
  twoCoins accId
  lift $ tagAddress (rows !! 0) [kycTag]
  lift $ setTagLocksCoins kycTag True
  return (pool, accId, rows)

lockingTagSpec :: Spec
lockingTagSpec =
  describe "A tag that locks its coins" $ do
    it "locks none until it is told to" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ lockingTags `dbShouldBe` []

    it "is among the tags that lock their coins once it is told to" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag, coldTag]
        lift $ setTagLocksCoins kycTag True
        lift $ lockingTags `dbShouldBe` [kycTag]

    it "can lock its coins before anything has it, and is then a tag of the database" $
      inDatabaseE $ do
        lift $ setTagLocksCoins kycTag True
        lift $ (namesOf <$> allTags) `dbShouldBe` ["kyc"]
        lift $ lockingTags `dbShouldBe` [kycTag]

    it "stops locking its coins when it is told to" $
      inDatabaseE $ do
        lift $ setTagLocksCoins kycTag True
        lift $ setTagLocksCoins kycTag False
        lift $ lockingTags `dbShouldBe` []

    -- kyc is on one address and locks, cold is on one address and does not
    it "says among its uses whether it locks its coins" $
      inDatabaseE $ do
        (_, [first, second]) <- receivingAddresses 2
        lift $ tagAddress first [kycTag]
        lift $ tagAddress second [coldTag]
        lift $ setTagLocksCoins kycTag True
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag coldTag Nothing) 1 0 0 False, TagUse (ShownTag kycTag Nothing) 1 0 0 True]

    it "keeps its colour when it is made to lock its coins" $
      inDatabaseE $ do
        lift $ setTagColor kycTag (Just TagRed)
        lift $ setTagLocksCoins kycTag True
        lift $ allTags `dbShouldBe` [ShownTag kycTag (Just TagRed)]

    it "still locks its coins under a new name" $
      inDatabaseE $ do
        lift $ setTagLocksCoins kycTag True
        lift $ renameTag kycTag rentTag
        lift $ lockingTags `dbShouldBe` [rentTag]

    it "makes the tag it is merged into lock its coins" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [coldTag]
        lift $ setTagLocksCoins kycTag True
        lift $ renameTag kycTag coldTag
        lift $ lockingTags `dbShouldBe` [coldTag]

    it "does not unlock the tag it is merged into" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ setTagLocksCoins coldTag True
        lift $ renameTag kycTag coldTag
        lift $ lockingTags `dbShouldBe` [coldTag]

    it "no longer locks once it is deleted, nor when it is made again" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ setTagLocksCoins kycTag True
        _ <- lift $ deleteTag kycTag
        lift $ tagAddress addr [kycTag]
        lift $ lockingTags `dbShouldBe` []

    it "keeps the coins at its addresses from the coins a payment may spend" $
      inDatabaseE $ do
        (pool, _, _) <- kycLocksFirst
        (map (.outpoint) <$> spendableCoins pool 1) `dbShouldBeE` [thirtyCoin]

    it "holds them back with the lock of a tag" $
      inDatabaseE $ do
        (pool, _, _) <- kycLocksFirst
        (map (\(coin, lock) -> (coin.outpoint, lock)) <$> unspendableCoins pool 1) `dbShouldBeE` [(fiftyCoin, Just LockedByTag)]

    it "gives them to a payment that names it" $
      inDatabaseE $ do
        (pool, _, _) <- kycLocksFirst
        (sort . map (.outpoint) <$> spendableCoinsOpening [kycTag] pool 1) `dbShouldBeE` [fiftyCoin, thirtyCoin]

    -- /0/0 has kyc, which locks, and cold, which does not
    it "keeps them from a payment that names another tag of their address" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- kycLocksFirst
        lift $ tagAddress first [coldTag]
        (map (.outpoint) <$> spendableCoinsOpening [coldTag] pool 1) `dbShouldBeE` [thirtyCoin]

    -- /0/0 has kyc and cold, and both lock
    it "keeps a coin that two tags lock from a payment that names one of them" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- kycLocksFirst
        lift $ tagAddress first [coldTag]
        lift $ setTagLocksCoins coldTag True
        (map (.outpoint) <$> spendableCoinsOpening [kycTag] pool 1) `dbShouldBeE` [thirtyCoin]

    it "gives a coin that two tags lock to a payment that names both" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- kycLocksFirst
        lift $ tagAddress first [coldTag]
        lift $ setTagLocksCoins coldTag True
        (sort . map (.outpoint) <$> spendableCoinsOpening [kycTag, coldTag] pool 1) `dbShouldBeE` [fiftyCoin, thirtyCoin]

    -- The tag locks and is on /0/0 before any coin is there
    it "locks a coin that arrives after it was told to lock" $
      inDatabaseE $ do
        pool <- testPool
        (accId, [first, _]) <- receivingAddresses 2
        lift $ setTagLocksCoins kycTag True
        lift $ tagAddress first [kycTag]
        twoCoins accId
        (map (.outpoint) <$> spendableCoins pool 1) `dbShouldBeE` [thirtyCoin]

    it "locks the coins of an address it is put on afterwards" $
      inDatabaseE $ do
        (pool, _, [_, second]) <- kycLocksFirst
        lift $ tagAddress second [kycTag]
        (map (.outpoint) <$> spendableCoins pool 1) `dbShouldBeE` []

    it "frees its coins when it stops locking" $
      inDatabaseE $ do
        (pool, _, _) <- kycLocksFirst
        lift $ setTagLocksCoins kycTag False
        (sort . map (.outpoint) <$> spendableCoins pool 1) `dbShouldBeE` [fiftyCoin, thirtyCoin]

    it "frees the coins of an address that loses it" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- kycLocksFirst
        _ <- lift $ untagAddress first [kycTag]
        (sort . map (.outpoint) <$> spendableCoins pool 1) `dbShouldBeE` [fiftyCoin, thirtyCoin]

    -- coins lists the newest first: the 30000 of block 800001, then the
    -- 50000 of block 800000
    it "has its coins shown by coins as locked by a tag, and not spendable" $
      inDatabaseE $ do
        (pool, accId, _) <- kycLocksFirst
        (map (\(coin, _) -> (jsonCoinOutpoint coin, jsonCoinLock coin, jsonCoinSpendable coin)) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins noTagFilter Nothing Nothing)
          `dbShouldBeE` [(thirtyCoin, Nothing, True), (fiftyCoin, Just LockedByTag, False)]

    it "has its coins among the locked coins that coins lists" $
      inDatabaseE $ do
        (pool, accId, _) <- kycLocksFirst
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter UsualOrder pool [accId] LockedCoins noTagFilter Nothing Nothing)
          `dbShouldBeE` [fiftyCoin]

    -- /0/0: 50000 in all, none spendable, 50000 locked by a tag.
    -- /0/1: 30000 in all, all of it spendable.
    it "has the value of its coins counted apart by balances, as locked by a tag" $
      inDatabaseE $ do
        (_, accId, _) <- kycLocksFirst
        (map (\(addr, balance) -> (dBAddressIndex addr, balance)) <$> coinBalancesAfter UsualOrder btc [accId] noTagFilter Nothing Nothing)
          `dbShouldBeE` [(0, CoinBalance 50000 0 0 0 50000), (1, CoinBalance 30000 30000 0 0 0)]

    it "leaves a coin the user locks as well to lockcoin, which a payment that names the tag does not open" $
      inDatabaseE $ do
        (pool, _, _) <- kycLocksFirst
        _ <- lockCoin =<< walletCoin def Nothing fiftyCoin
        (map (.outpoint) <$> spendableCoinsOpening [kycTag] pool 1) `dbShouldBeE` [thirtyCoin]
        (map (\(coin, lock) -> (coin.outpoint, lock)) <$> unspendableCoins pool 1) `dbShouldBeE` [(fiftyCoin, Just LockedByUser)]

    it "is not unlocked by unlockcoin, which says what does" $
      inDatabaseE $ do
        _ <- kycLocksFirst
        lift $
          dbShouldFailWith "The coin is locked by a tag of its address, not by lockcoin. hw tags shows the tags that lock their coins: hw tag TAG --unlock-coins unlocks them, and --spend-from-tag TAG spends them" $
            unlockCoin =<< walletCoin def Nothing fiftyCoin

addressTagsSpec :: Spec
addressTagsSpec =
  describe "The tags of an address" $ do
    it "are those the user put on it, in the order of the alphabet" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag, coldTag]
        lift $ (namesOf <$> addressTags addr) `dbShouldBe` ["cold", "kyc"]

    it "come with the colour each tag has now" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ setTagColor kycTag (Just TagRed)
        lift $ addressTags addr `dbShouldBe` [ShownTag kycTag (Just TagRed)]

    it "hold a tag once, however many times it is put on" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ tagAddress addr [kycTag]
        lift $ (namesOf <$> addressTags addr) `dbShouldBe` ["kyc"]

    it "are each address's own" $
      inDatabaseE $ do
        (_, [first, second]) <- receivingAddresses 2
        lift $ tagAddress first [kycTag]
        lift $ (namesOf <$> addressTags second) `dbShouldBe` []

    it "lose the tags taken off, and keep the others" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag, coldTag]
        lift $ untagAddress addr [kycTag] `dbShouldBe` []
        lift $ (namesOf <$> addressTags addr) `dbShouldBe` ["cold"]

    it "name the tags to take off that the address did not have" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ untagAddress addr [kycTag, coldTag] `dbShouldBe` [coldTag]

    -- A change address takes no label, and takes tags
    it "go on a change address too" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc
        change <- nextChangeAddress (def :: Config) accId []
        lift $ tagAddress change [kycTag]
        lift $ (namesOf <$> addressTags change) `dbShouldBe` ["kyc"]

    it "go with the portfolio that is deleted, while the tag stays" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        (_, [addr]) <- receivingAddresses 1
        lift $ tagAddress addr [kycTag]
        lift $ tagTx (portfolioPool portfolio btc) (fakeTxHash 1) [kycTag]
        lift $ deletePortfolio portfolio.portfolioId
        lift $ tableRows "d_b_address_tag" `dbShouldBe` []
        lift $ tableRows "d_b_tx_tag" `dbShouldBe` []
        lift $ (namesOf <$> allTags) `dbShouldBe` ["kyc"]

-- | The addresses of others the rows of a transaction of the pool keep,
-- in the order of the alphabet
otherAddressesOf :: Pool -> TxHash -> DB IO [Text]
otherAddressesOf pool hash = do
  rows <- P.selectList [DBTxOtherAddressPortfolio P.==. pool.poolPortfolio, DBTxOtherAddressTxid P.==. hashBytes hash] [P.Asc DBTxOtherAddressAddress]
  return (dBTxOtherAddressAddress . P.entityVal <$> rows)

otherAddressSpec :: Spec
otherAddressSpec =
  describe "The addresses of others of a transaction, as txs finds a tagged contact at them" $ do
    -- The spending pays 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "are those a payment pays" $
      inDatabaseE $ do
        pool <- testPool
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary
        lift $ otherAddressesOf pool (fakeTxHash 2) `dbShouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The payment received was paid from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "are those a payment received was paid from" $
      inDatabaseE $ do
        pool <- testPool
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        lift $ otherAddressesOf pool (fakeTxHash 1) `dbShouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The payer also paid 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the change
    -- of the payer, or somebody the payer paid
    it "leave out what the payer of a payment received paid to others" $
      inDatabaseE $ do
        pool <- testPool
        let alsoPaysOthers = paymentSummary {summaryOtherOutputs = Map.fromList [(otherAddr 1, 70000)]}
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime alsoPaysOthers
        lift $ otherAddressesOf pool (fakeTxHash 1) `dbShouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The summary is written again, paying another address
    it "are those of the summary written last" $
      inDatabaseE $ do
        pool <- testPool
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 1, 40000)]}
        lift $ otherAddressesOf pool (fakeTxHash 2) `dbShouldBe` ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]

    it "go with the portfolio that is deleted, while the address book stays" $
      inDatabaseE $ do
        portfolio <- testPortfolio
        let pool = portfolioPool portfolio btc
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary
        addContact btc (otherAddr 0) "Landlord"
        lift $ deletePortfolio portfolio.portfolioId
        lift $ tableRows "d_b_tx_other_address" `dbShouldBe` []
        lift $ (fmap contactLabel <$> contactAt btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") `dbShouldBe` Just "Landlord"

txTagsSpec :: Spec
txTagsSpec =
  describe "The tags of a transaction" $ do
    it "are those the user put on it, in the order of the alphabet" $
      inDatabaseE $ do
        pool <- testPool
        lift $ tagTx pool (fakeTxHash 1) [rentTag, kycTag]
        lift $ (namesOf <$> txTags pool (fakeTxHash 1)) `dbShouldBe` ["kyc", "rent"]

    -- No summary is stored: a transaction just sent has tags before the
    -- sync stores its summary
    it "are kept apart from the transaction, which the wallet may not have yet" $
      inDatabaseE $ do
        pool <- testPool
        lift $ tagTx pool (fakeTxHash 9) [rentTag]
        lift $ (namesOf <$> txTags pool (fakeTxHash 9)) `dbShouldBe` ["rent"]

    -- A payment from portfolio 0 to portfolio 1 is a transaction of both
    it "are each portfolio's own" $
      inDatabaseE $ do
        payer <- testPool
        paid <- (`portfolioPool` btc) <$> nextPortfolioOf testMnemonic
        lift $ tagTx payer (fakeTxHash 1) [rentTag]
        lift $ (namesOf <$> txTags paid (fakeTxHash 1)) `dbShouldBe` []

    it "name the tags to take off that the transaction did not have" $
      inDatabaseE $ do
        pool <- testPool
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ untagTx pool (fakeTxHash 1) [rentTag, kycTag] `dbShouldBe` [kycTag]
        lift $ txTags pool (fakeTxHash 1) `dbShouldBe` []

    -- A pending transaction is tagged under its nosigHash, here hash 7,
    -- and the network has it under its id, here hash 2
    it "move from the nosigHash of a pending transaction to its id once the network has it" $
      inDatabaseE $ do
        pool <- testPool
        lift $ tagTx pool (fakeTxHash 7) [rentTag]
        lift $ moveTxTags pool (fakeTxHash 7) (fakeTxHash 2)
        lift $ (namesOf <$> txTags pool (fakeTxHash 2)) `dbShouldBe` ["rent"]
        lift $ txTags pool (fakeTxHash 7) `dbShouldBe` []

    it "are given to the replacement of a transaction, which keeps them too" $
      inDatabaseE $ do
        pool <- testPool
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        lift $ copyTxTags pool (fakeTxHash 2) pool (fakeTxHash 3)
        lift $ (namesOf <$> txTags pool (fakeTxHash 3)) `dbShouldBe` ["rent"]
        lift $ (namesOf <$> txTags pool (fakeTxHash 2)) `dbShouldBe` ["rent"]

    it "are written by exportwallet with those of the other transactions of the pool, in the order of the ids" $
      inDatabaseE $ do
        pool <- testPool
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag, coldTag]
        lift $ poolTxTags pool `dbShouldBe` [(fakeTxHash 1, [coldTag, kycTag]), (fakeTxHash 2, [rentTag])]

addressListingSpec :: Spec
addressListingSpec =
  describe "The addresses of a chain that have a tag" $ do
    let indexes = map dBAddressIndex
    -- /0/0 and /0/2 have the tag, /0/1 has none
    it "are listed alone, the newest first, as addresses lists them" $
      inDatabaseE $ do
        (accId, [first, _, third]) <- receivingAddresses 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress third [kycTag]
        lift $ (indexes <$> taggedAddressesAfter UsualOrder accId AddrExternal [kycTag] Nothing Nothing) `dbShouldBe` [2, 0]

    it "are listed from the first address on in the reverse order" $
      inDatabaseE $ do
        (accId, [first, _, third]) <- receivingAddresses 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress third [kycTag]
        lift $ (indexes <$> taggedAddressesAfter ReverseOrder accId AddrExternal [kycTag] Nothing Nothing) `dbShouldBe` [0, 2]

    it "go on after the address given, up to the count" $
      inDatabaseE $ do
        (accId, rows) <- receivingAddresses 3
        lift $ mapM_ (`tagAddress` [kycTag]) rows
        lift $ (indexes <$> taggedAddressesAfter UsualOrder accId AddrExternal [kycTag] (Just 2) (Just 1)) `dbShouldBe` [1]

    -- /0/0 has kyc, /0/1 has cold, /0/2 has both
    it "are those that have any one of several tags, each listed once" $
      inDatabaseE $ do
        (accId, [first, second, third]) <- receivingAddresses 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress second [coldTag]
        lift $ tagAddress third [kycTag, coldTag]
        lift $ (indexes <$> taggedAddressesAfter UsualOrder accId AddrExternal [kycTag, coldTag] Nothing Nothing) `dbShouldBe` [2, 1, 0]

    it "leave out the tagged addresses of the other chain" $
      inDatabaseE $ do
        (accId, [first]) <- receivingAddresses 1
        change <- nextChangeAddress (def :: Config) accId []
        lift $ tagAddress first [kycTag]
        lift $ tagAddress change [kycTag]
        lift $
          (map (\row -> (dBAddressInternal row, dBAddressIndex row)) <$> taggedAddressesAfter UsualOrder accId AddrInternal [kycTag] Nothing Nothing)
            `dbShouldBe` [(True, 0)]

untaggedAddressListingSpec :: Spec
untaggedAddressListingSpec =
  describe "The addresses of a chain that have no tag" $ do
    let indexes = map dBAddressIndex
        untagged = TagFilter [] True
    -- /0/1 has a tag, /0/0 and /0/2 have none
    it "are listed alone, the newest first, as addresses lists them" $
      inDatabaseE $ do
        (accId, [_, second, _]) <- receivingAddresses 3
        lift $ tagAddress second [kycTag]
        lift $ (indexes <$> keptAddressesAfter UsualOrder accId AddrExternal untagged Nothing Nothing) `dbShouldBe` [2, 0]

    it "are listed from the first address on in the reverse order" $
      inDatabaseE $ do
        (accId, [_, second, _]) <- receivingAddresses 3
        lift $ tagAddress second [kycTag]
        lift $ (indexes <$> keptAddressesAfter ReverseOrder accId AddrExternal untagged Nothing Nothing) `dbShouldBe` [0, 2]

    -- /0/2 has a tag. After /0/3 come /0/1 and /0/0, and the count keeps
    -- the first of them.
    it "go on after the address given, up to the count" $
      inDatabaseE $ do
        (accId, [_, _, third, _]) <- receivingAddresses 4
        lift $ tagAddress third [kycTag]
        lift $ (indexes <$> keptAddressesAfter UsualOrder accId AddrExternal untagged (Just 3) (Just 1)) `dbShouldBe` [1]

    it "are all of them when no address has a tag" $
      inDatabaseE $ do
        (accId, _) <- receivingAddresses 2
        lift $ (indexes <$> keptAddressesAfter UsualOrder accId AddrExternal untagged Nothing Nothing) `dbShouldBe` [1, 0]

    it "include an address whose last tag was taken off" $
      inDatabaseE $ do
        (accId, [first]) <- receivingAddresses 1
        lift $ tagAddress first [kycTag]
        lift $ (indexes <$> keptAddressesAfter UsualOrder accId AddrExternal untagged Nothing Nothing) `dbShouldBe` []
        _ <- lift $ untagAddress first [kycTag]
        lift $ (indexes <$> keptAddressesAfter UsualOrder accId AddrExternal untagged Nothing Nothing) `dbShouldBe` [0]

    -- /0/0 has kyc, /0/1 has cold, and /0/2 has no tag
    it "are listed with the addresses of a tag asked for as well" $
      inDatabaseE $ do
        (accId, [first, second, _]) <- receivingAddresses 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress second [coldTag]
        lift $ (indexes <$> keptAddressesAfter UsualOrder accId AddrExternal (TagFilter [kycTag] True) Nothing Nothing) `dbShouldBe` [2, 0]

coinListingSpec :: Spec
coinListingSpec =
  describe "The coins at the addresses that have a tag" $ do
    it "are listed alone by coins" $
      inDatabaseE $ do
        pool <- testPool
        (accId, [first, _]) <- receivingAddresses 2
        twoCoins accId
        lift $ tagAddress first [kycTag]
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins (TagFilter [kycTag] False) Nothing Nothing)
          `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]

    it "show the tags of their address" $
      inDatabaseE $ do
        pool <- testPool
        (accId, [first, _]) <- receivingAddresses 2
        twoCoins accId
        lift $ tagAddress first [kycTag, coldTag]
        (map (namesOf . jsonCoinAddressTags . fst) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins noTagFilter Nothing Nothing)
          `dbShouldBeE` [[], ["cold", "kyc"]]

    -- Only /0/1 has the tag: its coin of 30000 is the whole balance listed
    it "are added up alone by balances" $
      inDatabaseE $ do
        (accId, [_, second]) <- receivingAddresses 2
        twoCoins accId
        lift $ tagAddress second [kycTag]
        (map (\(addr, balance) -> (dBAddressIndex addr, coinBalanceTotal balance)) <$> coinBalancesAfter UsualOrder btc [accId] (TagFilter [kycTag] False) Nothing Nothing)
          `dbShouldBeE` [(1, 30000)]

    it "are both listed by balances when no tag is asked for" $
      inDatabaseE $ do
        (accId, _) <- receivingAddresses 2
        twoCoins accId
        (map (\(addr, balance) -> (dBAddressIndex addr, coinBalanceTotal balance)) <$> coinBalancesAfter UsualOrder btc [accId] noTagFilter Nothing Nothing)
          `dbShouldBeE` [(0, 50000), (1, 30000)]

    -- /0/0 has a tag: the coin of 30000 at /0/1 is the one at an address
    -- without a tag
    it "are left out by coins asked for the coins at the addresses without a tag" $
      inDatabaseE $ do
        pool <- testPool
        (accId, [first, _]) <- receivingAddresses 2
        twoCoins accId
        lift $ tagAddress first [kycTag]
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins (TagFilter [] True) Nothing Nothing)
          `dbShouldBeE` [OutPoint (fakeTxHash 2) 0]

    -- /0/1 has a tag: the 50000 at /0/0 are what no tag holds
    it "are left out by balances asked for the addresses without a tag" $
      inDatabaseE $ do
        (accId, [_, second]) <- receivingAddresses 2
        twoCoins accId
        lift $ tagAddress second [kycTag]
        (map (\(addr, balance) -> (dBAddressIndex addr, coinBalanceTotal balance)) <$> coinBalancesAfter UsualOrder btc [accId] (TagFilter [] True) Nothing Nothing)
          `dbShouldBeE` [(0, 50000)]

    -- /0/0 has kyc and /0/1 has cold: kyc and the addresses without a tag
    -- leave the coin at /0/1 out
    it "are listed by coins with the coins at the addresses without a tag, when both are asked for" $
      inDatabaseE $ do
        pool <- testPool
        (accId, [first, second]) <- receivingAddresses 2
        twoCoins accId
        lift $ tagAddress first [kycTag]
        lift $ tagAddress second [coldTag]
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins (TagFilter [kycTag] True) Nothing Nothing)
          `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]

txListingSpec :: String -> Natural -> Spec
txListingSpec way lookedUp =
  describe ("The transactions a filter of tags keeps, " <> way) $ do
    -- The whole listing: the second payment first, after which the
    -- portfolio holds 80000, then the first one, after which it held
    -- 50000
    it "are all of them without a tag asked for, each with the balance after it" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        keptIn lookedUp UsualOrder pool noTxTagFilter Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    it "are those that have a tag asked for" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] False) Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    -- The first payment pays /0/0, which has the tag
    it "are those at an address that has a tag of addresses asked for" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagAddress first [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- A filter only leaves rows out: the first payment shows 50000, what
    -- the portfolio held after it, though the listing skips the payment
    -- above it
    it "show the balance they show in the whole listing" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] False) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    it "show the same balance in the reverse order" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp ReverseOrder pool (TxTagFilter [rentTag] [] False) Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    -- The second payment has the tag rent, and the first one pays an
    -- address that has the tag kyc
    it "are those that match any one of the tags, of either kind" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        lift $ tagAddress first [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [kycTag] False) Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    -- Both payments have the tag. The oldest comes first, and each shows
    -- the balance after it: 50000, then 50000 + 30000 = 80000.
    it "show the balance after each in the reverse order, from the oldest on" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp ReverseOrder pool (TxTagFilter [rentTag] [] False) Nothing
          `dbShouldBeE` [(fakeTxHash 1, Just 50000), (fakeTxHash 2, Just 80000)]

    -- The second payment is replaced or dropped: the portfolio holds the
    -- 50000 of the first one alone, and the dead payment shows no balance
    it "show no balance for a dead transaction, which the balance leaves out" $
      inDatabaseE $ do
        (pool, accId, _) <- twoPayments
        lift $ markTxInfoDead pool 800002 (fakeTxHash 2)
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 50000]
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] False) Nothing
          `dbShouldBeE` [(fakeTxHash 2, Nothing), (fakeTxHash 1, Just 50000)]

    it "are none when no transaction matches" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ ensureTag rentTag
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] False) Nothing `dbShouldBeE` []

    -- --after names the second payment, which the filter does not keep
    it "go on after a transaction the filter does not keep" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        cursor <- txsCursorAt UsualOrder pool (fakeTxHash 2)
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] False) (Just cursor) `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- A third payment of 30000 at /0/1 waits in the mempool: the
    -- portfolio then holds 80000 + 30000 = 110000 with it
    it "keep an unconfirmed transaction that matches, first in the listing" $
      inDatabaseE $ do
        (pool, accId, _) <- twoPayments
        lift $ void $ repsertSummary pool (fakeTxHash 3) (Store.MemRef 1700000300) seenTime secondPaymentSummary
        lift $ P.update accId [DBAccountBalanceUnconfirmed P.=. 30000]
        lift $ tagTx pool (fakeTxHash 3) [rentTag]
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] False) Nothing
          `dbShouldBeE` [(fakeTxHash 3, Just 110000), (fakeTxHash 1, Just 50000)]

    it "tell each transaction which tags of addresses the listing keeps, for its short views" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagAddress first [kycTag, coldTag]
        rows <- keptTxsAfterBy lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing Nothing
        let shown = [(namesOf (keptTagsAt (listedTxInfo listed) (externalAddr 0)), namesOf (tagsOfAddress (listedTxInfo listed))) | (listed, _) <- rows]
            tagsOfAddress info = Map.findWithDefault [] (externalAddr 0) (txAddressTags (txInfoTags info))
        lift $ return shown `dbShouldBe` [(["kyc"], ["cold", "kyc"])]

    -- A full sync writes every summary again, and with it the addresses
    -- of each transaction
    it "find the transactions again once their summaries are written again, as a full sync does" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagAddress first [kycTag]
        lift $ P.deleteWhere ([] :: [P.Filter DBTxAddress])
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` []
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- The first payment was paid from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU,
    -- and the second from 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "are those paid from a contact that has a tag of addresses asked for" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    -- The spending pays 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, after
    -- which the portfolio holds 80000 - 41000 = 39000
    it "are those that pay a contact that has a tag of addresses asked for" $
      inDatabaseE $ do
        pool <- paidFromTwo
        lift $ void $ repsertSummary pool (fakeTxHash 3) (Store.BlockRef 800002 1) seenTime spendSummary
        (accId, _) <- testNewAcc
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 39000]
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag] False) Nothing
          `dbShouldBeE` [(fakeTxHash 3, Just 39000), (fakeTxHash 1, Just 50000)]

    it "are none when the contact has another tag than the one asked for" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [coldTag]
        lift $ ensureTag kycTag
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` []

    -- The contact was added after its payment was synced
    it "find the past transactions of a contact added after them" $
      inDatabaseE $ do
        pool <- paidFromTwo
        keptIn lookedUp UsualOrder pool noTxTagFilter Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag] False) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- kyc is on the address the first payment pays, and on the contact
    -- that paid the second one
    it "are those at a tagged address of the portfolio and those at a tagged contact, together" $
      inDatabaseE $ do
        pool <- paidFromTwo
        (accId, _) <- testNewAcc
        rowM <- lift $ P.get (DBAddressKey accId False 0)
        lift $ mapM_ (`tagAddress` [kycTag]) rowM
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    -- The payer of the first payment also paid 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    -- in the same transaction: that output is the payer's business, and
    -- no view of the payment shows it
    it "leave out a payment received whose payer also paid a contact in the same transaction" $
      inDatabaseE $ do
        pool <- testPool
        _ <- receivingAddresses 1
        let alsoPaysAlice = paymentSummary {summaryOtherOutputs = Map.fromList [(otherAddr 1, 70000)]}
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime alsoPaysAlice
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` []

    -- The contact is on Bitcoin Cash, and the payment on Bitcoin.
    -- bitcoincash:qrauw9xxxlvgm4s6ye4swvmj4aw4c68fp579jhzlam is the
    -- CashAddr form of 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ.
    it "leave out a transaction at an address that is a contact on another network" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact bch (otherAddr 1) "Alice, on cash"
        lift $ tagContact bch "bitcoincash:qrauw9xxxlvgm4s6ye4swvmj4aw4c68fp579jhzlam" [kycTag]
        lift $ (fmap (map (tagText . shownTag) . contactTags) <$> contactAt bch "bitcoincash:qrauw9xxxlvgm4s6ye4swvmj4aw4c68fp579jhzlam") `dbShouldBe` Just ["kyc"]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag] False) Nothing `dbShouldBeE` []

    -- A full sync writes every summary again, and with it the addresses
    -- of others of each transaction
    it "find the transactions of a contact again once their summaries are written again, as a full sync does" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        lift $ P.deleteWhere ([] :: [P.Filter DBTxOtherAddress])
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag] False) Nothing `dbShouldBeE` []
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag] False) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    it "tell each transaction which tags the listing keeps, for its short views to show them beside a contact" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag, coldTag]
        rows <- keptTxsAfterBy lookedUp UsualOrder pool (TxTagFilter [] [rentTag] False) Nothing Nothing
        let shown = [namesOf (keptContactTags info contact) | (listed, _) <- rows, let info = listedTxInfo listed, contact <- Map.elems (txInfoContacts info)]
        lift $ return shown `dbShouldBe` [["rent"]]

    it "are those without a tag when the transactions left to sort are asked for" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp UsualOrder pool leftToSort Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    it "are none left to sort once every transaction has a tag" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagTx pool (fakeTxHash 2) [kycTag]
        keptIn lookedUp UsualOrder pool leftToSort Nothing `dbShouldBeE` []

    -- The second payment was paid from 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ,
    -- a contact that has a tag: the tag of the contact sorts it
    it "are not left to sort when paid from a contact that has a tag" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool leftToSort Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- The spending pays 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, the
    -- landlord, who also paid the first payment. The second payment, from
    -- somebody else, is the one left. It shows 80000, what the portfolio
    -- held after it, before the spending.
    it "are not left to sort when they pay a contact that has a tag" $
      inDatabaseE $ do
        pool <- paidFromTwo
        lift $ void $ repsertSummary pool (fakeTxHash 3) (Store.BlockRef 800002 1) seenTime spendSummary
        (accId, _) <- testNewAcc
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 39000]
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        keptIn lookedUp UsualOrder pool leftToSort Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    it "are still left to sort when paid from a contact that has no tag" $
      inDatabaseE $ do
        pool <- paidFromTwo
        addContact btc (otherAddr 1) "Alice"
        keptIn lookedUp UsualOrder pool leftToSort Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    -- The tag of an address of the portfolio says which group its coins
    -- are in, not what the payment is for
    it "are still left to sort when they pay an address of the portfolio that has a tag" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagAddress first [kycTag]
        keptIn lookedUp UsualOrder pool leftToSort Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    -- The first payment has rent, the second one cold, and the spending
    -- has no tag. After the spending of 41000 the portfolio holds
    -- 80000 - 41000 = 39000.
    it "are those of a tag asked for and those left to sort, together" $
      inDatabaseE $ do
        (pool, accId, _) <- twoPayments
        lift $ void $ repsertSummary pool (fakeTxHash 3) (Store.BlockRef 800002 1) seenTime spendSummary
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 39000]
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagTx pool (fakeTxHash 2) [coldTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [] True) Nothing
          `dbShouldBeE` [(fakeTxHash 3, Just 39000), (fakeTxHash 1, Just 50000)]

    it "show the tags of a transaction and of its addresses when it is listed" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagAddress first [kycTag]
        infoM <- storedTxInfo pool (fakeTxHash 1)
        let shown = (\info -> (namesOf (txOwnTags (txInfoTags info)), [(addr, namesOf tags) | (addr, tags) <- Map.toList (txAddressTags (txInfoTags info))])) <$> infoM
        lift $ return shown `dbShouldBe` Just (["rent"], [(externalAddr 0, ["kyc"])])

-- | The ids a statement or a journal of some transactions asks for: those
-- of the transactions of a pool a filter keeps. The two payments are the
-- transactions 1 and 2 of the pool.
keptIdsSpec :: Spec
keptIdsSpec =
  describe "The ids of the transactions of a pool a filter of tags keeps" $ do
    it "are those of the transactions that have a tag asked for" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        txidsKept pool (TxTagFilter [rentTag] [] False) `dbShouldBeE` [fakeTxHash 2]

    -- The first payment pays /0/0, which has the tag
    it "are those of the transactions at an address that has a tag of addresses asked for" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagAddress first [kycTag]
        txidsKept pool (TxTagFilter [] [kycTag] False) `dbShouldBeE` [fakeTxHash 1]

    -- The second payment has a tag, so the first one is left to sort
    it "are those of the transactions left to sort, when those are asked for" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        txidsKept pool leftToSort `dbShouldBeE` [fakeTxHash 1]

    it "are none when no transaction has the tag asked for" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments
        lift $ ensureTag rentTag
        txidsKept pool (TxTagFilter [rentTag] [] False) `dbShouldBeE` []

    it "are those of every transaction that matches any one of the options, each once" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagAddress first [kycTag]
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        (sort <$> txidsKept pool (TxTagFilter [rentTag] [kycTag] False)) `dbShouldBeE` [fakeTxHash 1, fakeTxHash 2]
