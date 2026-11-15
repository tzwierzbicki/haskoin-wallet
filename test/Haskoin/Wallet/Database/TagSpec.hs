{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The tags of the database: the tags themselves and their colours, the
-- tags of addresses and of transactions, and the listings that keep the
-- rows of some tags. The pool is what portfolio 0 of the test wallet
-- holds on Bitcoin. The story of the listings is two payments received
-- by its bip44 account: 50000 at its first receiving address, in block
-- 800000, then 30000 at its second one, in block 800001.
module Haskoin.Wallet.Database.TagSpec (spec) where

import Control.Monad (void)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Database.Persist as P
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Config (Config (..))
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Account (deletePortfolio)
import Haskoin.Wallet.Database.Address (AddrType (..), CoinBalance (..), genExtAddress, nextChangeAddress, taggedAddressesAfter)
import Haskoin.Wallet.Database.Coin (JsonCoin (..), coinBalancesAfter, listedCoinsAfter, refreshCoins, CoinFilter (..))
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
spec = prepareContext $ \ctx -> do
  tagsSpec ctx
  addressTagsSpec ctx
  txTagsSpec ctx
  otherAddressSpec ctx
  addressListingSpec ctx
  coinListingSpec ctx
  -- txs finds the transactions of a filter by their ids when they are
  -- few, and by a walk of the history when they are many. Both ways must
  -- list the same rows, so every rule is checked both ways: up to 2000
  -- transactions looked up, as hw does, and none, which always walks.
  txListingSpec "found by their ids" 2000 ctx
  txListingSpec "found by a walk of the history" 0 ctx

{- The tags of these tests are those of TestUtils: kyc, cold and rent -}

-- | The words of tags as an answer shows them
namesOf :: [ShownTag] -> [Text]
namesOf = map (tagText . shownTag)

-- | The first receiving addresses of the bip44 account of the pool, as
-- many as asked, generated in the database: /0/0, /0/1 and so on
receivingAddresses :: Ctx -> Int -> ExceptT String (DB IO) (DBAccountId, [DBAddress])
receivingAddresses ctx count = do
  (accId, _) <- testNewAcc ctx
  rows <- mapM (const (genExtAddress ctx (def :: Config) accId "")) [1 .. count]
  return (accId, rows)

{- The payments of the story -}

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
twoPayments :: Ctx -> ExceptT String (DB IO) (Pool, DBAccountId, [DBAddress])
twoPayments ctx = do
  pool <- testPool ctx
  (accId, rows) <- receivingAddresses ctx 2
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
paidFromTwo :: Ctx -> ExceptT String (DB IO) Pool
paidFromTwo ctx = do
  pool <- testPool ctx
  (accId, _) <- receivingAddresses ctx 2
  lift $ do
    void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
    void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime secondPaymentSummary {summaryOtherInputs = Map.fromList [(otherAddr 1, 31000)]}
    P.update accId [DBAccountBalanceConfirmed P.=. 80000]
  return pool

-- | The id and the balance of each transaction txs lists with a filter of
-- tags, in the order given, looking up at most that many transactions by
-- their ids
keptIn :: Natural -> ListingOrder -> Pool -> TxTagFilter -> Maybe TxsCursor -> ExceptT String (DB IO) [(TxHash, Maybe Integer)]
keptIn lookedUp order pool kept afterM = do
  rows <- keptTxsAfterBy lookedUp order pool kept afterM Nothing
  return [(chainTxid (chainOf (listedTxInfo listed)), listedTxBalance listed) | (listed, _) <- rows]

tagsSpec :: Ctx -> Spec
tagsSpec ctx =
  describe "A tag of the database" $ do
    it "exists from the first time it is put on something, without a colour" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
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
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [rentTag, coldTag, kycTag]
        lift $ (namesOf <$> allTags) `dbShouldBe` ["cold", "kyc", "rent"]

    it "stays, with its colour, when nothing has it any more" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag]
        lift $ setTagColor kycTag (Just TagRed)
        _ <- lift $ untagAddress addr [kycTag]
        lift $ allTags `dbShouldBe` [ShownTag kycTag (Just TagRed)]

    -- kyc is on two addresses and one transaction, cold on one address
    it "is counted on the addresses and the transactions that have it" $
      inDatabaseE $ do
        pool <- testPool ctx
        (_, [first, second]) <- receivingAddresses ctx 2
        lift $ tagAddress first [kycTag, coldTag]
        lift $ tagAddress second [kycTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag]
        lift $ tagUses `dbShouldBe` [TagUse (ShownTag coldTag Nothing) 1 0 0, TagUse (ShownTag kycTag Nothing) 2 1 0]

    it "is deleted from every address and transaction that has it, and says how many had it" $
      inDatabaseE $ do
        pool <- testPool ctx
        (_, [first, second]) <- receivingAddresses ctx 2
        lift $ tagAddress first [kycTag, coldTag]
        lift $ tagAddress second [kycTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag]
        lift $ deleteTag kycTag `dbShouldBe` TagDeleted 2 1 0
        lift $ (namesOf <$> allTags) `dbShouldBe` ["cold"]
        lift $ (namesOf <$> addressTags first) `dbShouldBe` ["cold"]
        lift $ txTags pool (fakeTxHash 1) `dbShouldBe` []

    it "must exist for a command that filters by it or spends from it" $
      inDatabaseE $ do
        lift $ dbShouldFailWith "There is no tag kyc. hw tags lists the tags." (requireTags [kycTag])

    it "is offered to the shell when it starts with the word typed" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag, coldTag, forceRight (readTag "kitchen")]
        lift $ tagsStarting "k" 21 `dbShouldBe` ["kitchen", "kyc"]

addressTagsSpec :: Ctx -> Spec
addressTagsSpec ctx =
  describe "The tags of an address" $ do
    it "are those the user put on it, in the order of the alphabet" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag, coldTag]
        lift $ (namesOf <$> addressTags addr) `dbShouldBe` ["cold", "kyc"]

    it "come with the colour each tag has now" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag]
        lift $ setTagColor kycTag (Just TagRed)
        lift $ addressTags addr `dbShouldBe` [ShownTag kycTag (Just TagRed)]

    it "hold a tag once, however many times it is put on" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag]
        lift $ tagAddress addr [kycTag]
        lift $ (namesOf <$> addressTags addr) `dbShouldBe` ["kyc"]

    it "are each address's own" $
      inDatabaseE $ do
        (_, [first, second]) <- receivingAddresses ctx 2
        lift $ tagAddress first [kycTag]
        lift $ (namesOf <$> addressTags second) `dbShouldBe` []

    it "lose the tags taken off, and keep the others" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag, coldTag]
        lift $ untagAddress addr [kycTag] `dbShouldBe` []
        lift $ (namesOf <$> addressTags addr) `dbShouldBe` ["cold"]

    it "name the tags to take off that the address did not have" $
      inDatabaseE $ do
        (_, [addr]) <- receivingAddresses ctx 1
        lift $ tagAddress addr [kycTag]
        lift $ untagAddress addr [kycTag, coldTag] `dbShouldBe` [coldTag]

    -- A change address takes no label, and takes tags
    it "go on a change address too" $
      inDatabaseE $ do
        (accId, _) <- testNewAcc ctx
        change <- nextChangeAddress ctx (def :: Config) accId []
        lift $ tagAddress change [kycTag]
        lift $ (namesOf <$> addressTags change) `dbShouldBe` ["kyc"]

    it "go with the portfolio that is deleted, while the tag stays" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (_, [addr]) <- receivingAddresses ctx 1
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

otherAddressSpec :: Ctx -> Spec
otherAddressSpec ctx =
  describe "The addresses of others of a transaction, as txs finds a tagged contact at them" $ do
    -- The spending pays 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "are those a payment pays" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary
        lift $ otherAddressesOf pool (fakeTxHash 2) `dbShouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The payment received was paid from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU
    it "are those a payment received was paid from" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        lift $ otherAddressesOf pool (fakeTxHash 1) `dbShouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The payer also paid 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ: the change
    -- of the payer, or somebody the payer paid
    it "leave out what the payer of a payment received paid to others" $
      inDatabaseE $ do
        pool <- testPool ctx
        let alsoPaysOthers = paymentSummary {summaryOtherOutputs = Map.fromList [(otherAddr 1, 70000)]}
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime alsoPaysOthers
        lift $ otherAddressesOf pool (fakeTxHash 1) `dbShouldBe` ["1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"]

    -- The summary is written again, paying another address
    it "are those of the summary written last" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary {summaryOtherOutputs = Map.fromList [(otherAddr 1, 40000)]}
        lift $ otherAddressesOf pool (fakeTxHash 2) `dbShouldBe` ["1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ"]

    it "go with the portfolio that is deleted, while the address book stays" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        let pool = portfolioPool portfolio btc
        lift $ void $ repsertSummary pool (fakeTxHash 2) (Store.BlockRef 800001 1) seenTime spendSummary
        addContact btc (otherAddr 0) "Landlord"
        lift $ deletePortfolio portfolio.portfolioId
        lift $ tableRows "d_b_tx_other_address" `dbShouldBe` []
        lift $ (fmap contactLabel <$> contactAt btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU") `dbShouldBe` Just "Landlord"

txTagsSpec :: Ctx -> Spec
txTagsSpec ctx =
  describe "The tags of a transaction" $ do
    it "are those the user put on it, in the order of the alphabet" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ tagTx pool (fakeTxHash 1) [rentTag, kycTag]
        lift $ (namesOf <$> txTags pool (fakeTxHash 1)) `dbShouldBe` ["kyc", "rent"]

    -- No summary is stored: a transaction just sent has tags before the
    -- sync stores its summary
    it "are kept apart from the transaction, which the wallet may not have yet" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ tagTx pool (fakeTxHash 9) [rentTag]
        lift $ (namesOf <$> txTags pool (fakeTxHash 9)) `dbShouldBe` ["rent"]

    -- A payment from portfolio 0 to portfolio 1 is a transaction of both
    it "are each portfolio's own" $
      inDatabaseE $ do
        payer <- testPool ctx
        paid <- (`portfolioPool` btc) <$> nextPortfolioOf ctx testMnemonic
        lift $ tagTx payer (fakeTxHash 1) [rentTag]
        lift $ (namesOf <$> txTags paid (fakeTxHash 1)) `dbShouldBe` []

    it "name the tags to take off that the transaction did not have" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ untagTx pool (fakeTxHash 1) [rentTag, kycTag] `dbShouldBe` [kycTag]
        lift $ txTags pool (fakeTxHash 1) `dbShouldBe` []

    -- A pending transaction is tagged under its nosigHash, here hash 7,
    -- and the network has it under its id, here hash 2
    it "move from the nosigHash of a pending transaction to its id once the network has it" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ tagTx pool (fakeTxHash 7) [rentTag]
        lift $ moveTxTags pool (fakeTxHash 7) (fakeTxHash 2)
        lift $ (namesOf <$> txTags pool (fakeTxHash 2)) `dbShouldBe` ["rent"]
        lift $ txTags pool (fakeTxHash 7) `dbShouldBe` []

    it "are given to the replacement of a transaction, which keeps them too" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        lift $ copyTxTags pool (fakeTxHash 2) pool (fakeTxHash 3)
        lift $ (namesOf <$> txTags pool (fakeTxHash 3)) `dbShouldBe` ["rent"]
        lift $ (namesOf <$> txTags pool (fakeTxHash 2)) `dbShouldBe` ["rent"]

    it "are written by exportwallet with those of the other transactions of the pool, in the order of the ids" $
      inDatabaseE $ do
        pool <- testPool ctx
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        lift $ tagTx pool (fakeTxHash 1) [kycTag, coldTag]
        lift $ poolTxTags pool `dbShouldBe` [(fakeTxHash 1, [coldTag, kycTag]), (fakeTxHash 2, [rentTag])]

addressListingSpec :: Ctx -> Spec
addressListingSpec ctx =
  describe "The addresses of a chain that have a tag" $ do
    let indexes = map dBAddressIndex
    -- /0/0 and /0/2 have the tag, /0/1 has none
    it "are listed alone, the newest first, as addresses lists them" $
      inDatabaseE $ do
        (accId, [first, _, third]) <- receivingAddresses ctx 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress third [kycTag]
        lift $ (indexes <$> taggedAddressesAfter UsualOrder accId AddrExternal [kycTag] Nothing Nothing) `dbShouldBe` [2, 0]

    it "are listed from the first address on in the reverse order" $
      inDatabaseE $ do
        (accId, [first, _, third]) <- receivingAddresses ctx 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress third [kycTag]
        lift $ (indexes <$> taggedAddressesAfter ReverseOrder accId AddrExternal [kycTag] Nothing Nothing) `dbShouldBe` [0, 2]

    it "go on after the address given, up to the count" $
      inDatabaseE $ do
        (accId, rows) <- receivingAddresses ctx 3
        lift $ mapM_ (`tagAddress` [kycTag]) rows
        lift $ (indexes <$> taggedAddressesAfter UsualOrder accId AddrExternal [kycTag] (Just 2) (Just 1)) `dbShouldBe` [1]

    -- /0/0 has kyc, /0/1 has cold, /0/2 has both
    it "are those that have any one of several tags, each listed once" $
      inDatabaseE $ do
        (accId, [first, second, third]) <- receivingAddresses ctx 3
        lift $ tagAddress first [kycTag]
        lift $ tagAddress second [coldTag]
        lift $ tagAddress third [kycTag, coldTag]
        lift $ (indexes <$> taggedAddressesAfter UsualOrder accId AddrExternal [kycTag, coldTag] Nothing Nothing) `dbShouldBe` [2, 1, 0]

    it "leave out the tagged addresses of the other chain" $
      inDatabaseE $ do
        (accId, [first]) <- receivingAddresses ctx 1
        change <- nextChangeAddress ctx (def :: Config) accId []
        lift $ tagAddress first [kycTag]
        lift $ tagAddress change [kycTag]
        lift $
          (map (\row -> (dBAddressInternal row, dBAddressIndex row)) <$> taggedAddressesAfter UsualOrder accId AddrInternal [kycTag] Nothing Nothing)
            `dbShouldBe` [(True, 0)]

coinListingSpec :: Ctx -> Spec
coinListingSpec ctx =
  describe "The coins at the addresses that have a tag" $ do
    -- The coins of the story: 50000 at /0/0, of transaction 1, and 30000
    -- at /0/1, of transaction 2
    let twoCoins accId =
          void $
            refreshCoins
              btc
              ctx
              accId
              (const seenTime)
              [externalAddr 0, externalAddr 1]
              [storeCoin ctx (fakeTxHash 1, 0) (Just 800000) (externalAddr 0) 50000, storeCoin ctx (fakeTxHash 2, 0) (Just 800001) (externalAddr 1) 30000]

    it "are listed alone by coins" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, [first, _]) <- receivingAddresses ctx 2
        twoCoins accId
        lift $ tagAddress first [kycTag]
        (map (jsonCoinOutpoint . fst) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins [kycTag] Nothing Nothing)
          `dbShouldBeE` [OutPoint (fakeTxHash 1) 0]

    it "show the tags of their address" $
      inDatabaseE $ do
        pool <- testPool ctx
        (accId, [first, _]) <- receivingAddresses ctx 2
        twoCoins accId
        lift $ tagAddress first [kycTag, coldTag]
        (map (namesOf . jsonCoinAddressTags . fst) <$> listedCoinsAfter UsualOrder pool [accId] AllCoins [] Nothing Nothing)
          `dbShouldBeE` [[], ["cold", "kyc"]]

    -- Only /0/1 has the tag: its coin of 30000 is the whole balance listed
    it "are added up alone by balances" $
      inDatabaseE $ do
        (accId, [_, second]) <- receivingAddresses ctx 2
        twoCoins accId
        lift $ tagAddress second [kycTag]
        (map (\(addr, balance) -> (dBAddressIndex addr, coinBalanceTotal balance)) <$> coinBalancesAfter UsualOrder btc [accId] [kycTag] Nothing Nothing)
          `dbShouldBeE` [(1, 30000)]

    it "are both listed by balances without a tag" $
      inDatabaseE $ do
        (accId, _) <- receivingAddresses ctx 2
        twoCoins accId
        (map (\(addr, balance) -> (dBAddressIndex addr, coinBalanceTotal balance)) <$> coinBalancesAfter UsualOrder btc [accId] [] Nothing Nothing)
          `dbShouldBeE` [(0, 50000), (1, 30000)]

txListingSpec :: String -> Natural -> Ctx -> Spec
txListingSpec way lookedUp ctx =
  describe ("The transactions a filter of tags keeps, " <> way) $ do
    -- The whole listing: the second payment first, after which the
    -- portfolio holds 80000, then the first one, after which it held
    -- 50000
    it "are all of them without a tag asked for, each with the balance after it" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        keptIn lookedUp UsualOrder pool noTxTagFilter Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    it "are those that have a tag asked for" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] []) Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    -- The first payment pays /0/0, which has the tag
    it "are those at an address that has a tag of addresses asked for" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments ctx
        lift $ tagAddress first [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- A filter only leaves rows out: the first payment shows 50000, what
    -- the portfolio held after it, though the listing skips the payment
    -- above it
    it "show the balance they show in the whole listing" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] []) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    it "show the same balance in the reverse order" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp ReverseOrder pool (TxTagFilter [rentTag] []) Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    -- The second payment has the tag rent, and the first one pays an
    -- address that has the tag kyc
    it "are those that match any one of the tags, of either kind" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        lift $ tagAddress first [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] [kycTag]) Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    -- Both payments have the tag. The oldest comes first, and each shows
    -- the balance after it: 50000, then 50000 + 30000 = 80000.
    it "show the balance after each in the reverse order, from the oldest on" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp ReverseOrder pool (TxTagFilter [rentTag] []) Nothing
          `dbShouldBeE` [(fakeTxHash 1, Just 50000), (fakeTxHash 2, Just 80000)]

    -- The second payment is replaced or dropped: the portfolio holds the
    -- 50000 of the first one alone, and the dead payment shows no balance
    it "show no balance for a dead transaction, which the balance leaves out" $
      inDatabaseE $ do
        (pool, accId, _) <- twoPayments ctx
        lift $ markTxInfoDead pool 800002 (fakeTxHash 2)
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 50000]
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagTx pool (fakeTxHash 2) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] []) Nothing
          `dbShouldBeE` [(fakeTxHash 2, Nothing), (fakeTxHash 1, Just 50000)]

    it "are none when no transaction matches" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        lift $ ensureTag rentTag
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] []) Nothing `dbShouldBeE` []

    -- --after names the second payment, which the filter does not keep
    it "go on after a transaction the filter does not keep" $
      inDatabaseE $ do
        (pool, _, _) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        cursor <- txsCursorAt UsualOrder pool (fakeTxHash 2)
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] []) (Just cursor) `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- A third payment of 30000 at /0/1 waits in the mempool: the
    -- portfolio then holds 80000 + 30000 = 110000 with it
    it "keep an unconfirmed transaction that matches, first in the listing" $
      inDatabaseE $ do
        (pool, accId, _) <- twoPayments ctx
        lift $ void $ repsertSummary pool (fakeTxHash 3) (Store.MemRef 1700000300) seenTime secondPaymentSummary
        lift $ P.update accId [DBAccountBalanceUnconfirmed P.=. 30000]
        lift $ tagTx pool (fakeTxHash 3) [rentTag]
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [rentTag] []) Nothing
          `dbShouldBeE` [(fakeTxHash 3, Just 110000), (fakeTxHash 1, Just 50000)]

    it "tell each transaction which tags of addresses the listing keeps, for its short views" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments ctx
        lift $ tagAddress first [kycTag, coldTag]
        rows <- keptTxsAfterBy lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing Nothing
        let shown = [(namesOf (keptTagsAt (listedTxInfo listed) (externalAddr 0)), namesOf (tagsOfAddress (listedTxInfo listed))) | (listed, _) <- rows]
            tagsOfAddress info = Map.findWithDefault [] (externalAddr 0) (txAddressTags (txInfoTags info))
        lift $ return shown `dbShouldBe` [(["kyc"], ["cold", "kyc"])]

    -- A full sync writes every summary again, and with it the addresses
    -- of each transaction
    it "find the transactions again once their summaries are written again, as a full sync does" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments ctx
        lift $ tagAddress first [kycTag]
        lift $ P.deleteWhere ([] :: [P.Filter DBTxAddress])
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` []
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- The first payment was paid from 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU,
    -- and the second from 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    it "are those paid from a contact that has a tag of addresses asked for" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000)]

    -- The spending pays 40000 to 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU, after
    -- which the portfolio holds 80000 - 41000 = 39000
    it "are those that pay a contact that has a tag of addresses asked for" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        lift $ void $ repsertSummary pool (fakeTxHash 3) (Store.BlockRef 800002 1) seenTime spendSummary
        (accId, _) <- testNewAcc ctx
        lift $ P.update accId [DBAccountBalanceConfirmed P.=. 39000]
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag]) Nothing
          `dbShouldBeE` [(fakeTxHash 3, Just 39000), (fakeTxHash 1, Just 50000)]

    it "are none when the contact has another tag than the one asked for" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [coldTag]
        lift $ ensureTag kycTag
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` []

    -- The contact was added after its payment was synced
    it "find the past transactions of a contact added after them" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        keptIn lookedUp UsualOrder pool noTxTagFilter Nothing `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag]) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    -- kyc is on the address the first payment pays, and on the contact
    -- that paid the second one
    it "are those at a tagged address of the portfolio and those at a tagged contact, together" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        (accId, _) <- testNewAcc ctx
        rowM <- lift $ P.get (DBAddressKey accId False 0)
        lift $ mapM_ (`tagAddress` [kycTag]) rowM
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing
          `dbShouldBeE` [(fakeTxHash 2, Just 80000), (fakeTxHash 1, Just 50000)]

    -- The payer of the first payment also paid 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ
    -- in the same transaction: that output is the payer's business, and
    -- no view of the payment shows it
    it "leave out a payment received whose payer also paid a contact in the same transaction" $
      inDatabaseE $ do
        pool <- testPool ctx
        _ <- receivingAddresses ctx 1
        let alsoPaysAlice = paymentSummary {summaryOtherOutputs = Map.fromList [(otherAddr 1, 70000)]}
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime alsoPaysAlice
        addContact btc (otherAddr 1) "Alice"
        lift $ tagContact btc "1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ" [kycTag]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` []

    -- The contact is on Bitcoin Cash, and the payment on Bitcoin.
    -- bitcoincash:qrauw9xxxlvgm4s6ye4swvmj4aw4c68fp579jhzlam is the
    -- CashAddr form of 1PxH6Yutj49mRAabGvcTxnLkFZCuXDXvRJ.
    it "leave out a transaction at an address that is a contact on another network" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        addContact bch (otherAddr 1) "Alice, on cash"
        lift $ tagContact bch "bitcoincash:qrauw9xxxlvgm4s6ye4swvmj4aw4c68fp579jhzlam" [kycTag]
        lift $ (fmap (map (tagText . shownTag) . contactTags) <$> contactAt bch "bitcoincash:qrauw9xxxlvgm4s6ye4swvmj4aw4c68fp579jhzlam") `dbShouldBe` Just ["kyc"]
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [kycTag]) Nothing `dbShouldBeE` []

    -- A full sync writes every summary again, and with it the addresses
    -- of others of each transaction
    it "find the transactions of a contact again once their summaries are written again, as a full sync does" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag]
        lift $ P.deleteWhere ([] :: [P.Filter DBTxOtherAddress])
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag]) Nothing `dbShouldBeE` []
        lift $ void $ repsertSummary pool (fakeTxHash 1) (Store.BlockRef 800000 1) seenTime paymentSummary
        keptIn lookedUp UsualOrder pool (TxTagFilter [] [rentTag]) Nothing `dbShouldBeE` [(fakeTxHash 1, Just 50000)]

    it "tell each transaction which tags the listing keeps, for its short views to show them beside a contact" $
      inDatabaseE $ do
        pool <- paidFromTwo ctx
        addContact btc (otherAddr 0) "Landlord"
        lift $ tagContact btc "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" [rentTag, coldTag]
        rows <- keptTxsAfterBy lookedUp UsualOrder pool (TxTagFilter [] [rentTag]) Nothing Nothing
        let shown = [namesOf (keptContactTags info contact) | (listed, _) <- rows, let info = listedTxInfo listed, contact <- Map.elems (txInfoContacts info)]
        lift $ return shown `dbShouldBe` [["rent"]]

    it "show the tags of a transaction and of its addresses when it is listed" $
      inDatabaseE $ do
        (pool, _, [first, _]) <- twoPayments ctx
        lift $ tagTx pool (fakeTxHash 1) [rentTag]
        lift $ tagAddress first [kycTag]
        infoM <- storedTxInfo pool (fakeTxHash 1)
        let shown = (\info -> (namesOf (txOwnTags (txInfoTags info)), [(addr, namesOf tags) | (addr, tags) <- Map.toList (txAddressTags (txInfoTags info))])) <$> infoM
        lift $ return shown `dbShouldBe` Just (["rent"], [(externalAddr 0, ["kyc"])])
