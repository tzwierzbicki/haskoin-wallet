{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The addresses of an account: the receiving addresses that `receive`
-- gives out and the change addresses the wallet pays itself at, generated
-- along the two chains of the account within the gap of BIP44, with their
-- labels, balances and derivation paths. The account of these tests is
-- the bip44 account of portfolio 0 of the test wallet on Bitcoin, whose
-- first ten addresses of each chain are written out in
-- "Haskoin.Wallet.TestUtils". The addresses of a pool, the accounts of a
-- portfolio on one network together, are read with the type of their
-- account. An address the user types names a row once its network is
-- known, since the test networks share their keys.
module Haskoin.Wallet.Database.AddressSpec (spec) where

import Control.Monad (forM_, replicateM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode, toJSON)
import Data.Aeson.Types (parseEither)
import Data.Default (def)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Database.Account
import Haskoin.Wallet.Database.Address
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.Database.Portfolio (PortfolioRef (..), portfolioPool, portfolioRef)
import Haskoin.Wallet.Keys (AccountKind (..), ForeignKind (..), PoolPath (..), foreignAddress, legacyPath)
import Haskoin.Wallet.TestUtils
import Numeric.Natural (Natural)
import Haskoin.Util.Arbitrary (IdentityTests (..), JsonBox (..), testIdentity)
import Haskoin.Wallet.Arbitrary (arbitraryAddressBalance, arbitraryDBAddress)
import Test.Hspec
import Text.RawString.QQ (r)

spec :: Spec
spec = prepareContext $ \ctx -> do
  receivingSpec ctx
  changeSpec ctx
  restoringSpec ctx
  databaseSpec ctx
  poolSpec ctx
  typedSpec ctx
  pathSpec
  derivingSpec ctx
  jsonSpec
  -- An address and its balance read back as they were written
  testIdentity def {jsonTests = [JsonBox (arbitraryDBAddress btc), JsonBox arbitraryAddressBalance]}

-- | The account of these tests
account :: Ctx -> ExceptT String (DB IO) DBAccountId
account ctx = fst <$> testNewAcc ctx

-- | The next index of each chain of the account: external, then internal
nextIndices :: DBAccountId -> ExceptT String (DB IO) (Int, Int)
nextIndices accId = do
  acc <- getAccountById accId
  return (dBAccountExternal acc, dBAccountInternal acc)

-- | The indices of a page of a chain of the account, as `addresses` lists them
listed :: DBAccountId -> AddrType -> Maybe Int -> Maybe Natural -> ExceptT String (DB IO) [Int]
listed accId addrType afterM countM = map dBAddressIndex <$> lift (addressesAfter accId addrType afterM countM)

-- | The index, derivation path, text, label and chain of an address
addressRow :: DBAddress -> (Int, Text, Text, Text, Bool)
addressRow a = (dBAddressIndex a, pathText (addrPath a), dBAddressAddress a, dBAddressLabel a, dBAddressInternal a)

-- | The accounts that hold some of the addresses typed, by their keys, as
-- a command finds them: each address read on the network of each account
holders :: [Text] -> ExceptT String (DB IO) [DBAccountId]
holders typed = map (\(_, accId, _) -> accId) <$> accountsHolding (\net -> mapMaybe (textToAddr net) typed)

balanceOf :: DBAddress -> AddressBalance
balanceOf a =
  AddressBalance (dBAddressBalanceConfirmed a) (dBAddressBalanceUnconfirmed a) (dBAddressBalanceCoins a) (dBAddressBalanceTxs a) (dBAddressBalanceReceived a)

-- | The row of the receiving address /0/3 of the account, labelled, paid
-- three times for 9000 in all, with 5000 confirmed and 300 unconfirmed
-- left in two coins
fixtureRow :: DBAddress
fixtureRow = DBAddress (toSqlKey 1) False 3 "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1" "Savings" 5000 300 2 3 9000 fixtureTime

receivingGap, changeGap :: String
receivingGap = "The last 20 receiving addresses await a payment: reuse one of them, listed by hw addresses"
changeGap = "The last 20 change addresses are held by pending transactions: send or delete some of them"

receivingSpec :: Ctx -> Spec
receivingSpec ctx =
  describe "Receiving addresses" $ do
    let cfg = def :: Config
    it "gives the addresses of the external chain in order, with their labels" $
      inDatabaseE $ do
        accId <- account ctx
        first <- genExtAddress ctx cfg accId "Rent"
        second <- genExtAddress ctx cfg accId ""
        liftIO $ do
          addressRow first `shouldBe` (0, "/0/0", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "Rent", False)
          addressRow second `shouldBe` (1, "/0/1", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "", False)
          dBAddressAccount first `shouldBe` accId
          balanceOf first `shouldBe` AddressBalance 0 0 0 0 0
        nextIndices accId `dbShouldBeE` (2, 0)

    it "refuses the twenty-first address while the first twenty await a payment, and says so" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 20 $ genExtAddress ctx cfg accId ""
        lift $ dbShouldFailWith receivingGap $ genExtAddress ctx cfg accId ""
        nextIndices accId `dbShouldBeE` (20, 0)

    it "allows twenty addresses past the highest one that was paid" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 20 $ genExtAddress ctx cfg accId ""
        updateAddressBalances btc accId [paidOnce (externalAddr 2) 5000, paidOnce (externalAddr 4) 1000]
        lift (highestUsedIndex accId AddrExternal) `dbShouldBeE` Just 4
        -- Addresses 5 to 24 are the twenty unused ones past it
        replicateM_ 5 $ genExtAddress ctx cfg accId ""
        lift $ dbShouldFailWith receivingGap $ genExtAddress ctx cfg accId ""
        nextIndices accId `dbShouldBeE` (25, 0)

    it "counts an address whose coins were all spent as paid" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 20 $ genExtAddress ctx cfg accId ""
        updateAddressBalances btc accId [emptied (externalAddr 9) 5000]
        lift (highestUsedIndex accId AddrExternal) `dbShouldBeE` Just 9
        (dBAddressIndex <$> genExtAddress ctx cfg accId "") `dbShouldBeE` 20

    it "counts the gap of each chain apart from the other's" $
      inDatabaseE $ do
        -- A paid change address does not extend the receiving gap
        accId <- account ctx
        replicateM_ 20 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc accId [paidOnce (internalAddr 0) 1000]
        lift (highestUsedIndex accId AddrInternal) `dbShouldBeE` Just 0
        lift (highestUsedIndex accId AddrExternal) `dbShouldBeE` Nothing
        lift $ dbShouldFailWith receivingGap $ genExtAddress ctx cfg accId ""
        -- Nor does a paid receiving address extend the change gap, in the
        -- account of the second wallet
        (other, _) <- testNewAcc2 ctx
        _ <- genExtAddress ctx cfg other ""
        updateAddressBalances btc other [paidOnce externalAddr2 1000]
        let held = [Deriv :/ 1 :/ i | i <- [0 .. 19]]
        replicateM_ 20 $ nextChangeAddress ctx cfg other held
        lift $ dbShouldFailWith changeGap $ nextChangeAddress ctx cfg other held

    it "labels a receiving address by its index, as receive --label does" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        labelled <- setAddrLabel accId 1 "Savings"
        liftIO $ addressRow labelled `shouldBe` (1, "/0/1", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "Savings", False)
        (map dBAddressLabel <$> lift (addressesAfter accId AddrExternal Nothing Nothing)) `dbShouldBeE` ["", "Savings", ""]

    it "refuses to label an address the account has not reached" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        lift $ dbShouldFailWith "Address 3 does not exist" $ setAddrLabel accId 3 "Savings"

    it "labels receiving addresses only: the index names the external chain" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        _ <- genExtAddress ctx cfg accId ""
        _ <- setAddrLabel accId 0 "Rent"
        (map dBAddressLabel <$> lift (addressesAfter accId AddrExternal Nothing Nothing)) `dbShouldBeE` ["Rent"]
        (map dBAddressLabel <$> lift (addressesAfter accId AddrInternal Nothing Nothing)) `dbShouldBeE` [""]

    -- 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg is the receiving address /0/1
    it "labels a receiving address given by its text, as label does" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        (_, _, typed) <- addressByText cfg Nothing "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"
        labelled <- labelAddress typed "Savings"
        liftIO $ addressRow labelled `shouldBe` (1, "/0/1", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "Savings", False)
        (map dBAddressLabel <$> lift (addressesAfter accId AddrExternal Nothing Nothing)) `dbShouldBeE` ["", "Savings", ""]

    -- 17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn is the first change address
    it "refuses to label a change address given by its text" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        (_, _, typed) <- addressByText cfg Nothing "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"
        lift $ dbShouldFailWith "A change address takes no label. Only a receiving address does." $ labelAddress typed "Rent"

    it "gives a bip84 account its P2WPKH addresses, which start with bc1q, on both chains" $
      inDatabaseE $ do
        (accId, _) <- testNewSegwitAcc ctx
        first <- genExtAddress ctx cfg accId ""
        liftIO $ dBAddressAddress first `shouldBe` segwitExternalAddrsText !! 0
        change <- nextChangeAddress ctx cfg accId []
        liftIO $ dBAddressAddress change `shouldBe` segwitInternalAddrsText !! 0

    it "gives a Bitcoin Cash account addresses in the cash address format" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch ctx
        addr <- genExtAddress ctx cfg accId ""
        liftIO $ addressRow addr `shouldBe` (0, "/0/0", "bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "", False)

changeSpec :: Ctx -> Spec
changeSpec ctx =
  describe "Change addresses" $ do
    let cfg = def :: Config
    it "gives the first change address when there is none, and the same one while nothing uses it" $
      inDatabaseE $ do
        accId <- account ctx
        addr <- nextChangeAddress ctx cfg accId []
        liftIO $ addressRow addr `shouldBe` (0, "/1/0", "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn", "", True)
        nextChangeAddress ctx cfg accId [] `dbShouldBeE` addr
        nextIndices accId `dbShouldBeE` (0, 1)

    it "skips a change address a pending transaction reserves, and generates the next" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]) `dbShouldBeE` 1
        nextIndices accId `dbShouldBeE` (0, 2)

    it "never gives again a change address paid on the chain, even once its coins are spent" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc accId [paidOnce (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
        updateAddressBalances btc accId [emptied (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1

    it "counts an unconfirmed payment to a change address as using it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc accId [paidInMempool (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1

    it "fills the lowest unused address first, as a deleted transaction leaves one" $
      inDatabaseE $ do
        accId <- account ctx
        -- Three change addresses: the first is paid, the other two were
        -- reserved by transactions that are gone
        _ <- nextChangeAddress ctx cfg accId []
        _ <- nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]
        _ <- nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0, Deriv :/ 1 :/ 1]
        updateAddressBalances btc accId [paidOnce (internalAddr 0) 1000]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 2]) `dbShouldBeE` 1
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 1]) `dbShouldBeE` 2
        nextIndices accId `dbShouldBeE` (0, 3)

    it "gives back a change address whose only payment the network dropped" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc accId [paidInMempool (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
        -- The store no longer counts the transaction: as BIP44 asks, an
        -- address with no transaction on the chain is unused
        updateAddressBalances btc accId [Store.Balance (internalAddr 0) 0 0 0 0 0]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 0

    it "refuses the twenty-first change address while pending transactions hold twenty, and says so" $
      inDatabaseE $ do
        accId <- account ctx
        let held = [Deriv :/ 1 :/ i | i <- [0 .. 19]]
        -- Every call finds its predecessors reserved and generates the next
        replicateM_ 20 $ nextChangeAddress ctx cfg accId held
        lift $ dbShouldFailWith changeGap $ nextChangeAddress ctx cfg accId held
        nextIndices accId `dbShouldBeE` (0, 20)

    it "allows twenty change addresses past the highest one that was paid" $
      inDatabaseE $ do
        accId <- account ctx
        let held = [Deriv :/ 1 :/ i | i <- [0 .. 20]]
        replicateM_ 20 $ nextChangeAddress ctx cfg accId held
        updateAddressBalances btc accId [paidOnce (internalAddr 5) 1000]
        -- Addresses 6 to 25 are the twenty past it
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId held) `dbShouldBeE` 20

restoringSpec :: Ctx -> Spec
restoringSpec ctx =
  describe "Generating the addresses a scan found used" $ do
    let cfg = def :: Config
    it "generates the addresses of a chain up to an index, whatever the gap" $
      inDatabaseE $ do
        accId <- account ctx
        extendChainTo ctx accId AddrExternal 30
        extendChainTo ctx accId AddrInternal 3
        listed accId AddrExternal Nothing Nothing `dbShouldBeE` [29, 28 .. 0]
        listed accId AddrInternal Nothing Nothing `dbShouldBeE` [2, 1, 0]
        addressPath btc accId (externalAddr 9) `dbShouldBeE` Just (Deriv :/ 0 :/ 9)
        addressPath btc accId (internalAddr 2) `dbShouldBeE` Just (Deriv :/ 1 :/ 2)
        nextIndices accId `dbShouldBeE` (30, 3)

    it "leaves alone a chain that already reaches the index" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 5 $ genExtAddress ctx cfg accId ""
        extendChainTo ctx accId AddrExternal 0
        extendChainTo ctx accId AddrExternal 5
        nextIndices accId `dbShouldBeE` (5, 0)

    it "goes on receiving after the discovered addresses, within the gap" $
      inDatabaseE $ do
        accId <- account ctx
        -- A discovery found address 9 used and generated up to it
        extendChainTo ctx accId AddrExternal 10
        updateAddressBalances btc accId [paidOnce (externalAddr 9) 1000]
        (pathText . addrPath <$> genExtAddress ctx cfg accId "") `dbShouldBeE` "/0/10"
        -- The discovered addresses count against the gap like the others,
        -- here in the bip84 account of the portfolio
        (accId2, _) <- testNewSegwitAcc ctx
        extendChainTo ctx accId2 AddrExternal 25
        lift $ dbShouldFailWith receivingGap $ genExtAddress ctx cfg accId2 ""

    it "lives with a hole in a chain, as an invalid child key leaves one" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        extendChainTo ctx accId AddrInternal 3
        -- The chains moved past index 1 but nothing was stored there
        lift $ P.delete $ DBAddressKey accId False 1
        lift $ P.delete $ DBAddressKey accId True 1
        listed accId AddrExternal Nothing Nothing `dbShouldBeE` [2, 0]
        listed accId AddrInternal Nothing Nothing `dbShouldBeE` [2, 0]
        (Map.size . fst <$> allAddressesMap btc accId) `dbShouldBeE` 4
        lift $ dbShouldFailWith "Address 1 does not exist" $ setAddrLabel accId 1 "gone"
        -- The chains go on after it, and the change skips to an address that exists
        (dBAddressIndex <$> genExtAddress ctx cfg accId "") `dbShouldBeE` 3
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]) `dbShouldBeE` 2
        nextIndices accId `dbShouldBeE` (4, 3)

databaseSpec :: Ctx -> Spec
databaseSpec ctx =
  describe "Addresses in the database" $ do
    let cfg = def :: Config
    it "stores the balances the store reports, address by address" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        updateAddressBalances btc accId [Store.Balance (externalAddr 0) 5000 300 2 3 9000]
        -- An address the wallet does not have is passed over
        updateAddressBalances btc accId [paidOnce (otherAddr 0) 1000]
        rows <- lift $ addressesAfter accId AddrExternal Nothing Nothing
        liftIO $ map balanceOf rows `shouldBe` [AddressBalance 0 0 0 0 0, AddressBalance 5000 300 2 3 9000]

    it "sums the balances of the addresses into the account's" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc accId [Store.Balance (externalAddr 1) 5000 0 1 1 5000, Store.Balance (internalAddr 0) 2000 700 2 2 2700]
        acc <- lift $ updateAccountBalances accId
        liftIO $ (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc) `shouldBe` (7000, 700, 3)

    -- Five addresses asked two at a time take three queries: the account
    -- has the first three, and the last two are another wallet's. The
    -- order of the rows is the query's, so they are compared sorted.
    it "finds the addresses of an account among more than one query holds, a few at a time" $
      inDatabaseE $ do
        held <- testNewAcc ctx
        replicateM_ 3 $ genExtAddress ctx cfg (fst held) ""
        found <- lift $ storedAmongBy 2 [externalAddr 0, externalAddr 1, externalAddr 2, otherAddr 0, otherAddr 1] held
        liftIO $
          sort (map dBAddressAddress found)
            `shouldBe` ["1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "1Dg6Kg7kQuyiZz41HRWXKUWKRu6ZyEf1Nr", "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    -- SQLite takes at most 32766 values in a query, and the first sync
    -- of a large pool asks about more addresses. The account is one value
    -- of the query too.
    it "puts no more addresses in one query than SQLite takes, with the account" $
      (inListBatch + 1) `shouldSatisfy` (<= 32766)

    it "finds an address of the wallet by its text, with its account and the portfolio of that account" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId ""
        change <- nextChangeAddress ctx cfg accId []
        (portfolio, holder, found) <- addressByText cfg Nothing "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"
        liftIO $ do
          found `shouldBe` change
          holder `shouldBe` accId
          portfolioRef portfolio `shouldBe` PortfolioRef 0 0

    it "knows no address the wallet does not have, and names it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId ""
        lift $
          dbShouldFailWith "The wallet has no address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU" $
            addressByText cfg Nothing "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u is the first
    -- receiving address of the account of portfolio 0 of the test wallet
    -- on Bitcoin Cash, as hw stores it. 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the
    -- same address in the legacy form.
    it "finds an address of the wallet written without its prefix, or in the legacy form" $
      inDatabaseE $ do
        (cashId, _) <- testNewAccBch ctx
        stored <- genExtAddress ctx cfg cashId ""
        forM_ ["qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u", "1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ"] $ \written -> do
          (_, holder, found) <- addressByText cfg Nothing written
          liftIO $ do
            found `shouldBe` stored
            holder `shouldBe` cashId

    -- externalAddr2Text is the first receiving address of the second test
    -- wallet; 1JCq8Aa9 is no address of the wallet. The accounts come in
    -- the order of their wallets.
    it "finds the accounts of addresses by their text, each once, none for an address it does not have" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx
        _ <- genExtAddress ctx cfg accId ""
        _ <- genExtAddress ctx cfg otherId ""
        holders [externalAddr2Text, "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", externalAddr2Text]
          `dbShouldBeE` [accId, otherId]

    -- bitcoincash:qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u is the first
    -- receiving address of the account of portfolio 0 of the test wallet
    -- on Bitcoin Cash, as hw stores it. 1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ is the
    -- same address in the legacy form.
    it "finds the account of a Bitcoin Cash address written without its prefix, or in the legacy form" $
      inDatabaseE $ do
        (cashId, _) <- testNewAccBch ctx
        _ <- genExtAddress ctx cfg cashId ""
        holders ["qzvzk6v4gdxt705vcmt3nxwwl7j3pfzercknlz3k4u"] `dbShouldBeE` [cashId]
        holders ["1EsbiWQZvECGZzzRFgH38ZsQTyx5TRAZXQ"] `dbShouldBeE` [cashId]

    -- 1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj is the first receiving address of
    -- the account on Bitcoin, and
    -- bitcoincash:qryq64fz8ch3qcfk8twj57crv0pgt0ef353f374c3t the same
    -- address on Bitcoin Cash, which Bitcoin does not read. The account of
    -- the portfolio on Bitcoin Cash reads it, and has other keys.
    it "reads an address on the network of each account: a Bitcoin account has no Bitcoin Cash address" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId ""
        holders ["bitcoincash:qryq64fz8ch3qcfk8twj57crv0pgt0ef353f374c3t"] `dbShouldBeE` []

    it "keeps those of the given addresses that have transactions" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        updateAddressBalances btc accId [paidOnce (externalAddr 0) 1000]
        lift (usedAddresses [accId] ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"])
          `dbShouldBeE` ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    it "lists the addresses of a chain newest first, after a given one when asked" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        listed accId AddrExternal Nothing Nothing `dbShouldBeE` [2, 1, 0]
        listed accId AddrExternal Nothing (Just 2) `dbShouldBeE` [2, 1]
        listed accId AddrExternal (Just 1) (Just 2) `dbShouldBeE` [0]
        listed accId AddrExternal (Just 0) Nothing `dbShouldBeE` []
        listed accId AddrInternal Nothing Nothing `dbShouldBeE` [0]

    it "names the row of an address of the account on its chain, for --after, and refuses one of the other chain" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        (dBAddressIndex <$> listedAddress btc accId AddrExternal "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg") `dbShouldBeE` 1
        lift $
          dbShouldFailWith "The address 1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg is not a change address of the account" $
            listedAddress btc accId AddrInternal "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg"
        lift $
          dbShouldFailWith "The address 1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU is not a receiving address of the account" $
            listedAddress btc accId AddrExternal "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"
        lift $
          dbShouldFailWith "The address nothing is not valid on btc" $
            listedAddress btc accId AddrExternal "nothing"

    it "maps every address of the account to its path and its balance" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc accId [paidOnce (externalAddr 1) 5000]
        (paths, balances) <- allAddressesMap btc accId
        liftIO $ do
          paths `shouldBe` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0), (externalAddr 1, Deriv :/ 0 :/ 1), (internalAddr 0, Deriv :/ 1 :/ 0)]
          balances
            `shouldBe` Map.fromList
              [ (externalAddr 0, AddressBalance 0 0 0 0 0),
                (externalAddr 1, AddressBalance 5000 0 1 1 5000),
                (internalAddr 0, AddressBalance 0 0 0 0 0)
              ]

    it "gives the path of an address of the account, and nothing for another account's or an outsider's" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx
        _ <- genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        _ <- genExtAddress ctx cfg otherId ""
        addressPath btc accId (externalAddr 0) `dbShouldBeE` Just (Deriv :/ 0 :/ 0)
        addressPath btc accId (internalAddr 0) `dbShouldBeE` Just (Deriv :/ 1 :/ 0)
        addressPath btc accId externalAddr2 `dbShouldBeE` Nothing
        addressPath btc accId (otherAddr 0) `dbShouldBeE` Nothing
        listed accId AddrExternal Nothing Nothing `dbShouldBeE` [0]

{- The addresses of a pool -}

-- The pool is what portfolio 0 of the test wallet holds on Bitcoin: its
-- bip44 account, with the receiving address /0/0 and the change address
-- /1/0, and its bip84 account, with the receiving address /0/0
poolSpec :: Ctx -> Spec
poolSpec ctx =
  describe "The addresses of a pool" $ do
    let cfg = def :: Config
        bothAccounts = do
          portfolio <- testPortfolio ctx
          (legacyId, _) <- testNewAcc ctx
          (segwitId, _) <- testNewSegwitAcc ctx
          _ <- genExtAddress ctx cfg legacyId "Rent"
          _ <- nextChangeAddress ctx cfg legacyId []
          _ <- genExtAddress ctx cfg segwitId "Salary"
          return (portfolioPool portfolio btc, legacyId, segwitId)
    it "are those of both of its accounts, each with the type of its account and its path there" $
      inDatabaseE $ do
        (pool, _, _) <- bothAccounts
        (fst <$> poolAddressesMap pool)
          `dbShouldBeE` Map.fromList
            [ (externalAddr 0, PoolPath Bip44 (Deriv :/ 0 :/ 0)),
              (internalAddr 0, PoolPath Bip44 (Deriv :/ 1 :/ 0)),
              (segwitExternalAddr 0, PoolPath Bip84 (Deriv :/ 0 :/ 0))
            ]

    it "come with the balance of each, whichever account has it" $
      inDatabaseE $ do
        (pool, legacyId, segwitId) <- bothAccounts
        updateAddressBalances btc legacyId [paidOnce (externalAddr 0) 5000]
        updateAddressBalances btc segwitId [paidOnce (segwitExternalAddr 0) 7000]
        (snd <$> poolAddressesMap pool)
          `dbShouldBeE` Map.fromList
            [ (externalAddr 0, AddressBalance 5000 0 1 1 5000),
              (internalAddr 0, AddressBalance 0 0 0 0 0),
              (segwitExternalAddr 0, AddressBalance 7000 0 1 1 7000)
            ]

    it "are found one by one with the account that has each, and none for an outsider's" $
      inDatabaseE $ do
        (pool, legacyId, segwitId) <- bothAccounts
        poolAddressPath pool (internalAddr 0) `dbShouldBeE` Just (legacyId, PoolPath Bip44 (Deriv :/ 1 :/ 0))
        poolAddressPath pool (segwitExternalAddr 0) `dbShouldBeE` Just (segwitId, PoolPath Bip84 (Deriv :/ 0 :/ 0))
        poolAddressPath pool (otherAddr 0) `dbShouldBeE` Nothing

    it "are kept among given addresses with their paths in the pool, once each" $
      inDatabaseE $ do
        (pool, _, _) <- bothAccounts
        poolPaths pool [externalAddr 0, otherAddr 0, externalAddr 0, segwitExternalAddr 0, internalAddr 0]
          `dbShouldBeE` Map.fromList
            [ (externalAddr 0, legacyPath (Deriv :/ 0 :/ 0)),
              (internalAddr 0, legacyPath (Deriv :/ 1 :/ 0)),
              (segwitExternalAddr 0, PoolPath Bip84 (Deriv :/ 0 :/ 0))
            ]

    it "give their rows, with their labels, whichever account has them" $
      inDatabaseE $ do
        (pool, _, _) <- bothAccounts
        (fmap dBAddressLabel <$> lift (poolAddressRow pool (externalAddr 0))) `dbShouldBeE` Just "Rent"
        (fmap dBAddressLabel <$> lift (poolAddressRow pool (segwitExternalAddr 0))) `dbShouldBeE` Just "Salary"
        lift (poolAddressRow pool (otherAddr 0)) `dbShouldBeE` Nothing

    it "tell an address of the pool from any other" $
      inDatabaseE $ do
        (pool, _, _) <- bothAccounts
        lift (inPool pool (segwitExternalAddr 0)) `dbShouldBeE` True
        lift (inPool pool (otherAddr 0)) `dbShouldBeE` False

    -- The first receiving address of the second test wallet, and that of
    -- portfolio 1 of the test wallet, are in the database, in other pools
    it "are those of its portfolio alone: not another wallet's, nor another portfolio's of the wallet" $
      inDatabaseE $ do
        (pool, _, _) <- bothAccounts
        (otherId, _) <- testNewAcc2 ctx
        _ <- genExtAddress ctx cfg otherId ""
        second <- nextPortfolioOf ctx testMnemonic
        (secondId, _) <- accountOf second btc Bip44
        _ <- genExtAddress ctx cfg secondId ""
        lift (inPool pool externalAddr2) `dbShouldBeE` False
        lift (inPool pool account1Addr) `dbShouldBeE` False
        lift (inPool (portfolioPool second btc) account1Addr) `dbShouldBeE` True

    -- The bip44 account of the portfolio on bch has other keys
    it "are those of its network alone" $
      inDatabaseE $ do
        (pool, _, _) <- bothAccounts
        portfolio <- testPortfolio ctx
        lift (inPool (portfolioPool portfolio bch) (externalAddr 0)) `dbShouldBeE` False
        (Map.size . fst <$> poolAddressesMap (portfolioPool portfolio bch)) `dbShouldBeE` 0
        (Map.size . fst <$> poolAddressesMap pool) `dbShouldBeE` 3

{- An address the user types -}

-- btctest and btctest4 share the coin type 1, so the bip44 accounts of a
-- portfolio on them have the same key. The receiving address /0/0 of
-- both is mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih, generated on each.
typedSpec :: Ctx -> Spec
typedSpec ctx =
  describe "An address the user types that two test networks share" $ do
    -- hw shows every network here, but for the tests of the hidden ones
    let cfg = showingEveryNetwork def
        onBothTestnets = do
          portfolio <- testPortfolio ctx
          (testnet3Id, _) <- accountOf portfolio btcTest Bip44
          (testnet4Id, _) <- accountOf portfolio btcTest4 Bip44
          first <- genExtAddress ctx cfg testnet3Id ""
          second <- genExtAddress ctx cfg testnet4Id ""
          return ((testnet3Id, first), (testnet4Id, second))
        rowOf (_, accId, row) = (accId, row)
    it "is the same text on both, in a row of each account" $
      inDatabaseE $ do
        ((testnet3Id, first), (testnet4Id, second)) <- onBothTestnets
        liftIO $ do
          dBAddressAddress first `shouldBe` "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"
          dBAddressAddress second `shouldBe` "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"
        (map rowOf <$> lift (findAddressByText "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih")) `dbShouldBeE` [(testnet3Id, first), (testnet4Id, second)]

    -- A recipient typed on a command is read on the network of each
    -- account
    it "is held by the accounts of both networks" $
      inDatabaseE $ do
        ((testnet3Id, _), (testnet4Id, _)) <- onBothTestnets
        holders ["mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"] `dbShouldBeE` [testnet3Id, testnet4Id]

    it "names the row on the --network" $
      inDatabaseE $ do
        (_, onTestnet4) <- onBothTestnets
        (rowOf <$> addressByText cfg (Just btcTest4) "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih") `dbShouldBeE` onTestnet4

    it "names the row on the default network without --network" $
      inDatabaseE $ do
        (onTestnet3, _) <- onBothTestnets
        (rowOf <$> addressByText cfg {configDefaultNetwork = btcTest} Nothing "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih")
          `dbShouldBeE` onTestnet3

    it "takes --network over the default network" $
      inDatabaseE $ do
        (_, onTestnet4) <- onBothTestnets
        (rowOf <$> addressByText cfg {configDefaultNetwork = btcTest} (Just btcTest4) "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih")
          `dbShouldBeE` onTestnet4

    -- The default network is btc, where the address is not
    it "is refused without --network when the default network has no row of it, naming the networks that have" $
      inDatabaseE $ do
        _ <- onBothTestnets
        lift $
          dbShouldFailWith "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4: give the --network" $
            addressByText cfg Nothing "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"

    it "is refused on a --network that has no row of it, naming the networks that have" $
      inDatabaseE $ do
        _ <- onBothTestnets
        lift $
          dbShouldFailWith "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, not on the --network btcreg" $
            addressByText cfg (Just btcRegTest) "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"

    -- Generated on btctest alone: the default network, btc, does not matter
    it "names its one row without --network when one network alone has it" $
      inDatabaseE $ do
        portfolio <- testPortfolio ctx
        (testnet3Id, _) <- accountOf portfolio btcTest Bip44
        row <- genExtAddress ctx cfg testnet3Id ""
        (rowOf <$> addressByText cfg Nothing "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih") `dbShouldBeE` (testnet3Id, row)

    -- hw shows btc, bch and btctest4: btctest, which has the address
    -- too, is hidden, and is no candidate. The default network is btc.
    it "names its row on the one network hw shows that has it, without --network, though a hidden network has it too" $
      inDatabaseE $ do
        (_, onTestnet4) <- onBothTestnets
        (rowOf <$> addressByText def {configShownNetworks = [btc, bch, btcTest4]} Nothing "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih")
          `dbShouldBeE` onTestnet4

    -- The default configuration shows btc and bch
    it "is refused when the networks that have it are all hidden, naming them, with the command that shows one" $
      inDatabaseE $ do
        _ <- onBothTestnets
        lift $
          dbShouldFailWith "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest and btctest4, which are hidden. To show one of them, run for instance: hw setdefault --show-network btctest" $
            addressByText def Nothing "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"

    -- addresses --after and balances --after: each section of the listing
    -- is given with its network and the row the address has in it
    it "starts a listing after it in the one section that holds it" $
      sectionOfAddress "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih" [(btc, Nothing), (btcTest, Just "row on btctest"), (btcRegTest, Nothing)]
        `shouldBe` Right (Just (1, "row on btctest" :: Text))

    -- Starting in the first section that holds it would loop: the last row
    -- of the next page would be the address again
    it "is refused as the start of a listing when several of its sections hold it, naming their networks" $
      sectionOfAddress "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih" [(btcTest, Just "a"), (btcTest4, Just "b"), (btcRegTest, Just "c")]
        `shouldBe` (Left "The address mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih is on btctest, btctest4 and btcreg: give the --network" :: Either String (Maybe (Int, Text)))

    it "gives no section to start a listing in when no section holds it" $
      sectionOfAddress "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih" [(btcTest, Nothing), (btcTest4, Nothing)]
        `shouldBe` (Right Nothing :: Either String (Maybe (Int, Text)))

    it "is labelled on the network named alone" $
      inDatabaseE $ do
        ((testnet3Id, _), (testnet4Id, _)) <- onBothTestnets
        (_, _, typed) <- addressByText cfg (Just btcTest4) "mhHhjvhZaqHfhghiCcyHsWmE9GjGNQoMih"
        _ <- labelAddress typed "Faucet"
        (map dBAddressLabel <$> lift (addressesAfter testnet4Id AddrExternal Nothing Nothing)) `dbShouldBeE` ["Faucet"]
        (map dBAddressLabel <$> lift (addressesAfter testnet3Id AddrExternal Nothing Nothing)) `dbShouldBeE` [""]

pathSpec :: Spec
pathSpec =
  describe "Derivation paths" $ do
    it "tells the change paths from the receiving ones" $ do
      isIntPath (Deriv :/ 1 :/ 5) `shouldBe` True
      isIntPath (Deriv :/ 0 :/ 5) `shouldBe` False
      isIntPath (Deriv :/ 1) `shouldBe` False
      isIntPath (Deriv :/ 1 :/ 2 :/ 3) `shouldBe` False

    it "writes a path as JSON and the signing data carry it" $
      pathText (Deriv :/ 0 :/ 7) `shouldBe` "/0/7"

    it "goes from a chain and an index to the path and back, for the two chains only" $ do
      chainPath False 7 `shouldBe` Deriv :/ 0 :/ 7
      chainPath True 3 `shouldBe` Deriv :/ 1 :/ 3
      chainIndex (Deriv :/ 0 :/ 7) `shouldBe` Just (AddrExternal, 7)
      chainIndex (Deriv :/ 1 :/ 3) `shouldBe` Just (AddrInternal, 3)
      chainIndex (Deriv :/ 2 :/ 3) `shouldBe` Nothing
      chainIndex (Deriv :/ 0 :/ 3 :/ 1) `shouldBe` Nothing

    it "reads the path of an address as the user types it, on either chain, up to the last index before the hardened ones" $ do
      readChainPath "/0/3" `shouldBe` Right (AddrExternal, 3)
      readChainPath "/1/2" `shouldBe` Right (AddrInternal, 2)
      readChainPath "/0/0" `shouldBe` Right (AddrExternal, 0)
      -- 2^31 - 1: BIP32 keeps 2^31 and above for hardened steps
      readChainPath "/0/2147483647" `shouldBe` Right (AddrExternal, 2147483647)

    -- A hardened step, one step or three, a third chain, no leading slash,
    -- an index of 2^31 or more, and 4294967296, which is 0 in 32 bits
    it "refuses every other path, naming the two forms" $
      forM_ ["/2/3", "/0/3'", "/0", "/0/3/1", "0/3", "m/0/3", "M/0/3", "/0/-1", "/0/2147483648", "/0/4294967296", "/0/", "/00/3", "/0/3 ", "", "abc"] $ \typed ->
        readChainPath typed `shouldBe` Left ("The path " <> cs typed <> " is not that of an address: /0/i is the receiving address i, and /1/i the change address i")

    it "reads a path that starts with the type of its account, as the views write it, in any case" $ do
      readTypedChainPath "bip84/0/3" `shouldBe` Just (Right (Just Bip84, AddrExternal, 3))
      readTypedChainPath "bip44/1/2" `shouldBe` Just (Right (Just Bip44, AddrInternal, 2))
      readTypedChainPath "BIP84/0/3" `shouldBe` Just (Right (Just Bip84, AddrExternal, 3))

    it "reads a path without a type, and leaves the type to the command" $
      readTypedChainPath "/0/3" `shouldBe` Just (Right (Nothing, AddrExternal, 3))

    it "refuses a path of a type hw holds no account of, naming the types it holds" $
      readTypedChainPath "bip49/0/3" `shouldBe` Just (Left "The path bip49/0/3 is of no type of account hw holds: a path starts with bip44 or bip84, such as bip84/0/3, or with no type, such as /0/3")

    it "refuses a path after a type that is not that of an address, naming the path as typed" $
      readTypedChainPath "bip84/0/3'" `shouldBe` Just (Left "The path bip84/0/3' is not that of an address: /0/i is the receiving address i, and /1/i the change address i")

    -- A letter in front of a path is read as a type only in the form bipN
    -- and right before the first slash. Every other word names something
    -- else, such as a file.
    it "takes no other word for a path" $
      forM_ ["unsigned.psbt", "./bip84/0/3", "bip84", "bip/0/3", "bip84x/0/3", "bip84-0/3"] $ \typed ->
        readTypedChainPath typed `shouldBe` Nothing

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 is P2PKH, and
    -- bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc P2WPKH
    it "tells the type of the account of a row by the form of its address" $ do
      addrPoolPath btc fixtureRow `shouldBe` Just (PoolPath Bip44 (Deriv :/ 0 :/ 3))
      addrPoolPath btc fixtureRow {dBAddressAddress = "bc1qynnywxcvs9dwuqk8h53xzn9kpeyprgqeacxwtc"} `shouldBe` Just (PoolPath Bip84 (Deriv :/ 0 :/ 3))

    it "reads the path of a row from its chain and index" $ do
      addrPath fixtureRow `shouldBe` Deriv :/ 0 :/ 3
      addrPath fixtureRow {dBAddressInternal = True} `shouldBe` Deriv :/ 1 :/ 3
      fromDBAddr btc fixtureRow `shouldBe` Right (externalAddr 3, Deriv :/ 0 :/ 3)
      fromDBAddr btc fixtureRow {dBAddressAddress = "nonsense"} `shouldBe` Left "The address nonsense is not valid on btc"

derivingSpec :: Ctx -> Spec
derivingSpec ctx =
  describe "Deriving addresses from the account key" $ do
    -- An invalid child key, which BIP32 says to skip, has odds of one in
    -- 2^127 per index and no known example, so that path is covered by
    -- the hole test above and by reasoning alone
    it "derives the address of a chain at an index" $ do
      deriveChainAddress ctx Bip44 (testAccountPub ctx) extDeriv 9 `shouldReturn` Just (externalAddr 9)
      deriveChainAddress ctx Bip44 (testAccountPub ctx) intDeriv 0 `shouldReturn` Just (internalAddr 0)

    it "lists a window of a chain with the indices" $ do
      chainAddresses ctx Bip44 (testAccountPub ctx) extDeriv 2 3
        `shouldReturn` [(externalAddr 2, 2), (externalAddr 3, 3), (externalAddr 4, 4)]
      chainAddresses ctx Bip44 (testAccountPub ctx) intDeriv 5 0 `shouldReturn` []

    -- The first ten addresses of each chain of the bip84 account, derived
    -- apart from hw with the Python tool of the message-signing vectors
    it "derives the P2WPKH addresses of a bip84 account from its key, on both chains" $ do
      deriveChainAddress ctx Bip84 (testSegwitAccountPub ctx) extDeriv 0 `shouldReturn` Just (segwitExternalAddr 0)
      chainAddresses ctx Bip84 (testSegwitAccountPub ctx) extDeriv 0 10 `shouldReturn` zip (segwitExternalAddr <$> [0 .. 9]) [0 .. 9]
      chainAddresses ctx Bip84 (testSegwitAccountPub ctx) intDeriv 0 10 `shouldReturn` zip (segwitInternalAddr <$> [0 .. 9]) [0 .. 9]

    -- The first five addresses of each chain of the bip49 and bip86
    -- accounts 0 of the test wallet, derived apart from hw with the same
    -- Python tool
    it "derives the addresses of an account of a kind hw does not hold, given how its keys are addressed" $ do
      chainAddressesBy (foreignAddress Bip49 btc ctx) ctx (testWrappedAccountPub ctx) extDeriv 0 5 `shouldReturn` zip (wrappedExternalAddr <$> [0 .. 4]) [0 .. 4]
      chainAddressesBy (foreignAddress Bip49 btc ctx) ctx (testWrappedAccountPub ctx) intDeriv 0 5 `shouldReturn` zip (wrappedInternalAddr <$> [0 .. 4]) [0 .. 4]
      chainAddressesBy (foreignAddress Bip86 btc ctx) ctx (testTaprootAccountPub ctx) extDeriv 0 5 `shouldReturn` zip (taprootExternalAddr <$> [0 .. 4]) [0 .. 4]
      chainAddressesBy (foreignAddress Bip86 btc ctx) ctx (testTaprootAccountPub ctx) intDeriv 0 5 `shouldReturn` zip (taprootInternalAddr <$> [0 .. 4]) [0 .. 4]

jsonSpec :: Spec
jsonSpec =
  describe "Addresses in JSON" $ do
    it "prints an address as it stores it, and reads it back" $ do
      let json :: String
          json =
            [r|{
              "account": 1,
              "index": 3,
              "derivation": "/0/3",
              "address": "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
              "label": "Savings",
              "balance": {"confirmed": 5000, "unconfirmed": 300, "coins": 2, "txs": 3, "received": 9000},
              "internal": false,
              "created": "2023-11-01T00:00:00Z"
            }|]
      eitherDecode (cs json) `shouldBe` Right (toJSON fixtureRow)
      eitherDecode (cs json) `shouldBe` Right fixtureRow

    -- 1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1 is P2PKH: an address of a bip44
    -- account
    it "prints an address with the type of its account and the balance it can spend among its balances, as hw addresses does, and reads it back" $ do
      let json :: String
          json =
            [r|{
              "account": 1,
              "index": 3,
              "type": "bip44",
              "derivation": "/0/3",
              "address": "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
              "label": "Savings",
              "balance": {"confirmed": 5000, "unconfirmed": 300, "coins": 2, "txs": 3, "received": 9000, "spendable": 4800},
              "internal": false,
              "created": "2023-11-01T00:00:00Z"
            }|]
      eitherDecode (cs json) `shouldBe` Right (addressWithSpendable btc fixtureRow 4800)
      (parseEither parseAddressWithSpendable =<< eitherDecode (cs json)) `shouldBe` Right (fixtureRow, 4800)

    -- The two coins hold 5300: the 5000 confirmed can be spent, the 300
    -- unconfirmed cannot yet, and nothing locks them
    it "prints an address with the type of its account and the balance of its coins among its balances, as balances does, and reads it back" $ do
      let json :: String
          json =
            [r|{
              "account": 1,
              "index": 3,
              "type": "bip44",
              "derivation": "/0/3",
              "address": "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
              "label": "Savings",
              "balance": {"confirmed": 5000, "unconfirmed": 300, "coins": 2, "txs": 3, "received": 9000, "total": 5300, "spendable": 5000, "unspendable": 300, "lockedbytx": 0, "lockedbyuser": 0, "notspendableyet": 300},
              "internal": false,
              "created": "2023-11-01T00:00:00Z"
            }|]
      eitherDecode (cs json) `shouldBe` Right (addressWithCoinBalance btc fixtureRow (CoinBalance 5300 5000 0 0))
      (parseEither parseAddressWithCoinBalance =<< eitherDecode (cs json)) `shouldBe` Right (fixtureRow, CoinBalance 5300 5000 0 0)
