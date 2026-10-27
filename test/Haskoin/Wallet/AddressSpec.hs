{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The addresses of an account: the receiving addresses that `receive`
-- gives out and the change addresses the wallet pays itself at, generated
-- along the two chains of the account within the gap of BIP44, with their
-- labels, balances and derivation paths. The account of these tests is
-- account 0 of the test wallet on Bitcoin, whose first ten addresses of
-- each chain are written out in "Haskoin.Wallet.TestUtils".
module Haskoin.Wallet.AddressSpec (spec) where

import Control.Monad (replicateM_)
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans (lift, liftIO)
import Data.Aeson (eitherDecode, toJSON)
import Data.Aeson.Types (parseEither)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)
import Haskoin
import qualified Haskoin.Store.Data as Store
import Haskoin.Wallet.Account
import Haskoin.Wallet.Address
import Haskoin.Wallet.Config
import Haskoin.Wallet.Database
import Haskoin.Wallet.TestUtils
import Haskoin.Wallet.Util (Page (Page))
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
  pathSpec
  derivingSpec ctx
  jsonSpec
  -- An address and its balance read back as they were written
  testIdentity def {jsonTests = [JsonBox (arbitraryDBAddress btc), JsonBox arbitraryAddressBalance]}

-- | The account of these tests
account :: Ctx -> ExceptT String (DB IO) DBAccountId
account ctx = fst <$> testNewAcc ctx "main"

-- | The next index of each chain of the account: external, then internal
nextIndices :: DBAccountId -> ExceptT String (DB IO) (Int, Int)
nextIndices accId = do
  acc <- getAccountById accId
  return (dBAccountExternal acc, dBAccountInternal acc)

-- | The indices of a page of a chain of the account, as addrs lists them
listed :: DBAccountId -> AddrType -> Page -> ExceptT String (DB IO) [Int]
listed accId addrType page = map dBAddressIndex <$> lift (addressPage accId addrType page)

-- | The index, derivation path, text, label and chain of an address
addressRow :: DBAddress -> (Int, Text, Text, Text, Bool)
addressRow a = (dBAddressIndex a, pathText (addrPath a), dBAddressAddress a, dBAddressLabel a, dBAddressInternal a)

balanceOf :: DBAddress -> AddressBalance
balanceOf a =
  AddressBalance (dBAddressBalanceConfirmed a) (dBAddressBalanceUnconfirmed a) (dBAddressBalanceCoins a) (dBAddressBalanceTxs a) (dBAddressBalanceReceived a)

-- | The row of the receiving address /0/3 of the account, labelled, paid
-- three times for 9000 in all, with 5000 confirmed and 300 unconfirmed
-- left in two coins
fixtureRow :: DBAddress
fixtureRow = DBAddress (toSqlKey 1) False 3 "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1" "Savings" 5000 300 2 3 9000 fixtureTime

receivingGap, changeGap :: String
receivingGap = "The last 20 receiving addresses await a payment: reuse one of them, listed by addrs"
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
        updateAddressBalances btc [paidOnce (externalAddr 2) 5000, paidOnce (externalAddr 4) 1000]
        lift (highestUsedIndex accId AddrExternal) `dbShouldBeE` Just 4
        -- Addresses 5 to 24 are the twenty unused ones past it
        replicateM_ 5 $ genExtAddress ctx cfg accId ""
        lift $ dbShouldFailWith receivingGap $ genExtAddress ctx cfg accId ""
        nextIndices accId `dbShouldBeE` (25, 0)

    it "counts an address whose coins were all spent as paid" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 20 $ genExtAddress ctx cfg accId ""
        updateAddressBalances btc [emptied (externalAddr 9) 5000]
        lift (highestUsedIndex accId AddrExternal) `dbShouldBeE` Just 9
        (dBAddressIndex <$> genExtAddress ctx cfg accId "") `dbShouldBeE` 20

    it "counts the gap of each chain apart from the other's" $
      inDatabaseE $ do
        -- A paid change address does not extend the receiving gap
        accId <- account ctx
        replicateM_ 20 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc [paidOnce (internalAddr 0) 1000]
        lift (highestUsedIndex accId AddrInternal) `dbShouldBeE` Just 0
        lift (highestUsedIndex accId AddrExternal) `dbShouldBeE` Nothing
        lift $ dbShouldFailWith receivingGap $ genExtAddress ctx cfg accId ""
        -- Nor does a paid receiving address extend the change gap, in the
        -- account of the second wallet
        (other, _) <- testNewAcc2 ctx "other"
        _ <- genExtAddress ctx cfg other ""
        updateAddressBalances btc [paidOnce externalAddr2 1000]
        let held = [Deriv :/ 1 :/ i | i <- [0 .. 19]]
        replicateM_ 20 $ nextChangeAddress ctx cfg other held
        lift $ dbShouldFailWith changeGap $ nextChangeAddress ctx cfg other held

    it "labels a receiving address by its index, as label does" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        labelled <- setAddrLabel accId 1 "Savings"
        liftIO $ addressRow labelled `shouldBe` (1, "/0/1", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "Savings", False)
        (map dBAddressLabel <$> lift (addressPage accId AddrExternal (Page 5 0))) `dbShouldBeE` ["", "Savings", ""]

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
        (map dBAddressLabel <$> lift (addressPage accId AddrExternal (Page 5 0))) `dbShouldBeE` ["Rent"]
        (map dBAddressLabel <$> lift (addressPage accId AddrInternal (Page 5 0))) `dbShouldBeE` [""]

    it "gives a Bitcoin Cash account addresses in the cash address format" $
      inDatabaseE $ do
        (accId, _) <- testNewAccBch ctx "cash"
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
        updateAddressBalances btc [paidOnce (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
        updateAddressBalances btc [emptied (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1

    it "counts an unconfirmed payment to a change address as using it" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc [paidInMempool (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1

    it "fills the lowest unused address first, as a deleted transaction leaves one" $
      inDatabaseE $ do
        accId <- account ctx
        -- Three change addresses: the first is paid, the other two were
        -- reserved by transactions that are gone
        _ <- nextChangeAddress ctx cfg accId []
        _ <- nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0]
        _ <- nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 0, Deriv :/ 1 :/ 1]
        updateAddressBalances btc [paidOnce (internalAddr 0) 1000]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 2]) `dbShouldBeE` 1
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId [Deriv :/ 1 :/ 1]) `dbShouldBeE` 2
        nextIndices accId `dbShouldBeE` (0, 3)

    it "gives back a change address whose only payment the network dropped" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc [paidInMempool (internalAddr 0) 11774]
        (dBAddressIndex <$> nextChangeAddress ctx cfg accId []) `dbShouldBeE` 1
        -- The store no longer counts the transaction: as BIP44 asks, an
        -- address with no transaction on the chain is unused
        updateAddressBalances btc [Store.Balance (internalAddr 0) 0 0 0 0 0]
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
        updateAddressBalances btc [paidOnce (internalAddr 5) 1000]
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
        listed accId AddrExternal (Page 100 0) `dbShouldBeE` [29, 28 .. 0]
        listed accId AddrInternal (Page 100 0) `dbShouldBeE` [2, 1, 0]
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
        updateAddressBalances btc [paidOnce (externalAddr 9) 1000]
        (pathText . addrPath <$> genExtAddress ctx cfg accId "") `dbShouldBeE` "/0/10"
        -- The discovered addresses count against the gap like the others
        (accId2, _) <- testNewAcc ctx "second"
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
        listed accId AddrExternal (Page 100 0) `dbShouldBeE` [2, 0]
        listed accId AddrInternal (Page 100 0) `dbShouldBeE` [2, 0]
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
        updateAddressBalances btc [Store.Balance (externalAddr 0) 5000 300 2 3 9000]
        -- An address the wallet does not have is passed over
        updateAddressBalances btc [paidOnce (otherAddr 0) 1000]
        rows <- lift $ addressPage accId AddrExternal (Page 5 0)
        liftIO $ map balanceOf rows `shouldBe` [AddressBalance 0 0 0 0 0, AddressBalance 5000 300 2 3 9000]

    it "sums the balances of the addresses into the account's" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc [Store.Balance (externalAddr 1) 5000 0 1 1 5000, Store.Balance (internalAddr 0) 2000 700 2 2 2700]
        acc <- lift $ updateAccountBalances accId
        liftIO $ (dBAccountBalanceConfirmed acc, dBAccountBalanceUnconfirmed acc, dBAccountBalanceCoins acc) `shouldBe` (7000, 700, 3)

    it "finds an address of the wallet by its text, with its account" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId ""
        change <- nextChangeAddress ctx cfg accId []
        (acc, found) <- getAddressByText "17KiDLpE3r92gWR8kFGkYDtgHqEVJrznvn"
        liftIO $ do
          found `shouldBe` change
          dBAccountName acc `shouldBe` "main"
        lift $ dbShouldFailWith "Address does not exist" $ getAddressByText "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"

    -- externalAddr2Text is the first receiving address of the second test
    -- wallet; 1JCq8Aa9 is no address of the wallet
    it "finds the accounts of addresses by their text, each once, none for an address it does not have" $
      inDatabaseE $ do
        accId <- account ctx
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- genExtAddress ctx cfg accId ""
        _ <- genExtAddress ctx cfg otherId ""
        (map fst <$> addressAccounts [externalAddr2Text, "1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU", externalAddr2Text])
          `dbShouldBeE` [otherId, accId]

    it "keeps those of the given addresses that have transactions" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        updateAddressBalances btc [paidOnce (externalAddr 0) 1000]
        lift (usedAddresses ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj", "1AVj9WSYayTwUd8rS1mTTo4A6CPsS83VTg", "1JCq8Aa9d9rg4T4XV93RV3DMxd5u7GkSSU"])
          `dbShouldBeE` ["1KEn7jEXa7KCLeZy59dka5qRJBLnPMmrLj"]

    it "lists the addresses of a chain newest first, a page at a time" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 3 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        listed accId AddrExternal (Page 5 0) `dbShouldBeE` [2, 1, 0]
        listed accId AddrExternal (Page 2 0) `dbShouldBeE` [2, 1]
        listed accId AddrExternal (Page 2 2) `dbShouldBeE` [0]
        listed accId AddrExternal (Page 2 3) `dbShouldBeE` []
        listed accId AddrInternal (Page 5 0) `dbShouldBeE` [0]

    it "maps every address of the account to its path and its balance" $
      inDatabaseE $ do
        accId <- account ctx
        replicateM_ 2 $ genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        updateAddressBalances btc [paidOnce (externalAddr 1) 5000]
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
        (otherId, _) <- testNewAcc2 ctx "other"
        _ <- genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        _ <- genExtAddress ctx cfg otherId ""
        addressPath btc accId (externalAddr 0) `dbShouldBeE` Just (Deriv :/ 0 :/ 0)
        addressPath btc accId (internalAddr 0) `dbShouldBeE` Just (Deriv :/ 1 :/ 0)
        addressPath btc accId externalAddr2 `dbShouldBeE` Nothing
        addressPath btc accId (otherAddr 0) `dbShouldBeE` Nothing
        listed accId AddrExternal (Page 5 0) `dbShouldBeE` [0]

    it "keeps the account's addresses with their paths, once each" $
      inDatabaseE $ do
        accId <- account ctx
        _ <- genExtAddress ctx cfg accId ""
        _ <- nextChangeAddress ctx cfg accId []
        walletPaths btc accId [externalAddr 0, otherAddr 0, externalAddr 0, internalAddr 0]
          `dbShouldBeE` Map.fromList [(externalAddr 0, Deriv :/ 0 :/ 0), (internalAddr 0, Deriv :/ 1 :/ 0)]

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
      deriveChainAddress ctx (testAccountPub ctx) extDeriv 9 `shouldReturn` Just (externalAddr 9)
      deriveChainAddress ctx (testAccountPub ctx) intDeriv 0 `shouldReturn` Just (internalAddr 0)

    it "lists a window of a chain with the indices" $ do
      chainAddresses ctx (testAccountPub ctx) extDeriv (Page 3 2)
        `shouldReturn` [(externalAddr 2, 2), (externalAddr 3, 3), (externalAddr 4, 4)]
      chainAddresses ctx (testAccountPub ctx) intDeriv (Page 0 5) `shouldReturn` []

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

    it "prints an address with the balance it can spend among its balances, as addrs does, and reads it back" $ do
      let json :: String
          json =
            [r|{
              "account": 1,
              "index": 3,
              "derivation": "/0/3",
              "address": "1yQZuJjA6w7hXpc3C2LRiCv22rKCas7F1",
              "label": "Savings",
              "balance": {"confirmed": 5000, "unconfirmed": 300, "coins": 2, "txs": 3, "received": 9000, "spendable": 4800},
              "internal": false,
              "created": "2023-11-01T00:00:00Z"
            }|]
      eitherDecode (cs json) `shouldBe` Right (addressWithSpendable fixtureRow 4800)
      (parseEither parseAddressWithSpendable =<< eitherDecode (cs json)) `shouldBe` Right (fixtureRow, 4800)
